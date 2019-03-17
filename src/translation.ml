
type language = string

type tag = string

let generic = ""

let iso639 = Utils.id
let from_iso639 = Utils.id

let get_tag = Utils.id

type 'a t = ('a * language, string) PMap.t

(** A decision tree for translations. **)
type 'a tree =
  | Leaf of 'a list
    (** A leaf: the only possible translations at this stage.
     * There may be more than one: one is then taken randomly. **)
  | Node of tag * 'a tree * 'a tree
    (** When reaching this node, we divide the set of translations
     * in two subsets: the one where the tag is present (first subset)
     * and the one where is it not present (second subset). **)

type 'a gt = ('a * language, (string * tag Utils.PSet.t) tree) PMap.t

type 'b sitem =
  | Direct of string
  | Variable of 'b * tag Utils.PSet.t

type ('a, 'b) st = ('a * language, ('b sitem list * tag Utils.PSet.t) tree) PMap.t

type translation_function = tag Utils.PSet.t -> (string * tag Utils.PSet.t) option
type complete_translation_function = tag Utils.PSet.t -> string * tag Utils.PSet.t

type element = {
    category : Utils.Id.t t ;
    attribute : State.attribute t ;
    constructor : State.constructor gt
  }


let empty = PMap.empty
let gempty = PMap.empty
let sempty = PMap.empty

let empty_element = {
    category = empty ;
    attribute = empty ;
    constructor = gempty
  }


let add m lg o str = PMap.add (o, lg) str m

let translate m lg o =
  try Some (PMap.find (o, lg) m)
  with Not_found -> None

(** Produces a [force_] alternative to a translation function by trying
 * the [generic] language if the given one failed.
 * It also takes as an argument a return with an empty string and a way
 * to map each strings in a result. **)
let fallback_to_generic lg f d change =
  match f lg with
  | Some r -> r
  | None ->
    match f generic with
    | Some r -> change (fun str -> "<" ^ str ^ ">") r
    | None ->
        change (fun str ->
          let str = if str = "" then "" else (" (" ^ str ^ ")") in
          "<Missing translation" ^ str ^ ">") d

let force_translate m lg o =
  fallback_to_generic lg (fun lg -> translate m lg o) "" Utils.id


(** Given a set of tags and a tree, explores the tree following the path
 * indicated by the set.
 * It then returns the found list. **)
let rec search_tree tags = function
  | Leaf l -> l
  | Node (t, t1, t2) ->
    if Utils.PSet.mem t tags then
      search_tree tags t1
    else search_tree tags t2

(** Most of the time, we really want a result.
 * It is safe to return a result associated with less tags than provided.
 * This alternative function thus explores more of the set by looking at
 * negative branches when the positive did not return any interesting
 * result.
 * The opposite is however not safe in general. **)
let rec search_backtrack_tree tags = function
  | Leaf l -> l
  | Node (t, t1, t2) ->
    if Utils.PSet.mem t tags then
      let l = search_backtrack_tree tags t1 in
      if l = [] then search_backtrack_tree tags t2
      else l
    else search_backtrack_tree tags t2

(** Given a set of tags and a function, finds the corresponding list
 * and apply the function to this list.
 * This function is given the list of tags seen when traversing the tree
 * in the order they were given. **)
let apply_tree tags f =
  let rec aux seen = function
  | Leaf l -> f (List.rev seen) l
  | Node (t, t1, t2) ->
    let (t1, t2) =
      if Utils.PSet.mem t tags then
        (aux (t :: seen) t1, t2)
      else (t1, aux (t :: seen) t2) in
    Node (t, t1, t2) in
  aux []

(** Just adds a value to the list naturally given by [search_tree]. **)
let add_tree tags v =
  apply_tree tags (fun _ l -> Leaf (v :: l))

let gadd m lg tags o str tags' =
  let tags' = Utils.PSet.from_list tags' in
  let t =
    try PMap.find (o, lg) m
    with Not_found -> Leaf [] in
  PMap.add (o, lg) (apply_tree (Utils.PSet.from_list tags) (fun seen l ->
    let rec aux l = function
      | [] -> Leaf ((str, tags') :: l)
      | tag :: tags ->
        if List.mem tag seen then aux l tags
        else Node (tag, aux [] tags, Leaf l) in
    aux l tags) t) m

let sadd = gadd

let gtranslate m lg o tags =
  try
    let t = PMap.find (o, lg) m in
    let l = search_backtrack_tree tags t in
    try Some (Utils.select_any l)
    with Utils.EmptyList -> None
  with Not_found -> None

let gforce_translate m lg o tags =
  fallback_to_generic
    lg (fun lg -> gtranslate m lg o tags)
    (String.concat ", " (Utils.PSet.to_list tags), Utils.PSet.empty)
    (fun f (str, s) -> (f str, s))

let stranslate m lg trv o tags =
  try
    let t = PMap.find (o, lg) m in
    let l = search_backtrack_tree tags t in
    try
      let (l, tags) = Utils.select_any l in
      Option.map (fun l -> (String.concat " " l, tags))
        (Utils.list_map_option (function
          | Direct str -> Some str
          | Variable (x, tags) ->
            Option.map fst (trv x tags)
            (* FIXME: We are ignoring the returned tags here.
             * This is wrong. *)) l)
    with Utils.EmptyList -> None
  with Not_found -> None

let sforce_translate m lg trv o tags =
  fallback_to_generic
    lg (fun lg -> stranslate m lg (fun o tags -> Some (trv o tags)) o tags)
    (String.concat ", " (Utils.PSet.to_list tags), Utils.PSet.empty)
    (fun f (str, s) -> (f str, s))


let from_json fileName fileContent =
  match Yojson.Safe.from_string ~fname:fileName fileContent with
  | `List l ->
    Utils.list_fold_lefti (fun i (t, lgs) ->
        let current =
          "The " ^ string_of_int (1 + i) ^ "th element"
          ^ " of the file “" ^ fileName ^ "”" in function
        | `Assoc l ->
          let errorKey key =
            failwith (current ^ " associates the field “" ^ key
                      ^ "” to something else than a string.") in
          let lg =
            try match List.assoc "iso639" l with
                | `String lg -> lg
                | _ -> errorKey "iso639"
            with Not_found ->
              failwith (current ^ " has no key “iso639”.") in
          (List.fold_left (fun t -> function
            | key, `String str -> add t lg key str
            | (key, _) -> errorKey key) t l, lg :: lgs)
        | _ ->
          failwith (current ^ " is not an object.")) (empty, []) l
  | _ -> failwith ("The file “" ^ fileName ^ "” is not a list.")

