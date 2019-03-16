
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

type element = {
    category : Utils.Id.t t ;
    attribute : State.attribute t ;
    constructor : State.constructor gt
  }


let empty = PMap.empty
let gempty = PMap.empty

let empty_element = {
    category = empty ;
    attribute = empty ;
    constructor = gempty
  }


let add m lg o str = PMap.add (o, lg) str m

let translate m o lg =
  try Some (PMap.find (o, lg) m)
  with Not_found -> None

let force_translate m o lg =
  match translate m o lg with
  | Some r -> r
  | None ->
    match translate m o generic with
    | Some r -> "<" ^ r ^ ">"
    | None -> "<Missing translation>"


(** Given a set of tags and a tree, explores the tree following the path
 * indicated by the set.
 * It then returns the found list. **)
let rec search_tree tags = function
  | Leaf l -> l
  | Node (t, t1, t2) ->
    if Utils.PSet.mem t tags then
      search_tree tags t1
    else search_tree tags t2

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

let gtranslate m o lg tags =
  try
    let t = PMap.find (o, lg) m in
    let l = search_tree tags t in
    try Some (Utils.select_any l)
    with Utils.EmptyList -> None
  with Not_found -> None

let gforce_translate m o lg tags =
  match gtranslate m o lg tags with
  | None -> ("??", Utils.PSet.empty) (* TODO *)
  | Some r -> r

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
  | _ ->
    failwith ("The file “" ^ fileName ^ "” is not a list.")

