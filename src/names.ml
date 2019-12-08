open ExtList

type property = Attribute.PlayerAttribute.constructor

(** A specification of the language sounds.
 * This type works like an automaton, the parameterised type being the state system.
 * Each step can fire properties: the final property has to be the whole set of properties. **)
type 'a automaton = {
    init : property PSet.t -> 'a * property list (** The initial state. **) ;
    transition : 'a -> string * ('a * property list) option
      (** A transition.
       * It takes as argument the current set of properties.
       * It returns the next step and the set of properties added to the character.
       * Returning [None] means that the transition halted. **)
  }

(** All the possible data stored in a name file. **)
type data =
  | Automaton : 'a automaton -> data
  | AttrList of (string * property list) list

type t = {
    property_infos : Attribute.PlayerAttribute.constructor_map ;
    translate : unit Translation.t ;
    data : data ;
    default : Translation.language PSet.t
  }

let translate g = g.translate

let is_default g lg = PSet.mem lg g.default

(** A more furnished automaton. **)
type 'a alternative = {
    alternative_size : int ; (** Expected size of the resulting words. **)
    alternative_init : property PSet.t -> (int * ('a * string * property list)) list
      (** Wheighed list of initial states. **) ;
    alternative_transition : property PSet.t -> 'a -> (int * ('a * string * property list)) list
      (** Wheighed list of transitions. **) ;
    alternative_final : property PSet.t -> 'a -> (int * (string * property list)) list
      (** Wheighed list of final states. **)
  }

let convertAlternative spec =
  (** Decides whether it is time to halt the generation. **)
  let halt size =
    (** The basic decide function is simple, but we may want to alter
     * its distribution.**)
    let decide () =
      size <= 0 || Random.int size = 0 in
    if size > spec.alternative_size * 3 / 2 then
      decide () && decide ()
    else if size < spec.alternative_size / 2 then (
      if size <= 0 then
        Random.int 4 <> 0
      else if size < spec.alternative_size / 4 then
        decide () || decide ()
      else decide () || Random.int 5 = 0
    ) else decide () in
  let empty props = ((0, None, "", props), []) in {
    init = (fun props ->
      let l = spec.alternative_init props in
      if l = [] then empty props
      else
        let (s, str, props') = Utils.select l in
        ((spec.alternative_size, Some s, str, props), props')) ;
    transition = fun (size, s, str, props) ->
      (let add_props =
         List.fold_left (fun props c -> PSet.add c props) props in
       match s with
       | None -> (str, None)
       | Some s ->
         (str,
           let final = spec.alternative_final props s in
           let normal = spec.alternative_transition props s in
           if (halt size && final <> []) || normal = [] then
             if final = [] then Some (empty props)
             else
               let (str, props') = Utils.select final in
               Some ((0, None, str, add_props props'), props')
           else
             let (s, str, props') = Utils.select normal in
             Some ((size - 1, Some s, str, add_props props'), props')))
  }

(** A simple type to represent in a transition system the alternance of vowels and consonant. **)
type vowelConsonant = int * bool option * string

(** Given a line, split it betwween a key and a value at the first occurrence of [:].
 * Returns [None] if no [:] is present in the list. **)
let split line =
  let split_at i line =
    let key = String.sub line 0 i in
    let value = String.sub line (1 + i) (String.length line - i - 1) in
    (key, value) in
  Option.map (fun i -> split_at i line) (String.index_opt line ':')

(** State whether a return value of [generate] is compatible with a preset set of properties. **)
let compatible_with m properties =
  let get_attribute c =
    Utils.assert_option __LOC__
      (Attribute.PlayerAttribute.constructor_attribute m c) in
  let attributes =
    PSet.map get_attribute properties in
  assert (PSet.length attributes = PSet.length properties) ;
  let ok (_, l) =
    List.for_all (fun c ->
      PSet.mem c properties || not (PSet.mem (get_attribute c) attributes)) l in
  ok

(** Try [n] times to call [f] and to get a result compatible with [ok], then call [fallback]
 * if none is found. **)
let rec try_n n ok fallback f =
  if n = 0 then
    fallback ()
  else
    let v = f () in
    if ok v then v
    else try_n (n - 1) ok fallback f

(** Create a transition system for vowels and consonants.
 * It takes as an argument six string specifying how the language sounds, as well
 * as the expected size of the output (in term of the given vowels and consonants).
 * It also takes as argument a associative list of definitions, associating each case
 * to a list of associated constructors.
 * Each string is a list of list separated by [,] for the inner lists and [;] for
 * the outer.
 * The inner lists (separated by [,]) commutes, whilst the outer lists represent
 * changes in probability (the first elements being more probable).
 * The six lists corresponds to:
 * - initial vowels and consonants;
 * - middle vowels and consonants;
 * - end vowels and consonants.
 * Each of the list element can be annotated by an additionnal [:] to associate it
 * with the associated constructors defined in [definitions]. **)
let createVowelConsonant m definitions size initV initC middleV middleC endV endC =
  let get_spec f unf strspec props =
    let rec aux = function
      | [] -> (1, [])
      | l :: ls ->
        let (weight, spec) = aux ls in
        let l =
          List.map (fun str ->
            let (str, props) =
              match split str with
              | None -> (str, [])
              | Some (str, case) ->
                match List.assoc_opt case definitions with
                | None -> invalid_arg ("Invalid name file of kind “alternate”: the case “"
                                       ^ case ^ "” is not defined.")
                | Some props -> (str, props) in
            (weight, f str props)) l in
        let spec =
          List.filter (fun (_, v) -> compatible_with m props (unf v)) l
          @ spec in
        (3 * weight, spec) in
    match strspec with
    | "" -> []
    | "," | ";" -> [(1, f "" [])]
    | _ ->
      snd (aux (List.map (String.split_on_char ',')
        (String.split_on_char ';' strspec))) in
  let add b e props = (b, e, props) in
  let unadd (b, e, props) = ((b, e), props) in
  Automaton (convertAlternative {
      alternative_size = size ;
      alternative_init =
        (fun props ->
          get_spec (add false) unadd initV props
          @ get_spec (add true) unadd initC props) ;
      alternative_transition = 
        (let middleV = get_spec (add false) unadd middleV in
         let middleC = get_spec (add true) unadd middleC in
         fun props b -> (if b then middleV else middleC) props) ;
      alternative_final = 
        (let endV = get_spec (fun str props -> (str, props)) Utils.id endV in
         let endC = get_spec (fun str props -> (str, props)) Utils.id endC in
         fun props b -> (if b then endV else endC) props)
    })


let generateAutomaton data properties =
  let rec aux str s props =
    let (suffix, next) = data.transition s in
    let str = str ^ suffix in
    match next with
    | None -> (str, props)
    | Some (s, props') -> aux str s (props' @ props) in
  let (s, props) = data.init properties in
  aux "" s props

let generateList m l properties =
  let ok = compatible_with m properties in
  let fallback _ =
    let l = List.filter ok l in
    if l = [] then ("", [])
    else Utils.select_any l in
  try_n 5 ok fallback (fun _ -> Utils.select_any l)

let generate g =
  match g.data with
  | Automaton data -> generateAutomaton data
  | AttrList l -> generateList g.property_infos l

let empty = {
    property_infos = Attribute.PlayerAttribute.empty_constructor_map ;
    translate = Translation.empty ;
    data = AttrList [("", [])] ;
    default = PSet.empty
  }

let import_alternate m file =
  let assoc =
    match Utils.list_map_option split file with
    | None -> invalid_arg "Invalid name file of kind “alternate”."
    | Some assoc -> assoc in
  let (definitions, assoc) =
    let rec aux defs assoc = function
      | [] -> (defs, assoc)
      | ("define", def) :: l ->
        let (constructors, l) =
          let rec aux constructors = function
            | [] -> invalid_arg "Invalid definition in name file of kind “alternate”."
            | ("", "end") :: l -> (constructors, l)
            | (att, c) :: l ->
              let a =
                match Attribute.PlayerAttribute.get_attribute m att with
                | None -> invalid_arg ("Invalid attribute “" ^ att ^ "” in name file.")
                | Some a -> a in
              let c =
                match Attribute.PlayerAttribute.get_constructor m a c with
                | None ->
                  invalid_arg ("Invalid constructor “" ^ c ^ "” for attribute “" ^ att
                               ^ "” in name file.")
                | Some c -> c in
              aux (c :: constructors) l in
          aux [] l in
        aux ((def, constructors) :: defs) assoc l
      | (key, value) :: l -> aux defs ((key, value) :: assoc) l in
    aux [] [] assoc in
  let get key =
    match List.assoc_opt key assoc with
    | None ->
      invalid_arg ("Invalid name file: missing key “" ^ key
                   ^ "” for kind “alternate”.")
    | Some v -> v in
  let startV = get "startVowels" in
  let startC = get "startConsonant" in
  let middleV = get "middleVowels" in
  let middleC = get "middleConsonant" in
  let endV = get "endVowels" in
  let endC = get "endConsonant" in
  let size =
    let raw = get "size" in
    try int_of_string raw
    with _ ->
      invalid_arg ("Invalid name file: invalid value for key “size”"
                   ^ " for kind “alternate”: “" ^ raw ^ "”.") in
  createVowelConsonant m definitions size startV startC middleV middleC endV endC

let import_attribute_list m file =
  let expected e g =
    invalid_arg ("Invalid name file of kind “attributeList”: expected “" ^ e
                 ^ "” but got “" ^ g ^ "”.") in
  let get_pair expect file =
    match Enum.get file with
    | None -> expected expect "EOF"
    | Some line ->
      match split line with
      | None -> expected expect line
      | Some (key, value) -> (key, value) in
  let rec parse_attribute_list acc props =
    let (key, value) = get_pair ":list" file in
    if key = "" && value = "list" then
      parse_list acc props
    else
      let c =
        let a =
          match Attribute.PlayerAttribute.get_attribute m key with
          | None -> invalid_arg ("Invalid attribute “" ^ key ^ "” in name file.")
          | Some a -> a in
        match Attribute.PlayerAttribute.get_constructor m a value with
        | None ->
          invalid_arg ("Invalid constructor “" ^ value ^ "” for attribute “" ^ key
                       ^ "” in name file.")
        | Some c -> c in
      parse_attribute_list acc (c :: props)
  and parse_list acc props =
    match Enum.get file with
    | None -> acc
    | Some name ->
      if name.[0] = ':' then (
        if name = ":attributes" then
          parse_attribute_list acc []
        else expected ":attributes" name
      ) else parse_list ((name, props) :: acc) props in
  let l =
    if Enum.is_empty file then []
    else
      let (key, value) = get_pair ":attributes" file in
      if key = "" && value = "attributes" then
        parse_attribute_list [] []
      else expected ":attributes" (key ^ ":" ^ value) in
  AttrList l

let import m file =
  let file = List.enum (String.split_on_char '\n' file) in
  let file = Enum.concat (Enum.map (fun str -> Utils.enum_split_on_char '\r' str) file) in
  let file = Enum.filter ((<>) "") file in
  let file = Enum.filter (fun line -> line.[0] <> '#') file in
  Enum.force file ;
  let get_key_value none cont =
    match Enum.peek file with
    | None -> none ()
    | Some line ->
      match split line with
      | None -> invalid_arg "Invalid name file: expected key value."
      | Some (key, value) ->
        Enum.junk file ;
        cont key value in
  let get_data = function
    | "alternate" -> import_alternate m (List.of_enum file)
    | "list" -> AttrList (List.of_enum (Enum.map (fun n -> (n, [])) file))
    | "attributeList" -> import_attribute_list m file
    | k -> invalid_arg ("Invalid name file: invalid kind “" ^ k ^ "”.") in
  let rec aux tr default =
    get_key_value (fun _ -> invalid_arg "Invalid name file: no kind given.")
      (fun lg name ->
        match lg with
        | "" ->
          let data = get_data name in {
            property_infos = m ;
            translate = tr ;
            data = data ;
            default = default
          }
        | "default" ->
          let elements =
            if name = "" then
              PSet.empty
            else
              let l = String.split_on_char ',' name in
              PSet.from_list (List.map Translation.from_iso639 l) in
          aux tr (PSet.merge elements default)
        | _ ->
          let lg = Translation.from_iso639 lg in
          aux (Translation.add tr lg () name) default) in
  aux Translation.empty PSet.empty

