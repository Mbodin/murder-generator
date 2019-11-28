open ExtList

(** A specification of the language sounds.
 * This type works like an automaton, the parameterised type being the state system. **)
type 'a automaton = {
    init : unit -> 'a (** The initial state. **) ;
    transition : 'a -> string * 'a option
      (** A transition.
       * Returning [None] means that the transition halted. **)
  }

(** All the possible data stored in a name file. **)
type data =
  | Automaton : 'a automaton -> data
  | List of string list

type t = {
    translate : unit Translation.t ;
    data : data ;
    default : Translation.language PSet.t
  }

let translate g = g.translate

let is_default g lg = PSet.mem lg g.default

(** A more furnished automaton. **)
type 'a alternative = {
    alternative_size : int ; (** Expected size of the resulting words. **)
    alternative_init : (int * ('a * string)) list
      (** Wheighed list of initial states. **) ;
    alternative_transition : 'a -> (int * ('a * string)) list
      (** Wheighed list of transitions. **) ;
    alternative_final : 'a -> (int * string) list
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
    ) else decide () in {
    init = (fun _ ->
      let (s, str) = Utils.select spec.alternative_init in
      (spec.alternative_size, Some s, str)) ;
    transition = fun (size, s, str) ->
      match s with
      | None -> (str, None)
      | Some s ->
        (str,
          if halt size then
            Some (0, None, Utils.select (spec.alternative_final s))
          else
            let (s, str) = Utils.select (spec.alternative_transition s) in
            Some (size - 1, Some s, str))
  }

(** A simple type to represent in a transition system the alternance of vowels and consonant. **)
type vowelConsonant = int * bool option * string

(** Create a transition system for vowels and consonants.
 * It takes as an argument six string specifying how the language sounds, as well
 * as the expected size of the output (in term of the given vowels and consonants).
 * Each string is a list of list separated by [,] for the inner lists and [;] for
 * the outer.
 * The inner lists (separated by [,]) commutes, whilst the outer lists represent
 * changes in probability (the first elements being more probable).
 * The six lists corresponds to:
 * - initial vowels and consonants;
 * - middle vowels and consonants;
 * - end vowels and consonants. **)
let createVowelConsonant size initV initC middleV middleC endV endC =
  let get_spec f strspec =
    let rec aux = function
      | [] -> (1, [])
      | l :: ls ->
        let (weight, spec) = aux ls in
        let spec = List.map (fun str -> (weight, (f str))) l @ spec in
        (3 * weight, spec) in
    snd (aux (List.map (String.split_on_char ',')
      (String.split_on_char ';' strspec))) in
  let add b e = (b, e) in
  Automaton (convertAlternative {
      alternative_size = size ;
      alternative_init = get_spec (add false) initV @ get_spec (add true) initC ;
      alternative_transition = 
        (let middleV = get_spec (add false) middleV in
         let middleC = get_spec (add true) middleC in
         fun b -> if b then middleV else middleC) ;
      alternative_final = 
        let endV = get_spec Utils.id endV in
        let endC = get_spec Utils.id endC in
        fun b -> if b then endV else endC
    })


let generateAutomaton data =
  let rec aux str s =
    let (suffix, next) = data.transition s in
    let str = str ^ suffix in
    match next with
    | None -> str
    | Some s -> aux str s in
  aux "" (data.init ())

let generate g =
  match g.data with
  | Automaton data -> generateAutomaton data
  | List l -> Utils.select_any l

let empty = {
    translate = Translation.empty ;
    data = List [""] ;
    default = PSet.empty
  }

let import file =
  let file = List.enum (String.split_on_char '\n' file) in
  let file = Enum.concat (Enum.map (fun str -> Utils.enum_split_on_char '\r' str) file) in
  let file = Enum.filter ((<>) "") file in
  let file = Enum.filter (fun line -> line.[0] <> '#') file in
  Enum.force file ;
  let split_at i line =
    let key = String.sub line 0 i in
    let value = String.sub line (1 + i) (String.length line - i - 1) in
    (key, value) in
  let split line =
    Option.map (fun i -> split_at i line) (String.index_opt line ':') in
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
    | "alternate" ->
      let assoc =
        match Utils.list_map_option split (List.of_enum file) with
        | None -> invalid_arg "Invalid name file of kind “alternate”."
        | Some assoc -> assoc in
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
      createVowelConsonant size startV startC middleV middleC endV endC
    | "list" -> List (List.of_enum file)
    | "attributeList" ->
      List ["TODO"] (* TODO *)
    | k -> invalid_arg ("Invalid name file: invalid kind “" ^ k ^ "”.") in
  let rec aux tr default =
    get_key_value (fun _ -> invalid_arg "Invalid name file: no kind given.")
      (fun lg name ->
        match lg with
        | "" ->
          let data = get_data name in {
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

