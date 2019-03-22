
type state = {
    names : string list ;
    language : Translation.language ;
    translation : Translation.element ;
    state : State.t
  }

let generic s = { s with language = Translation.generic }

let translate_attribute s a =
  let tr = s.translation.Translation.attribute in
  Translation.force_translate tr s.language a

let translate_value s f v =
  let tr l =
    let tr = s.translation.Translation.constructor in
    let v = Utils.select_any l in
    let v = f v in
    fst (Translation.gforce_translate tr s.language v Utils.PSet.empty) in
  match v with
  | State.Fixed_value (l, strict) -> tr l
  | State.One_value_of l -> tr l

(** Some special constructors provides negative information
 * (that is, that something is absent instead of present).
 * This function uses heuristics to determine whether a constructor
 * represents negative information.
 * Some exportation functions will display these differently, but it
 * should never profundly change the behaviour of the exportation. **)
let negative s v =
  let trv = translate_value (generic s) (fun v -> State.ContactConstructor v) v in
  List.mem trv ["False"; "None"; "Absent"; "Undefined"]

let to_graphviz s =
  let player_node c = "player" ^ string_of_int (Utils.Id.to_array c) in
  let cst = State.get_character_state s.state in
  let get_color s =
    let n = float_of_int (Hashtbl.hash s mod 1000) /. 1000. in
    string_of_float n ^ " 1 0.7" in
  String.concat "\n" [
      "digraph {" ; "" ;
      "  // Generated from https://github.com/Mbodin/murder-generator" ; "" ;
      "  node [shape=record]" ; "" ;
      (** Declaring each player **)
      String.concat "\n" (List.mapi (fun c name ->
        let c = Utils.Id.from_array c in
        "  " ^ player_node c ^ " [label=\"{"
        ^ name ^ "|"
        ^ String.concat "|" (PMap.foldi (fun a v l ->
              let tra = translate_attribute s (State.PlayerAttribute a) in
              let trv = translate_value s (fun v -> State.PlayerConstructor v) v in
              ("{" ^ tra ^ "|" ^ trv ^ "}") :: l)
            (State.get_all_attributes_character cst c) [])
        ^ "}\"]") s.names) ; "" ;
      (** Declaring their relations **)
      String.concat "\n" (List.concat (List.mapi (fun c _ ->
        let c = Utils.Id.from_array c in
        PMap.foldi (fun c' lv l ->
          List.map (fun (a, v) ->
            let tra = translate_attribute s (State.ContactAttribute a) in
            let trv = translate_value s (fun v -> State.ContactConstructor v) v in
            let color = if negative s v then "transparent" else get_color tra in
            "  " ^ player_node c ^ " -> " ^ player_node c'
            ^ " [label=\"" ^ tra ^ ":" ^ trv ^ "\""
            ^ " color=\"" ^ color ^ "\"] ;") lv @ l)
        (State.get_all_contacts_character cst c) []) s.names)) ;
      "}" ; ""
    ]

let to_json s =
  let s = generic s in
  let cst = State.get_character_state s.state in
  Yojson.Safe.to_string ~std:true (`List (List.mapi (fun c name ->
    let c = Utils.Id.from_array c in `Assoc [
        ("name", `String name) ;
        ("attributes", `Assoc (PMap.foldi (fun a v l ->
             let tra = translate_attribute s (State.PlayerAttribute a) in
             let trv = translate_value s (fun v -> State.PlayerConstructor v) v in
             (tra, `String trv) :: l)
           (State.get_all_attributes_character cst c) [])) ;
        ("contacts", `List (List.mapi (fun c' _ ->
          let c' = Utils.Id.from_array c' in
          `Assoc (List.map (fun (a, v) ->
              let tra = translate_attribute s (State.ContactAttribute a) in
              let trv = translate_value s (fun v -> State.ContactConstructor v) v in
              (tra, `String trv))
            (State.get_all_contact_character cst c c'))) s.names)) ;
        ("relations", `List (Utils.list_fold_lefti (fun c' l _ ->
          let c' = Utils.Id.from_array c' in
          let r = State.read_relation s.state c c' in
          if c <= c' then l
          else `String (Relation.to_string r) :: l) [] s.names)) ;
      ]) s.names))

let from_json m fileName fileContent =
  match Yojson.Safe.from_string ~fname:fileName fileContent with
  | `List l ->
    let state = State.create_state (List.length l) in
    let (names, state) =
      Utils.list_fold_lefti (fun c (names, state) ->
        let c = Utils.Id.from_array c in function
        | `Assoc l ->
          let name =
            try
              match List.assoc "name" l with
              | `String name -> name
              | _ -> failwith ("A field `name' in file `" ^ fileName
                               ^ "' is not a string.")
            with Not_found ->
              failwith ("Missing field `name' in file `" ^ fileName ^ "'.") in
          let get_field_list fld =
            try
              match List.assoc fld l with
              | `List l -> l
              | _ -> failwith ("A field `" ^ fld ^ "' in file `" ^ fileName
                               ^ "' is not a list")
            with
            | Not_found -> [] in
          let get_field_assoc fld =
            try
              match List.assoc fld l with
              | `Assoc l -> l
              | _ -> failwith ("A field `" ^ fld ^ "' in file `" ^ fileName
                               ^ "' is not an object")
            with
            | Not_found -> [] in
          let get_attribute en get m attribute =
            match get m attribute with
            | Some attribute -> attribute
            | None ->
              failwith ("Unknown " ^ en ^ " `" ^ attribute ^ "' in file `"
                        ^ fileName ^ "'.") in
          let get_constructor en get m attribute v =
            match get m attribute v with
            | Some v -> v
            | None ->
              failwith ("Unknown " ^ en ^ " constructor `" ^ v ^ "' in file `"
                        ^ fileName ^ "'.") in
          let state =
            let attributes = get_field_assoc "attributes" in
            List.fold_left (fun state -> function
              | (attribute, `String v) ->
                let attribute =
                  get_attribute "attribute" State.PlayerAttribute.get_attribute
                    m.State.player attribute in
                let v =
                  get_constructor "attribute" State.PlayerAttribute.get_constructor
                    m.State.player attribute v in
                State.write_attribute_character (State.get_character_state state) c
                  attribute (Fixed_value ([v], Strict)) ;
                state
              | (field, _) ->
                failwith ("Field `" ^ field ^ "' is file `" ^ fileName
                          ^ "' is supposed to be an attribute and thus associated"
                          ^ " to a string, which it is not.")) state attributes in
          let state =
            let contacts = get_field_list "contacts" in
            Utils.list_fold_lefti (fun c' state ->
              let c' = Utils.Id.from_array c' in function
              | `Assoc l ->
                List.fold_left (fun state -> function
                  | (attribute, `String v) ->
                    let attribute =
                      get_attribute "contact" State.ContactAttribute.get_attribute
                        m.State.contact attribute in
                    let v =
                      get_constructor "contact"
                        State.ContactAttribute.get_constructor
                        m.State.contact attribute v in
                    State.write_contact_character (State.get_character_state state)
                      c attribute c' (Fixed_value ([v], Strict)) ;
                    state
                  | (field, _) ->
                    failwith ("Field `" ^ field ^ "' is file `" ^ fileName
                              ^ "' is supposed to be a contact and thus associated"
                              ^ " to a string, which it is not.")) state l
              | _ -> failwith ("A contact in file `" ^ fileName
                               ^ "' is not associated an object.")) state contacts in
          let state =
            let relations = get_field_list "relations" in
            Utils.list_fold_lefti (fun c' state ->
              let c' = Utils.Id.from_array c' in function
              | `String r ->
                let r = Driver.parse_relation r in
                if c <> c' then State.write_relation state c c' r ;
                state
              | _ -> failwith ("Ill-formed `relations' field in file `"
                               ^ fileName ^ "'.")) state relations in
          (name :: names, state)
        | _ -> failwith ("A character in file `" ^ fileName
                 ^ "' is not a associated an object.")) ([], state) l in
    (List.rev names, state)
  | _ -> failwith ("The file `" ^ fileName ^ "' is not a list.")

let all_production = [
    ("graphviz", "graphvizDescription", "text/vnd.graphviz", "dot", to_graphviz) ;
    ("json", "jsonDescription", "application/json", "json", to_json)
  ]

