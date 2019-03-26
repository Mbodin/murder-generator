
type state = {
    names : string list ;
    language : Translation.language ;
    translation : Translation.element ;
    generic_translation : string Translation.t ;
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
    fst (Translation.gforce_translate tr s.language v PSet.empty) in
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
  let player_node c = "player" ^ string_of_int (Id.to_array c) in
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
        let c = Id.from_array c in
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
        let c = Id.from_array c in
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

let to_icalendar s =
  let events =
    let id_postfix =
      "-" ^ string_of_float (Sys.time ())
      ^ "-" ^ History.rfc2445 History.now
      ^ "-" ^ string_of_int (Hashtbl.hash s.names)
      ^ "-murder-generator" in
    List.concat (List.map (fun e ->
      "BEGIN:VEVENT"
      :: ("UID:" ^ string_of_int (Random.int max_int)
                 ^ "-" ^ string_of_int (Hashtbl.hash e) ^ id_postfix)
      :: ("DTSTAMP:" ^ History.rfc2445 History.now)
      :: ("DTSTART:" ^ History.rfc2445 e.History.event_begin)
      :: ("DTEND:" ^ History.rfc2445 e.History.event_end)
      :: List.map (fun c ->
           "ATTENDEE:" ^ List.nth s.names (Id.to_array c)) e.History.event_attendees
      @ "DESCRIPTION:" (* TODO *)
      :: "END:VEVENT"
      :: []) [(*TODO*)]) in
  String.concat "\n" (
    "BEGIN:VCALENDAR"
    :: "VERSION:2.0"
    :: ("PRODID:-//Martin Constantino-Bodin//Murder Generator//"
        ^ String.uppercase_ascii (Translation.iso639 s.language))
    :: events
    @ "END:VCALENDAR"
    :: [])

let to_org s =
  let cst = State.get_character_state s.state in
  let get_translation key =
    Utils.assert_option ("No key `" ^ key ^ "' found for language `"
                         ^ (Translation.iso639 s.language) ^ "' at "
                         ^ __LOC__ ^ ".")
      (Translation.translate s.generic_translation s.language key) in
  let print_event n e =
    String.concat "\n" (
      (String.make n '*' ^ " " (* TODO: Description *))
      :: (String.make (2 + n) ' '
          ^ History.orgmode_range e.History.event_begin e.History.event_end)
      :: List.map (fun c ->
           String.make (1 + n) ' ' ^ "- [X] "
           ^ List.nth s.names (Id.to_array c)) e.History.event_attendees) in
  String.concat "\n\n" (
    String.concat "\n" (
      ("* " ^ get_translation "forTheGM")
      :: List.map (print_event 3) [(* TODO: All events *)])
    :: List.mapi (fun c name ->
         let c = Id.from_array c in
         String.concat "\n" (
           ("* " ^ name)
           :: ("** " ^ get_translation "characterAttributes")
           :: PMap.foldi (fun a v l ->
                  let tra = translate_attribute s (State.PlayerAttribute a) in
                  let trv =
                    translate_value s (fun v -> State.PlayerConstructor v) v in
                  (String.make 4 ' ' ^ "- " ^ tra ^ ": " ^ trv) :: l)
                (State.get_all_attributes_character cst c) []
           @ ("** " ^ get_translation "characterEvents")
           :: List.map (print_event 3) [(* TODO: Events *)])) s.names)

let to_json s =
  let s = generic s in
  let cst = State.get_character_state s.state in
  let rst = State.get_relation_state s.state in
  Yojson.Safe.to_string ~std:true (`List (List.mapi (fun c name ->
    let c = Id.from_array c in `Assoc [
        ("name", `String name) ;
        ("attributes", `Assoc (PMap.foldi (fun a v l ->
             let tra = translate_attribute s (State.PlayerAttribute a) in
             let trv = translate_value s (fun v -> State.PlayerConstructor v) v in
             (tra, `String trv) :: l)
           (State.get_all_attributes_character cst c) [])) ;
        ("contacts", `List (List.mapi (fun c' _ ->
          let c' = Id.from_array c' in
          `Assoc (List.map (fun (a, v) ->
              let tra = translate_attribute s (State.ContactAttribute a) in
              let trv = translate_value s (fun v -> State.ContactConstructor v) v in
              (tra, `String trv))
            (State.get_all_contact_character cst c c'))) s.names)) ;
        ("relations", `List (Utils.list_fold_lefti (fun c' l _ ->
          let c' = Id.from_array c' in
          let r = State.read_relation s.state c c' in
          if c <= c' then l
          else `String (Relation.to_string r) :: l) [] s.names)) ;
        ("complexity", `Int (State.character_complexity rst c)) ;
        ("difficulty", `Int (State.character_difficulty rst c)) ;
        ("events", `List [(*TODO*)])
      ]) s.names))

let from_json m fileName fileContent =
  match Yojson.Safe.from_string ~fname:fileName fileContent with
  | `List l ->
    let state = State.create_state (List.length l) in
    let (names, state) =
      Utils.list_fold_lefti (fun c (names, state) ->
        let c = Id.from_array c in function
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
              let c' = Id.from_array c' in function
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
              let c' = Id.from_array c' in function
              | `String r ->
                let r = Driver.parse_relation r in
                if c <> c' then State.write_relation state c c' r ;
                state
              | _ -> failwith ("Ill-formed `relations' field in file `"
                               ^ fileName ^ "'.")) state relations in
          let state =
            try
              match List.assoc "complexity" l with
              | `Int v ->
                let rst = State.get_relation_state state in
                State.set_complexity rst c v ;
                state
              | _ -> failwith ("A field `complexity' in file `" ^ fileName
                               ^ "' is not an integer")
            with
            | Not_found -> state in
          let state =
            try
              match List.assoc "difficulty" l with
              | `Int v ->
                let rst = State.get_relation_state state in
                State.set_difficulty rst c v ;
                state
              | _ -> failwith ("A field `difficulty' in file `" ^ fileName
                               ^ "' is not an integer")
            with
            | Not_found -> state in
          (* TODO: Deal with events. *)
          (name :: names, state)
        | _ -> failwith ("A character in file `" ^ fileName
                 ^ "' is not a associated an object.")) ([], state) l in
    (List.rev names, state)
  | _ -> failwith ("The file `" ^ fileName ^ "' is not a list.")

let all_production = [
    ("json", "jsonDescription", "application/json", "json", to_json) ;
    ("orgmode", "orgDescription", "text/x-org", "org", to_org) ;
    ("graphviz", "graphvizDescription", "text/vnd.graphviz", "dot", to_graphviz) ;
    ("iCalendar", "icalDescription", "text/calendar", "ics", to_icalendar)
  ]

