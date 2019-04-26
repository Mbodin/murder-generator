
let webpage_address_base = "github.com/Mbodin/murder-generator"
let webpage_address = "https://" ^ webpage_address_base

type state = {
    names : string list ;
    language : Translation.language ;
    translation : Translation.element ;
    generic_translation : string Translation.t ;
    state : State.final
  }

(** As the state [s], but there the translation language has moved to
 * the “generic” language. **)
let generic s = { s with language = Translation.generic }

(** Provide a human-readable version of the attribute. **)
let translate_attribute s a =
  let tr = s.translation.Translation.attribute in
  Translation.force_translate tr s.language a

(** The following two functions instantiate [translate_attribute]
 * to player and contact attributes. **)
let translate_attribute_player s a =
  translate_attribute s (Attribute.PlayerAttribute a)
let translate_attribute_contact s a =
  translate_attribute s (Attribute.ContactAttribute a)

(** Provide a human-readable version of the constructor.
 * The function [f] is either [State.PlayerConstructor] or
 * [State.ContactConstructor]. **)
let translate_value s f v =
  let tr = s.translation.Translation.constructor in
  fst (Translation.gforce_translate tr s.language (f v) PSet.empty)

(** The following two functions instantiate [translate_value]
 * to player and contact attributes. **)
let translate_value_player s =
  translate_value s (fun v -> Attribute.PlayerConstructor v)
let translate_value_contact s =
  translate_value s (fun v -> Attribute.ContactConstructor v)

(** Provides a recoverable identifier for an event. **)
let translate_event_generic s e =
  let (nb_sentence, tr) = e.Events.translation in
  let tr = Translation.sforce_translate tr Translation.generic in
  fst (tr (fun _ -> PSet.empty) (fun _ _ -> None) (-1) PSet.empty)

(** Translates an event from a translation object [trp] for player. **)
let translate_event s trp e =
  let (nb_sentence, tr) = e.Events.translation in
  let tr =
    Translation.sforce_translate ~debug:(fun i ->
        Some (string_of_int i ^ " of " ^ translate_event_generic s e))
      tr s.language in
  let l =
    List.map (fun i ->
      fst (tr (fst trp) (snd trp) i PSet.empty)) (Utils.seq nb_sentence) in
  if String.concat "" l = "" then
    ["<Empty translation for " ^ translate_event_generic s e ^ ">"]
  else l

(** Generate a translation object for player for [translate_event]. **)
(* TODO: This function is actually a dummy one, waiting for a better one to come. *)
let translation_players s =
  let tags = PMap.empty in
  let read_tags p =
    try PMap.find p tags
    with Not_found -> PSet.empty in
  let names = Array.of_list s.names in
  (read_tags, fun c _ -> Some (names.(Id.to_array c), PSet.empty))

(** Return the name of a character [c]. **)
let get_name s c =
  List.nth s.names (Id.to_array c)

(** Some special constructors provides negative information
 * (that is, that something is absent instead of present).
 * This function uses heuristics to determine whether a constructor
 * represents negative information.
 * Some exportation functions will display these differently, but it
 * should never profundly change the behaviour of the exportation. **)
let negative s v =
  let trv = translate_value_contact (generic s) v in
  List.mem trv ["False"; "None"; "Absent"; "Undefined"]

(** Replace the characters '"' by '\"' and '\' by '\\'. **)
let escape_quote str =
  let replace input = Str.global_replace (Str.regexp_string input) in
  replace "\"" "\\\"" (replace "\\" "\\\\" str)

let to_graphviz s =
  let player_node c = "player" ^ string_of_int (Id.to_array c) in
  let get_color s =
    let n = float_of_int (Hashtbl.hash s mod 1000) /. 1000. in
    string_of_float n ^ " 1 0.7" in
  String.concat "\n" [
      "digraph {" ; "" ;
      "  // Generated from " ^ webpage_address ; "" ;
      "  node [shape=record]" ; "" ;
      (** Declaring each player **)
      String.concat "\n" (List.mapi (fun c name ->
        let c = Id.from_array c in
        "  " ^ player_node c ^ " [label=\"{"
        ^ escape_quote name ^ "|"
        ^ String.concat "|" (PMap.foldi (fun a v l ->
              let tra = translate_attribute_player s a in
              let trv = translate_value_player s v in
              ("{" ^ escape_quote tra ^ "|" ^ escape_quote trv ^ "}") :: l)
            (State.get_all_attributes_character_final s.state c) [])
        ^ "}\"]") s.names) ; "" ;
      (** Declaring their relations **)
      String.concat "\n" (List.concat (List.mapi (fun c _ ->
        let c = Id.from_array c in
        PMap.foldi (fun c' lv l ->
            List.map (fun (a, v) ->
              let tra = translate_attribute_contact s a in
              let trv = translate_value_contact s v in
              let color = if negative s v then "transparent" else get_color tra in
              "  " ^ player_node c ^ " -> " ^ player_node c'
              ^ " [label=\"" ^ escape_quote tra ^ ":" ^ escape_quote trv ^ "\""
              ^ " color=\"" ^ color ^ "\"] ;") lv @ l)
          (State.get_all_contacts_character_final s.state c) []) s.names)) ;
      "}" ; ""
    ]

let to_icalendar s =
  let rec split = function
    | [] -> []
    | str :: l ->
      if String.length str < 75 then
        str :: split l
      else
        String.sub str 0 74
        :: split ((" " ^ String.sub str 74 (String.length str - 74)) :: l) in
  let events =
    let tr_players = translation_players s in
    let id_postfix =
      "-" ^ string_of_int (Hashtbl.hash s.names)
      ^ "-" ^ string_of_int (Random.bits ())
      ^ "-" ^ string_of_float (Sys.time ())
      ^ "-" ^ Date.rfc2445 Date.now
      ^ "-murder-generator" in
    List.concat (List.map (fun e ->
      "BEGIN:VEVENT"
      :: ("UID:" ^ string_of_int (Random.bits ())
                 ^ Date.rfc2445 e.History.event_begin
                 ^ Date.rfc2445 e.History.event_end
                 ^ "-" ^ string_of_int (Hashtbl.hash e) ^ id_postfix)
      :: ("DTSTAMP:" ^ Date.rfc2445 Date.now)
      :: ("DTSTART:" ^ Date.rfc2445 e.History.event_begin)
      :: ("DTEND:" ^ Date.rfc2445 e.History.event_end)
      :: List.map (fun c ->
             "ATTENDEE;CN=\"" ^ escape_quote (get_name s c) ^ "\""
             ^ ":URN:tag:" ^ webpage_address_base
             ^ "," ^ Date.iso8601 Date.now
             ^ ":" ^ string_of_int (Id.to_array c)
             ^ "," ^ String.escaped (get_name s c))
           (PSet.to_list e.History.event.Events.event_attendees)
      @ ("DESCRIPTION;LANGUAGE=" ^ Translation.iso639 s.language ^ ":")
      :: List.map (fun str -> " " ^ str ^ "\\n")
           (translate_event s tr_players e.History.event)
      @ "END:VEVENT"
      :: []) (State.get_history_final s.state)) in
  String.concat "\r\n" (split (
    "BEGIN:VCALENDAR"
    :: "VERSION:2.0"
    :: ("PRODID:-//Martin Constantino-Bodin//Murder Generator//"
        ^ String.uppercase_ascii (Translation.iso639 s.language))
    :: events
    @ "END:VCALENDAR"
    :: []))

let to_org s =
  let get_translation key =
    Utils.assert_option ("No key `" ^ key ^ "' found for language `"
                         ^ (Translation.iso639 s.language) ^ "' at "
                         ^ __LOC__ ^ ".")
      (Translation.translate s.generic_translation s.language key) in
  let tr_players = translation_players s in
  let print_event n e =
    String.concat "\n" (
      (String.make n '*' ^ " " ^
        String.concat ("\n" ^ String.make (1 + n) ' ')
          (translate_event s tr_players e.History.event))
      :: (String.make (1 + n) ' '
          ^ Date.orgmode_range e.History.event_begin e.History.event_end)
      :: List.map (fun c ->
           String.make (1 + n) ' ' ^ "- [X] "
           ^ get_name s c) (PSet.to_list e.History.event.Events.event_attendees)) in
  String.concat "\n\n" (
    String.concat "\n" (
      ("* " ^ get_translation "forTheGM")
      :: ("** " ^ get_translation "GMEvents")
      :: List.map (print_event 3) (State.get_history_final s.state))
    :: List.mapi (fun c name ->
         let c = Id.from_array c in
         String.concat "\n" (
           ("* " ^ name)
           :: ("** " ^ get_translation "characterAttributes")
           :: PMap.foldi (fun a v l ->
                  let tra = translate_attribute_player s a in
                  let trv = translate_value_player s v in
                  (String.make 4 ' ' ^ "- " ^ tra ^ ": " ^ trv) :: l)
                (State.get_all_attributes_character_final s.state c) []
           @ ("** " ^ get_translation "characterContacts")
           :: PMap.foldi (fun c' lv l ->
                  ("*** " ^ get_translation "contactTo" ^ " " ^ get_name s c')
                  :: List.map (fun (a, v) ->
                      let tra = translate_attribute_contact s a in
                      let trv = translate_value_contact s v in
                      String.make 5 ' ' ^ "- " ^ tra ^ ": " ^ trv) lv @ l)
                (State.get_all_contacts_character_final s.state c) []
           @ ("** " ^ get_translation "characterEvents")
           :: Utils.list_map_filter (fun e ->
                if PSet.mem c e.History.event.Events.event_attendees then
                  Some (print_event 3 e)
                else None) (State.get_history_final s.state))) s.names)

let to_json s =
  let s = generic s in
  Yojson.Safe.to_string ~std:true (`Assoc [
    ("characters", `List (List.mapi (fun c name ->
       let c = Id.from_array c in `Assoc [
           ("name", `String name) ;
           ("attributes", `Assoc (PMap.foldi (fun a v l ->
                let tra = translate_attribute_player s a in
                let trv = translate_value_player s v in
                (tra, `String trv) :: l)
              (State.get_all_attributes_character_final s.state c) [])) ;
           ("contacts", `List (PMap.foldi (fun c' lv l ->
             `Assoc (List.map (fun (a, v) ->
                 let tra = translate_attribute_contact s a in
                 let trv = translate_value_contact s v in
                 (tra, `String trv)) lv) :: l)
               (State.get_all_contacts_character_final s.state c) [])) ;
           ("relations", `List (Utils.list_fold_lefti (fun c' l _ ->
             let c' = Id.from_array c' in
             let r = State.read_relation_final s.state c c' in
             if c <= c' then l
             else `String (Relation.to_string r) :: l) [] s.names)) ;
           ("complexity", `Int (State.character_complexity_final s.state c)) ;
           ("difficulty", `Int (State.character_difficulty_final s.state c)) ;
         ]) s.names)) ;
    ("events", `List (List.map (fun e ->
      `Assoc [
          ("begin", `String (Date.rfc2445 e.History.event_begin)) ;
          ("end", `String (Date.rfc2445 e.History.event_end)) ;
          ("event", `String (translate_event_generic s e.History.event)) ;
          ("attendees",
            `List (List.map (fun c -> `Int (Id.to_array c))
                    (e.History.event.Events.event_attendees_list)))
        ]) (State.get_history_final s.state))) ])

let from_json m fileName fileContent =
  match Yojson.Safe.from_string ~fname:fileName fileContent with
  | `Assoc l ->
    let events =
      try match List.assoc "events" l with
        | `List l -> l
        | _ -> failwith ("The field `events' of file `" ^ fileName
                         ^ "' is not a list.")
      with Not_found ->
        failwith ("Missing field `events' in file `" ^ fileName ^ "'.") in
    let l =
      try match List.assoc "characters" l with
        | `List l -> l
        | _ -> failwith ("The field `characters' of file `" ^ fileName
                         ^ "' is not a list.")
      with Not_found ->
        failwith ("Missing field `characters' in file `" ^ fileName ^ "'.") in
    let state = State.create_state (List.length l) in
    let (names, state) =
      Utils.list_fold_lefti (fun c (names, state) ->
        let c = Id.from_array c in function
        | `Assoc l ->
          let name =
            try match List.assoc "name" l with
            | `String name -> name
            | _ -> failwith ("A field `name' in file `" ^ fileName
                             ^ "' is not a string.")
            with Not_found ->
              failwith ("Missing field `name' in file `" ^ fileName ^ "'.") in
          let get_field_list fld =
            try match List.assoc fld l with
            | `List l -> l
            | _ -> failwith ("A field `" ^ fld ^ "' in file `" ^ fileName
                             ^ "' is not a list")
            with
            | Not_found -> [] in
          let get_field_assoc fld =
            try match List.assoc fld l with
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
                  get_attribute "attribute" Attribute.PlayerAttribute.get_attribute
                    m.Attribute.player attribute in
                let v =
                  get_constructor "attribute"
                    Attribute.PlayerAttribute.get_constructor
                    m.Attribute.player attribute v in
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
                      get_attribute "contact"
                        Attribute.ContactAttribute.get_attribute
                        m.Attribute.contact attribute in
                    let v =
                      get_constructor "contact"
                        Attribute.ContactAttribute.get_constructor
                        m.Attribute.contact attribute v in
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
            try match List.assoc "complexity" l with
            | `Int v ->
              let rst = State.get_relation_state state in
              State.set_complexity rst c v ;
              state
            | _ -> failwith ("A field `complexity' in file `" ^ fileName
                             ^ "' is not an integer")
            with
            | Not_found -> state in
          let state =
            try match List.assoc "difficulty" l with
            | `Int v ->
              let rst = State.get_relation_state state in
              State.set_difficulty rst c v ;
              state
            | _ -> failwith ("A field `difficulty' in file `" ^ fileName
                             ^ "' is not an integer")
            with
            | Not_found -> state in
          (name :: names, state)
        | _ -> failwith ("A character in file `" ^ fileName
                 ^ "' is not a associated an object.")) ([], state) l in
    (* TODO: Deal with events (using Variable [events]). *)
    (List.rev names, state)
  | _ -> failwith ("The file `" ^ fileName ^ "' is not an object.")

let all_production = [
    ("json", "jsonDescription", "application/json", "json", true, to_json) ;
    ("orgmode", "orgDescription", "text/x-org", "org", true, to_org) ;
    ("graphviz", "graphvizDescription", "text/vnd.graphviz", "dot", true,
      to_graphviz) ;
    ("iCalendar", "icalDescription", "text/calendar", "ics", false, to_icalendar)
  ]

