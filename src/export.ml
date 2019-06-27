
(* LATER: There would be a lot of possible factorisations in this file. *)

let webpage_address_base = "github.com/Mbodin/murder-generator"
let webpage_address = "https://" ^ webpage_address_base

type state = {
    names : string list ;
    language : Translation.language ;
    translation : Translation.element ;
    generic_translation : string Translation.t ;
    constructor_maps : Attribute.constructor_maps ;
    state : State.final ;
    unfinalised_state : State.t
  }

(** As the state [s], but there the translation language has moved to
 * the “generic” language. **)
let generic s = { s with language = Translation.generic }

(** A shortcut to [is_internal] for attributes. **)
let is_internal_attribute s a v =
  Attribute.PlayerAttribute.is_internal s.constructor_maps.Attribute.player a v

(** A shortcut to [is_internal] for contacts. **)
let is_internal_contact s a v =
  Attribute.ContactAttribute.is_internal s.constructor_maps.Attribute.contact a v

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
  fst (Translation.gforce_translate tr s.language (f v)
        (PSet.singleton Translation.base))

(** The following two functions instantiate [translate_value]
 * to player and contact attributes. **)
let translate_value_player s =
  translate_value s (fun v -> Attribute.PlayerConstructor v)
let translate_value_contact s =
  translate_value s (fun v -> Attribute.ContactConstructor v)

(** Translate an event from a translation object [trp] for player. **)
let translate_event s trp e =
  let (nb_sentence, tr) = e.Events.translation in
  let tr =
    Translation.sforce_translate ~debug:(fun i ->
        Some (string_of_int i ^ " of " ^ Events.print_event e))
      tr s.language in
  let l =
    List.map (fun i ->
        fst (tr (fst trp) (snd trp) i (PSet.singleton Translation.base)))
      (Utils.seq nb_sentence) in
  if String.concat "" l = "" then
    ["<Empty translation for " ^ Events.print_event e ^ ">"]
  else l

(** Generate a translation object for player for [translate_event]. **)
let translation_players s =
  let tags =
    let m =
      try PMap.find s.language s.translation.Translation.add
      with Not_found -> PMap.empty in
    PMap.foldi (fun c tags m ->
      let a =
        Utils.assert_option __LOC__
          (Attribute.PlayerAttribute.constructor_attribute
            s.constructor_maps.Attribute.player c) in
      List.fold_left (fun m p ->
        match State.get_attribute_character_final s.state p a with
        | None -> m
        | Some (c', fixed) ->
          if c = c' then (
            let sp =
              try PMap.find p m
              with Not_found -> PSet.empty in
            PMap.add p (PSet.merge sp tags) m
          ) else m) m (State.all_players_final s.state)) m PMap.empty in
  let read_tags p =
    try PMap.find p tags
    with Not_found -> PSet.empty in
  let tr_player =
    Array.mapi (fun c name ->
      let c = Id.from_array c in
      let att = State.get_all_attributes_character_final s.state c in
      let tr = Translation.gadd Translation.gempty s.language [] () name in
      PMap.foldi (fun a (c, fixed) tr ->
          let c = Attribute.PlayerConstructor c in
          Translation.gfold (fun tr tags str added removed ->
            if not (List.mem Translation.base tags) then (
              let commands =
                let convert b s =
                  List.map (fun tag -> (Some b, tag)) (PSet.to_list s) in
                List.map (fun tag -> (None, tag)) tags
                @ convert true added
                @ convert false removed in
              Translation.gadd tr s.language commands () str
            ) else tr) tr s.translation.Translation.constructor c s.language)
        att tr) (Array.of_list s.names) in
  let translate c =
    Translation.gtranslate tr_player.(Id.to_array c) s.language () in
  (read_tags, translate)

(** Return the name of a character [c]. **)
let get_name s c =
  List.nth s.names (Id.to_array c)

(** Replace the characters '"' by '\"' and '\' by '\\'. **)
let escape_quote str =
  let replace input = Re.Str.global_replace (Re.Str.regexp_string input) in
  replace "\"" "\\\"" (replace "\\" "\\\\" str)

(** Replace the characters '<' and '{' by ']' and '>' and '}' by ']'. **)
let escape_angle_brackets str =
  let replace2 in1 in2 =
    Re.Str.global_replace (Re.Str.regexp (in1 ^ "\\|" ^ in2)) in
  replace2 "{" "<" "[" (replace2 "}" ">" "]" str)

let to_graphviz_relation s =
  let player_node c = "player" ^ string_of_int (Id.to_array c) in
  let get_color s =
    let n = float_of_int (Hashtbl.hash s mod 1000) /. 1000. in
    string_of_float n ^ " 1 0.7" in
  let escape str = escape_angle_brackets (escape_quote str) in
  String.concat "\n" [
      "digraph {" ; "" ;
      "  // Generated from " ^ webpage_address ; "" ;
      "  node [shape=record]" ; "" ;
      (** Declaring each player **)
      String.concat "\n" (List.mapi (fun c name ->
        let c = Id.from_array c in
        "  " ^ player_node c ^ " [label=\"{"
        ^ escape name ^ "|"
        ^ String.concat "|" (PMap.foldi (fun a (v, fixed) l ->
              if is_internal_attribute s a v then l
              else
                let tra = translate_attribute_player s a in
                let trv = translate_value_player s v in
                ("{" ^ escape tra ^ "|" ^ escape trv ^ "}") :: l)
            (State.get_all_attributes_character_final s.state c) [])
        ^ "}\"]") s.names) ; "" ;
      (** Declaring their relations **)
      String.concat "\n" (List.concat (List.mapi (fun c _ ->
        let c = Id.from_array c in
        PMap.foldi (fun c' lv l ->
            Utils.list_map_filter (fun (a, (v, fixed)) ->
              if is_internal_contact s a v then None
              else
                let converse =
                  Utils.if_option (State.get_contact_character_final s.state c' a c)
                    (fun (v', _) ->
                       if is_internal_contact s a v' then None
                       else Some v') in
                if converse <> None && Id.to_array c < Id.to_array c' then None
                else Some (
                  let tra = translate_attribute_contact s a in
                  let trv = translate_value_contact s v in
                  let trv' =
                    match converse with
                    | None -> ""
                    | Some v' ->
                      if v = v' then ""
                      else "\\n" ^ escape (translate_value_contact s v') in
                  let color = get_color tra in
                  "  " ^ player_node c ^ " -> " ^ player_node c'
                  ^ " [label=\"" ^ escape tra ^ "\\n" ^ escape trv ^ trv' ^ "\""
                  ^ (if converse <> None then " dir=\"both\"" else "")
                  ^ " color=\"" ^ color ^ "\"] ;")) lv @ l)
          (State.get_all_contacts_character_final s.state c) []) s.names)) ;
      "}" ; ""
    ]

let to_graphviz_event s =
  let event_node id = "event" ^ string_of_int (Id.to_array id) in
  let tr_players = translation_players s in
  let end_file = ["}" ; ""] in
  let graph =
    History.fold_graph (fun l ev id (before, after) ->
      let color =
        let v =
          let v = List.length after in
          if v > 4 then 0.8
          else float_of_int (v - 1) /. 5. in
        "0 0 " ^ string_of_float v in
      ("  " ^ event_node id
       ^ if ev.Events.event_phantom then
           " [shape=circle, label=\"\"] ;"
         else
           (" [style=rounded, label=\""
            ^ String.concat "|"
                (List.map escape_angle_brackets (translate_event s tr_players ev))
            ^ "\"] ;"))
      :: List.map (fun id' ->
        "  " ^ event_node id ^ " -> " ^ event_node id'
        ^ " [color=\"" ^ color ^ "\"] ;") after
      @ "" :: l) end_file (State.get_history_state s.unfinalised_state) in
  String.concat "\n" (
      "digraph {" :: ""
      :: ("  // Generated from " ^ webpage_address) :: ""
      :: "  node [shape=record] ;" :: "  rankdir=LR ;" :: ""
      :: graph)

let to_icalendar s =
  let rec split = function
    | [] -> []
    | str :: l ->
      if String.length str < 75 then
        str :: split l
      else
        let rec v i =
          if int_of_char str.[i] < 128 then i
          else if i = 1 then 1
          else v (i - 1) in
        let v = v 74 in
        String.sub str 0 v
        :: split ((" " ^ String.sub str v (String.length str - v)) :: l) in
  let events =
    let tr_players = translation_players s in
    let id_postfix =
      "-" ^ string_of_int (Hashtbl.hash s.names)
      ^ "-" ^ string_of_int (Hashtbl.hash s.state)
      ^ "-" ^ string_of_int (Random.bits ())
      ^ "-" ^ string_of_float (Sys.time ())
      ^ "-" ^ Date.rfc2445 Date.now
      ^ "-murder-generator" in
    List.concat (List.map (fun e ->
      "BEGIN:VEVENT"
      :: ("UID:" ^ string_of_int (Random.bits ())
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

let to_block s =
  let get_translation key =
    Utils.assert_option ("No key `" ^ key ^ "' found for language `"
                         ^ (Translation.iso639 s.language) ^ "' at "
                         ^ __LOC__ ^ ".")
      (Translation.translate s.generic_translation s.language key) in
  let event_type_to_class = function
    | Events.For_life_event -> "decades"
    | Events.Long_term_event -> "years"
    | Events.Medium_term_event -> "weeks"
    | Events.Short_term_event -> "days"
    | Events.Very_short_term_event -> "minutes"
    | Events.Immediate_event -> "seconds" in
  let tr_players = translation_players s in
  let print_events l =
    let nb_event_types = List.length Events.all_event_type in
    let header =
      (InOut.Text (get_translation "Year"), InOut.default)
      :: (InOut.Text (get_translation "Month"), InOut.default)
      :: (InOut.Text (get_translation "Day"), InOut.default)
      :: (InOut.Text (get_translation "Time"), InOut.default)
      :: List.map (fun name ->
           (InOut.Text name,
            { InOut.default with InOut.col = nb_event_types })) s.names
      @ [(InOut.Text (get_translation "eventDescription"), InOut.default)] in
    let content =
      (** We first determine how many “events” each event lasts,
       * counting the number of events with the same year, month, and day.
       * Furthermore, we cound how many events this event overlaps. **)
      let rec get_event_sharing = function
        | [] -> []
        | (beg, d, e) :: l ->
          let de = if beg then e.History.event_end else d in
          (* LATER: This is far from being an optimised algorithm. *)
          let rec aux f before num = function
            | [] -> num
            | (_, d', _) :: l ->
              if before d' d || f d' > f d then num
              else (
                if Utils.assert_defend then
                  assert (f d' = f d) ;
                aux f before (1 + num) l) in
          let rec duration n = function
            | [] -> n
            | (_, d', e') :: l ->
              if Date.compare de d' < 0
                 || (Date.compare de d' = 0
                     && Events.compare e.History.event e'.History.event = 0) then n
              else duration (1 + n) l in
          ((aux Date.year (fun _ _ -> false) 0 l,
            aux Date.month (fun d' d -> Date.year d' > Date.year d) 0 l,
            aux Date.day (fun d' d ->
              Date.year d' > Date.year d || Date.month d' > Date.month d) 0 l),
            duration 0 l, beg, d, e)
          :: get_event_sharing l in
      let evs =
        (** This function splits the list of events into a list of event
         * begin and ends, each with a boolean indicating whether the event
         * starts or ends, a date, and the corresponding event. **)
        let rec split_begin_end waiting_end = function
          | [] -> waiting_end
          | e :: l ->
            let d = e.History.event_begin in
            let (just_ended, waiting_end) =
              Utils.list_predicate_prefix (fun (_, d', _) ->
                Date.compare d' d <= 0) waiting_end in
            let waiting_end =
              let de = e.History.event_end in
              let rec aux = function
                | [] -> [(false, de, e)]
                | ((_, d', _) :: _) as l when Date.compare de d' <= 0 ->
                  (false, de, e) :: l
                | e :: l -> e :: aux l in
              aux waiting_end in
            just_ended @ (true, d, e) :: split_begin_end waiting_end l in
        get_event_sharing (split_begin_end [] l) in
      let activity =
        (** The activity of each line in the timeline: for each player and
         * event type, how many turns are they going to stay here. **)
        let m =
          List.fold_left (fun m t ->
            PMap.add t 0 m) PMap.empty Events.all_event_type in
        Array.make (List.length s.names) m in
      let (_, _, active, content) =
        List.fold_left (fun ((y, m, d), activity, active, l)
                            ((year, month, day), dur, beg, date, e) ->
            let (time, (y, m, d)) =
              let create_time text fusion =
                ([(InOut.Text text, {
                   InOut.row = 1 + fusion ;
                   InOut.col = 1 ;
                   InOut.classes = ["time"]
                  })], fusion) in
              let string_of_month n =
                if Utils.assert_defend then
                  assert (n > 0 && n <= 12) ;
                get_translation ("Month" ^ string_of_int n) in
              let (ty, y) =
                if y = 0 then
                  create_time (string_of_int (Date.year date)) year
                else ([], y - 1) in
              let (tm, m) =
                if m = 0 then
                  create_time (string_of_month (Date.month date)) month
                else ([], m - 1) in
              let (td, d) =
                if d = 0 then
                  create_time (string_of_int (Date.day date)) day
                else ([], d - 1) in
              let hour =
                let complete n =
                  Utils.complete_string_pre "0" (string_of_int n) 2 in
                (InOut.Text (complete (Date.hour date)
                             ^ ":" ^ complete (Date.minute date)),
                 { InOut.default with InOut.classes = ["time"] }) in
              (ty @ tm @ td @ [hour], (y, m, d)) in
            let lines =
              List.concat (List.map (fun c ->
                let a = activity.(Id.to_array c) in
                let attendee = PSet.mem c e.History.event.Events.event_attendees in
                List.map (fun t ->
                    let n =
                      try PMap.find t a
                      with Not_found -> 0 in
                    (InOut.Text "",
                     { InOut.default with classes =
                         "line" :: event_type_to_class t ::
                           if attendee
                              && t = e.History.event.Events.event_type then (
                                assert (n = 0) ;
                             ["active"; if beg then "event_start" else "event_end"]
                           ) else if n = 0 then [] else ["active"] }))
                  Events.all_event_type) (State.all_players_final s.state)) in
            let activity =
              Array.mapi (fun c ->
                let attendee =
                  PSet.mem (Id.from_array c)
                    e.History.event.Events.event_attendees in
                PMap.mapi (fun t n ->
                  if beg && attendee
                     && t = e.History.event.Events.event_type then dur
                  else max 0 (n - 1))) activity in
            let description =
              let node =
                if beg then
                  InOut.List (false,
                    List.map (fun text -> InOut.Text text)
                      (translate_event s tr_players e.History.event))
                else InOut.Space in
              let t = event_type_to_class e.History.event.Events.event_type in
              [(node, { InOut.default with InOut.classes =
                          if beg then ["description"; t] else [] })] in
            let tr = ([], time @ lines @ description) in
            ((y, m, d), activity, active, tr :: l))
          ((0, 0, 0), activity, [], []) evs in
      List.rev content in
    InOut.Table (["timeline"], header, content) in
  let print_attributes c =
    InOut.List (true,
      PMap.foldi (fun a (v, fixed) l ->
          if is_internal_attribute s a v then l
          else
            let tra = translate_attribute_player s a in
            let trv = translate_value_player s v in
            InOut.Text (tra ^ ": " ^ trv) :: l)
        (State.get_all_attributes_character_final s.state c) []) in
  let print_contacts c =
    InOut.List (false,
      PMap.foldi (fun c' lv l ->
          InOut.FoldableBlock (true,
            get_translation "contactTo" ^ " " ^ get_name s c',
              InOut.List (true,
                Utils.list_map_filter (fun (a, (v, fixed)) ->
                    if is_internal_contact s a v then None
                    else Some (
                      let tra = translate_attribute_contact s a in
                      let trv = translate_value_contact s v in
                      InOut.Text (tra ^ ": " ^ trv))) lv)) :: l)
        (State.get_all_contacts_character_final s.state c) []) in
  InOut.List (false,
    InOut.FoldableBlock (false, get_translation "forTheGM",
      InOut.List (false, [
          InOut.FoldableBlock (false, get_translation "GMCharacters",
            InOut.List (true, List.mapi (fun c name ->
              let c = Id.from_array c in
              InOut.FoldableBlock (true, name, print_attributes c)) s.names)) ;
          InOut.FoldableBlock (false, get_translation "GMContacts",
            InOut.List (true, List.mapi (fun c name ->
              let c = Id.from_array c in
              InOut.FoldableBlock (false, name, print_contacts c)) s.names)) ;
          InOut.FoldableBlock (false, get_translation "GMEvents",
            print_events (State.get_history_final s.state))
        ]))
    :: List.mapi (fun c name ->
         let c = Id.from_array c in
         InOut.FoldableBlock (false, name,
           InOut.List (false, [
               InOut.FoldableBlock (true, get_translation "characterAttributes",
                 print_attributes c) ;
               InOut.FoldableBlock (true, get_translation "characterContacts",
                 print_contacts c) ;
               InOut.FoldableBlock (true, get_translation "characterEvents",
                 print_events (Utils.list_map_filter (fun e ->
                   if PSet.mem c e.History.event.Events.event_attendees then
                     Some e
                   else None) (State.get_history_final s.state)))
             ]))) s.names)

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
  let print_attributes n c =
    PMap.foldi (fun a (v, fixed) l ->
        if is_internal_attribute s a v then l
        else
          let tra = translate_attribute_player s a in
          let trv = translate_value_player s v in
          (String.make n ' ' ^ "- " ^ tra ^ ": " ^ trv) :: l)
      (State.get_all_attributes_character_final s.state c) [] in
  let print_contacts n c =
    PMap.foldi (fun c' lv l ->
        (String.make n '*' ^ " " ^ get_translation "contactTo"
         ^ " " ^ get_name s c')
        :: Utils.list_map_filter (fun (a, (v, fixed)) ->
             if is_internal_contact s a v then None
             else Some (
               let tra = translate_attribute_contact s a in
               let trv = translate_value_contact s v in
               String.make 5 ' ' ^ "- " ^ tra ^ ": " ^ trv)) lv @ l)
      (State.get_all_contacts_character_final s.state c) [] in
  String.concat "\n\n" (
    String.concat "\n" (
      ("* " ^ get_translation "forTheGM")
      :: ("** " ^ get_translation "GMCharacters")
      :: List.concat (List.mapi (fun c name ->
           let c = Id.from_array c in
           ("*** " ^ name)
           :: print_attributes 5 c) s.names)
      @ ("** " ^ get_translation "GMContacts")
      :: List.concat (List.mapi (fun c name ->
           let c = Id.from_array c in
           ("*** " ^ name)
           :: print_contacts 4 c) s.names)
      @ ("** " ^ get_translation "GMEvents")
      :: List.map (print_event 3) (State.get_history_final s.state))
    :: List.mapi (fun c name ->
         let c = Id.from_array c in
         String.concat "\n" (
           ("* " ^ name)
           :: ("** " ^ get_translation "characterAttributes")
           :: print_attributes 4 c
           @ ("** " ^ get_translation "characterContacts")
           :: print_contacts 3 c
           @ ("** " ^ get_translation "characterEvents")
           :: Utils.list_map_filter (fun e ->
                if PSet.mem c e.History.event.Events.event_attendees then
                  Some (print_event 3 e)
                else None) (State.get_history_final s.state))) s.names)

let to_json s =
  let s = generic s in
  Yojson.Safe.to_string ~std:true (`Assoc [
    ("version", `String Version.version) ;
    ("characters", `List (List.mapi (fun c name ->
       let c = Id.from_array c in `Assoc [
           ("name", `String name) ;
           ("attributes", `Assoc (PMap.foldi (fun a (v, fixed) l ->
                let tra = translate_attribute_player s a in
                let trv = translate_value_player s v in
                let trv = if fixed then trv else ("?" ^ trv) in
                (tra, `String trv) :: l)
              (State.get_all_attributes_character_final s.state c) [])) ;
           ("contacts", `List (PMap.foldi (fun c' lv l ->
              `Assoc (List.map (fun (a, (v, fixed)) ->
                  let tra = translate_attribute_contact s a in
                  let trv = translate_value_contact s v in
                  let trv = if fixed then trv else ("?" ^ trv) in
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
          ("event", `String (Events.print_event e.History.event)) ;
          ("attendees",
            `List (List.map (fun c -> `Int (Id.to_array c))
                     (Events.get_attendees_list e.History.event))) ;
          ("all",
            `List (List.map (fun c -> `Int (Id.to_array c))
                    (e.History.event.Events.all_attendees)))
        ]) (State.get_history_final s.state))) ])

let from_json i fileName fileContent =
  let get_field_string fld l =
    try match List.assoc fld l with
    | `String name -> name
    | _ -> failwith ("A field `" ^ fld ^ "' in file `" ^ fileName
                     ^ "' is not a string.")
    with Not_found ->
      failwith ("Missing field `" ^ fld ^
                "' in file `" ^ fileName ^ "'.") in
  let get_field_int fld l =
    try match List.assoc fld l with
    | `Int v -> Some v
    | _ -> failwith ("A field `" ^ fld ^ "' in file `" ^ fileName
                     ^ "' is not an integer.")
    with Not_found -> None in
  let get_field_list fld l =
    try match List.assoc fld l with
    | `List l -> l
    | _ -> failwith ("A field `" ^ fld ^ "' in file `" ^ fileName
                     ^ "' is not a list.")
    with Not_found -> [] in
  let get_field_assoc fld l =
    try match List.assoc fld l with
    | `Assoc l -> l
    | _ -> failwith ("A field `" ^ fld ^ "' in file `" ^ fileName
                     ^ "' is not an object.")
    with Not_found -> [] in
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
          let name = get_field_string "name" l in
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
            let attributes = get_field_assoc "attributes" l in
            List.fold_left (fun state -> function
              | (attribute, `String v) ->
                let (v, fixed) =
                  if String.length v = 0 then
                    failwith ("Empty attribute `" ^ attribute ^ "' in file `"
                              ^ fileName ^ "'.")
                  else if v.[0] = '?' then
                    (String.sub v 1 (String.length v - 1), false)
                  else (v, true) in
                let attribute =
                  get_attribute "attribute" Attribute.PlayerAttribute.get_attribute
                    i.Driver.constructor_maps.Attribute.player attribute in
                let v =
                  get_constructor "attribute"
                    Attribute.PlayerAttribute.get_constructor
                    i.Driver.constructor_maps.Attribute.player attribute v in
                let v =
                  if fixed then (State.Fixed_value ([v], Strict))
                  else (State.One_value_of [v]) in
                State.write_attribute_character (State.get_character_state state) c
                  attribute v ;
                state
              | (field, _) ->
                failwith ("Field `" ^ field ^ "' is file `" ^ fileName
                          ^ "' is supposed to be an attribute and thus associated"
                          ^ " to a string, which it is not.")) state attributes in
          let state =
            let contacts = get_field_list "contacts" l in
            Utils.list_fold_lefti (fun c' state ->
              let c' = Id.from_array c' in function
              | `Assoc l ->
                List.fold_left (fun state -> function
                  | (attribute, `String v) ->
                    let (v, fixed) =
                      if String.length v = 0 then
                        failwith ("Empty contact `" ^ attribute ^ "' in file `"
                                  ^ fileName ^ "'.")
                      else if v.[0] = '?' then
                        (String.sub v 1 (String.length v - 1), false)
                      else (v, true) in
                    let attribute =
                      get_attribute "contact"
                        Attribute.ContactAttribute.get_attribute
                        i.Driver.constructor_maps.Attribute.contact attribute in
                    let v =
                      get_constructor "contact"
                        Attribute.ContactAttribute.get_constructor
                        i.Driver.constructor_maps.Attribute.contact attribute v in
                    let v =
                      if fixed then (State.Fixed_value ([v], Strict))
                      else (State.One_value_of [v]) in
                    State.write_contact_character (State.get_character_state state)
                      c attribute c' v ;
                    state
                  | (field, _) ->
                    failwith ("Field `" ^ field ^ "' is file `" ^ fileName
                              ^ "' is supposed to be a contact and thus associated"
                              ^ " to a string, which it is not.")) state l
              | _ -> failwith ("A contact in file `" ^ fileName
                               ^ "' is not associated an object.")) state contacts in
          let state =
            let relations = get_field_list "relations" l in
            Utils.list_fold_lefti (fun c' state ->
              let c' = Id.from_array c' in function
              | `String r ->
                let r = Driver.parse_relation r in
                if c <> c' then State.write_relation state c c' r ;
                state
              | _ -> failwith ("Ill-formed `relations' field in file `"
                               ^ fileName ^ "'.")) state relations in
          let state =
            match get_field_int "complexity" l with
            | Some v ->
              let rst = State.get_relation_state state in
              State.set_complexity rst c v ;
              state
            | None -> state in
          let state =
            match get_field_int "difficulty" l with
            | Some v ->
              let rst = State.get_relation_state state in
              State.set_difficulty rst c v ;
              state
            | None -> state in
          (name :: names, state)
        | _ -> failwith ("A character in file `" ^ fileName
                         ^ "' is not associated an object.")) ([], state) l in
    let events =
      List.map (function
        | `Assoc l ->
          let be = Date.from_rfc2445 (get_field_string "begin" l) in
          let en = Date.from_rfc2445 (get_field_string "end" l) in
          let name = get_field_string "event" l in
          let id =
            try PMap.find name i.Driver.event_id
            with Not_found -> failwith ("No event `" ^ name ^ "' found.") in
          let all =
            List.map (function
              | `Int c -> Id.from_array c
              | _ ->
                failwith ("An element of the full attendee list of an event is "
                          ^ "not an integer.")) (get_field_list "all" l) in
          let attendees =
            List.map (function
              | `Int c -> Id.from_array c
              | _ ->
                failwith ("An element of the attendee list of an event is "
                          ^ "not an integer.")) (get_field_list "attendees" l) in
          if not (List.for_all (fun c -> List.mem c all) attendees) then
            failwith ("Event `" ^ name ^ "' has an invalid list of attendees.") ;
          let convert = List.nth_opt all in
          let (phantom, blocking, translation) =
            let (phantom, blocking, (n, tr)) =
              try PMap.find id i.Driver.event_informations
              with Not_found -> assert false in
            match Translation.smap_option convert tr with
            | Some tr -> (phantom, blocking, (n, tr))
            | None ->
              failwith ("Non-matching translation for event `" ^ name ^ "'.") in
          let kinds =
            let k =
              try PMap.find id i.Driver.event_kinds
              with Not_found -> assert false in
            match PMap.foldi (fun c k m ->
                    Utils.if_option m (fun m ->
                      Utils.if_option (convert c) (fun c ->
                        Utils.apply_option (PSet.map_option
                          (Events.kind_convert convert) k) (fun k ->
                            PMap.add c k m)))) k (Some PMap.empty) with
            | Some k -> k
            | None -> failwith ("Non-matching kinds for event `" ^ name ^ "'.") in
          let e = {
              Events.event_id = id ;
              Events.event_phantom = phantom ;
              Events.event_blocking = blocking ;
              Events.event_type = History.get_event_type be en ;
              Events.event_attendees = PSet.from_list attendees ;
              Events.all_attendees = all ;
              Events.event_kinds = kinds ;
              Events.constraints_none = PMap.empty ;
              Events.constraints_some = PMap.empty ;
              Events.translation = translation
            } in {
            History.event_begin = be ;
            History.event_end = en ;
            History.event = e
          }
        | _ -> failwith ("An event of file `" ^ fileName
                         ^ "' is not an object.")) events in
    let state = State.set_history_state state (History.unfinalise events) in
    (List.rev names, state)
  | _ -> failwith ("The file `" ^ fileName ^ "' is not an object.")

let all_production = [
    ("json", "jsonDescription", "application/json", "json", true, to_json) ;
    ("orgmode", "orgDescription", "text/x-org", "org", true, to_org) ;
    ("graphvizRel", "graphvizRelDescription",
     "text/vnd.graphviz", "dot", true, to_graphviz_relation) ;
    ("graphvizEv", "graphvizEvDescription",
     "text/vnd.graphviz", "dot", true, to_graphviz_event) ;
    ("iCalendar", "icalDescription", "text/calendar", "ics", false, to_icalendar)
  ]

