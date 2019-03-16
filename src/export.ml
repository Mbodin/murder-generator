
type state = {
    names : string list ;
    language : Translation.language ;
    translation : Translation.element ;
    state : State.t
  }

let translate_attribute s a =
  let tr = s.translation.Translation.attribute in
  Translation.force_translate tr a s.language

let translate_value s f v =
  let tr l =
    let tr = s.translation.Translation.constructor in
    let v = Utils.select_any l in
    let v = f v in
    Translation.force_translate tr v s.language in
  match v with
  | State.Fixed_value (l, strict) -> tr l
  | State.One_value_of l -> tr l

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
            let color =
              if List.mem trv ["False"; "None"] then "transparent"
              else get_color tra in
            "  " ^ player_node c ^ " -> " ^ player_node c'
            ^ " [label=\"" ^ tra ^ ":" ^ trv ^ "\""
            ^ " color=\"" ^ color ^ "\"] ;") lv @ l)
        (State.get_all_contacts_character cst c) []) s.names)) ;
      "}" ; ""
    ]

let to_json s =
  let s = { s with language = Translation.generic } in
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
          if c < c' then l
          else `String (Relation.to_string r) :: l) [] s.names)) ;
      ]) s.names))

let all_production = [
    ("graphviz", "graphvizDescription", "text/vnd.graphviz", "dot", to_graphviz) ;
    ("json", "jsonDescription", "application/json", "json", to_json) ;
  ]

