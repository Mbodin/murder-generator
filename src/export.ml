
type state = {
    names : string array ;
    driver : Driver.state ;
    state : State.t
  }

let translate_attribute s lg a =
  let tr = Driver.translates_attribute s.driver in
  Utils.assert_option __LOC__ (Translation.translate tr a lg)

let translate_value s lg f v =
  let tr l =
    let tr = Driver.translates_constructor s.driver in
    let v = Utils.select_any l in
    let v = f v in
    Utils.assert_option __LOC__ (Translation.translate tr v lg) in
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
              let tra =
                translate_attribute s Translation.generic
                  (State.PlayerAttribute a) in
              let trv =
                translate_value s Translation.generic
                  (fun v -> State.PlayerConstructor v) v in
              ("{" ^ tra ^ "|" ^ trv ^ "}") :: l)
            (State.get_all_attributes_character cst c) [])
        ^ "}\"]") (Array.to_list s.names)) ; "" ;
      (** Declaring their relations **)
      String.concat "\n" (List.concat (List.mapi (fun c _ ->
        let c = Utils.Id.from_array c in
        PMap.foldi (fun a lv l ->
          List.map (fun (c', v) ->
            let tra =
              translate_attribute s Translation.generic
                (State.ContactAttribute a) in
            let trv =
              translate_value s Translation.generic
                (fun v -> State.ContactConstructor v) v in
            let color =
              if List.mem trv ["False"; "None"] then "transparent"
              else get_color tra in
            "  " ^ player_node c ^ " -> " ^ player_node c'
            ^ " [label=\"" ^ tra ^ ":" ^ trv ^ "\""
            ^ " color=\"" ^ color ^ "\"] ;") lv @ l)
        (State.get_all_contacts_character cst c) []) (Array.to_list s.names))) ;
      "}" ; ""
    ]

let to_json s =
  let cst = State.get_character_state s.state in
  Yojson.Safe.to_string ~std:true (`List (List.mapi (fun c name ->
    let c = Utils.Id.from_array c in `Assoc [
        ("name", `String name) ;
        ("attributes", `Assoc (PMap.foldi (fun a v l ->
             let tra =
               translate_attribute s Translation.generic
                 (State.PlayerAttribute a) in
             let trv =
               translate_value s Translation.generic
                 (fun v -> State.PlayerConstructor v) v in
             (tra, `String trv) :: l)
           (State.get_all_attributes_character cst c) [])) ;
        ("contacts", `Null (* TODO *)) ;
        ("relations", `Null (* TODO *)) ;
      ]) (Array.to_list s.names)))

let all_production = [
    ("graphviz", "graphvizDescription", "text/vnd.graphviz", "dot", to_graphviz) ;
    ("json", "jsonDescription", "application/json", "json", to_json) ;
  ]

