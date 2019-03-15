
type state = {
    names : string array ;
    driver : Driver.state ;
    state : State.t
  }

let to_graphviz s =
  let player_node c =
    "player" ^ string_of_int (Utils.Id.to_array c) in
  let get_name c = s.names.(Utils.Id.to_array c) in
  let cst = State.get_character_state s.state in
  String.concat "\n" [
      "digraph {" ; "" ;
      "  // Generated from https://github.com/Mbodin/murder-generator" ; "" ;
      "  node [shape=record]" ; "" ;
      (** Declaring each player **)
      String.concat "\n" (List.mapi (fun c name ->
        let c = Utils.Id.from_array c in
        "  " ^ player_node c ^ " [label=\"{"
        ^ get_name c ^ "|"
        ^ String.concat "|" (PMap.foldi (fun a v l ->
              let a = State.PlayerAttribute a in
              (* TODO: Factorise *)
              let tra =
                let tr = Driver.translates_attribute s.driver in
                Utils.assert_option __LOC__
                  (Translation.translate tr a Translation.generic) in
              let trv =
                let tr_strict = function
                  | State.NonStrict -> "compatible "
                  | State.LowStrict -> ""
                  | State.Strict -> "strict " in
                let tr l =
                  let tr = Driver.translates_constructor s.driver in
                  String.concat ", " (List.map (fun v ->
                      let v = State.PlayerConstructor v in
                      Utils.assert_option __LOC__
                        (Translation.translate tr v Translation.generic))
                    l) in
                match v with
                | State.Fixed_value (l, strict) -> tr_strict strict ^ tr l
                | State.One_value_of l -> "? " ^ tr l in
              ("{" ^ tra ^ "|" ^ trv ^ "}") :: l)
            (State.get_all_attributes_character cst c) [])
        ^ "}\"]") (Array.to_list s.names)) ; "" ;
      (** Declaring their relations **)
      String.concat "\n" (List.concat (List.mapi (fun c _ ->
        let c = Utils.Id.from_array c in
        PMap.foldi (fun a lv l ->
          List.map (fun (c', v) ->
            let a = State.ContactAttribute a in
            let tra =
              let tr = Driver.translates_attribute s.driver in
              Utils.assert_option __LOC__
                (Translation.translate tr a Translation.generic) in
            let trv =
              let tr_strict = function
                | State.NonStrict -> "compatible "
                | State.LowStrict -> ""
                | State.Strict -> "strict " in
              let tr l =
                let tr = Driver.translates_constructor s.driver in
                String.concat ", " (List.map (fun v ->
                    let v = State.ContactConstructor v in
                    Utils.assert_option __LOC__
                      (Translation.translate tr v
                        Translation.generic)) l) in
              match v with
              | State.Fixed_value (l, strict) ->
                tr_strict strict ^ tr l
              | State.One_value_of l -> "? " ^ tr l in
            (*TODO: tra ^ " to " ^ player_node c' ^ "(" ^ trv ^ "); "*)
            "  " ^ player_node c ^ " -> " ^ player_node c' ^ " ;") lv @ l)
        (State.get_all_contacts_character cst c) []) (Array.to_list s.names))) ;
      "}" ; ""
    ]

let all_production = [
    ("graphviz", "text/vnd.graphviz", "dot", to_graphviz)
  ]

