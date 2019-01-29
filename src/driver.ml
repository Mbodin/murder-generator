
let categories_names = ref (Utils.Id.map_create ())

let elements_names = ref (Utils.Id.map_create ())

let constructor_information = ref State.empty_constructor_maps

(* TODO: We need to store a lot of informations about categories. *)

let get_declarations =
  List.iter (function
    | Ast.DeclareInstance (Ast.Attribute, attribute, block) ->
      constructor_information :=
        let i = !constructor_information in
        { i with State.player =
              snd (State.PlayerAttribute.declare_attribute i.State.player attribute) }
    | Ast.DeclareInstance (Ast.Contact, attribute, block) ->
      constructor_information :=
        let i = !constructor_information in
        { i with State.contact =
              snd (State.ContactAttribute.declare_attribute i.State.contact attribute) }
    | Ast.DeclareConstructor (kind, attribute, constructor, block) -> ()
    | Ast.DeclareCategory (name, block) ->
      categories_names := Utils.Id.map_insert !categories_names name
      (* TODO: Extracts all the information of the block. *)
    | Ast.DeclareElement (name, block) ->
      elements_names := Utils.Id.map_insert !elements_names name)

