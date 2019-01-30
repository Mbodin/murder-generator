
type intermediary = {
    categories_to_be_defined : string Utils.PSet.t (** A set of categories expected
                                                    * to be declared. **) ;
    categories_names : string Utils.Id.map (** All declared category names. **) ;
    elements_names : string Utils.Id.map (** All declared element names. **) ;
    constructor_information : State.constructor_maps (** The constructor maps. **) ;
    category_dependencies : (Utils.Id.t, string list) PMap.t (** For each category,
                                                              * associates its list
                                                              * of category dependencies.
                                                              * It has to be given as
                                                              * string as some might
                                                              * not yet have been loaded. **) ;
    attribute_dependencies : (State.attribute, string list) PMap.t (** Similarly, the
                                                                    * dependencies
                                                                    * of each attribute. **) ;
    (* TODO: We need to store a lot more information. *)
  }

let empty_intermediary = {
    categories_to_be_defined = Utils.PSet.empty ;
    categories_names = Utils.Id.map_create () ;
    elements_names = Utils.Id.map_create () ;
    constructor_information = State.empty_constructor_maps ;
    category_dependencies = PMap.empty ;
    attribute_dependencies = PMap.empty ;
    (* TODO *)
  }

let categories_to_be_defined i = i.categories_to_be_defined

let is_intermediary_final i =
  Utils.PSet.is_empty i.categories_to_be_defined

let all_categories i =
  (* TODO: Add a fold operator on [Utils.Id.map] *)
  Utils.Id.fold_map (fun id _ l -> id :: l) [] i.categories_names

(** Extracts each category dependency from a block,
 * returning them as another list. **)
let get_category_block =
  let rec aux accBlock accCategory = function
  | [] -> (accBlock, accCategory)
  | c :: l ->
    let (accBlock, accCategory) =
      match c with
      | Ast.OfCategory c -> (accBlock, c :: accCategory)
      | _ -> (c :: accBlock, accCategory) in
    aux accBlock accCategory l
  in aux [] []

let prepare_declarations =
  List.fold_left (fun i -> function
    | Ast.DeclareInstance (Ast.Attribute, attribute, block) ->
      let (block, categories) = get_category_block block in
      let (id, state) =
        State.PlayerAttribute.declare_attribute
          i.constructor_information.State.player attribute in
      { i with constructor_information =
                 { i.constructor_information with State.player = state } ;
               attribute_dependencies =
                 PMap.add (State.PlayerAttribute id) categories
                   i.attribute_dependencies }
    | Ast.DeclareInstance (Ast.Contact, attribute, block) ->
      let (block, categories) = get_category_block block in
      let (id, state) =
        State.ContactAttribute.declare_attribute
          i.constructor_information.State.contact attribute in
      { i with constructor_information =
                 { i.constructor_information with State.contact = state } ;
               attribute_dependencies =
                 PMap.add (State.ContactAttribute id) categories
                   i.attribute_dependencies }
    | Ast.DeclareConstructor (kind, attribute, constructor, block) ->
      TODO
      ()
    | Ast.DeclareCategory (name, block) ->
      TODO
      categories_names := Utils.Id.map_insert !categories_names name
      (* TODO: Extracts all the information of the block. *)
    | Ast.DeclareElement (name, block) ->
      TODO
      elements_names := Utils.Id.map_insert !elements_names name)

