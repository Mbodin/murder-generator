
(** Separates each components of the type [Ast.command] into a separate list. **)
type block = {
    of_category : string list ;
    translation : Ast.translation list ;
    add : Ast.add list ;
    compatible_with : string list ;
    let_player : Ast.let_player list ;
    provide_relation : Ast.provide_relation list ;
    provide_attribute : Ast.provide_attribute list ;
    provide_contact : Ast.provide_contact list
  }

let empty_block = {
    of_category = [] ;
    translation = [] ;
    add = [] ;
    compatible_with = [] ;
    let_player = [] ;
    provide_relation = [] ;
    provide_attribute = [] ;
    provide_contact = []
  }

type state = {
    categories_names : string Utils.Id.map (** All declared category names. **) ;
    elements_names : string Utils.Id.map (** All declared element names. **) ;
    constructor_information : State.constructor_maps (** The constructor maps. **) ;
    category_dependencies :
      (Utils.Id.t, Utils.Id.t Utils.PSet.t) PMap.t
      (** For each category, associates its set of category dependencies.
       * Note that this list is global: it also comprises the dependencies
       * of each dependencies, and so on. **) ;
    attribute_dependencies :
      (State.attribute, Utils.Id.t Utils.PSet.t) PMap.t
      (** Similarly, the dependencies of each attribute. **) ;
    constructor_dependencies :
      (Utils.Id.t, Utils.Id.t Utils.PSet.t) PMap.t
      (** Similarly, the dependencies of each constructor. **) ;
    (* TODO: Elements and translations. *)
  }

type intermediary = {
    current_state : state ; (** The current state. **)
    categories_to_be_defined :
      (Utils.Id.t, Utils.Id.t Utils.PSet.t
                   * State.attribute Utils.PSet.t
                   * Utils.Id.t Utils.PSet.t) PMap.t
      (** The set of categories expected to be declared.
       * For each of these, we also put thee sets to which the category dependencies
       * should be attached: they are respectively the set of category identifiers,
       * of attributes, and of constructors. **) ;
    attributes_to_be_defined : (State.attribute, Utils.Id.t Utils.PSet.t) PMap.t
      (** Similarly, the set of attributes expected to be declared. **)
  }

let empty_state = {
    categories_names = Utils.Id.map_create () ;
    elements_names = Utils.Id.map_create () ;
    constructor_information = State.empty_constructor_maps ;
    category_dependencies = PMap.empty ;
    attribute_dependencies = PMap.empty ;
    constructor_dependencies = PMap.empty ;
    (* TODO *)
  }

let empty_intermediary = {
    current_state = empty_state ;
    categories_to_be_defined = PMap.empty ;
    attributes_to_be_defined = PMap.empty
  }

let categories_to_be_defined i =
  Utils.PSet.map (fun id ->
    match Utils.Id.map_inverse i.current_state.categories_names id with
    | Some c -> c
    | None -> assert false) (Utils.PSet.domain i.categories_to_be_defined)

let attributes_to_be_defined i =
  Utils.PSet.partition_map (function
    | State.PlayerAttribute id ->
      (match State.PlayerAttribute.attribute_name
        i.current_state.constructor_information.player id with
      | Some c -> Utils.Left c
      | None -> assert false)
    | State.ContactAttribute id ->
      (match State.ContactAttribute.attribute_name
        i.current_state.constructor_information.contact id with
      | Some c -> Utils.Right c
      | None -> assert false)) (Utils.PSet.domain i.attributes_to_be_defined)

let is_intermediary_final i =
  PMap.is_empty i.categories_to_be_defined
  && PMap.is_empty i.attributes_to_be_defined

let all_categories i =
  Utils.Id.map_fold (fun _ id l -> id :: l) [] i.categories_names

(** This type is used as an internal type to express the kind of expected
 * command type in a given block. **)
type command_type =
  | OfCategory
  | Translation
  | Add
  | CompatibleWith
  | LetPlayer
  | ProvideRelation
  | ProvideAttribute
  | ProvideContact

(** Converts command types to string, for easier-to-understand error messages. **)
let command_type_to_string = function
  | OfCategory -> "category"
  | Translation -> "translation"
  | Add -> "add"
  | CompatibleWith -> "category compatibility"
  | LetPlayer -> "player declaration"
  | ProvideRelation -> "relation provision"
  | ProvideAttribute -> "attribute provision"
  | ProvideContact -> "contact provision"

exception UnexpectedCommandInBlock of string * string

exception DefinedTwice of string * string

exception CircularDependency of string

(** Converts an [Ast.block] into a [block].
 * It takes a list of command types and checks that only these are present
 * in the given block: all the other kinds will thus be empty lists.
 * This function reverses the order of declaration in the block
 * (although this shouldn’t impact on anything). **)
let convert_block block_name expected =
  let check c =
    if not (List.mem c expected) then
      raise (UnexpectedCommandInBlock (block_name, command_type_to_string c)) in
  let rec aux acc = function
  | [] -> acc
  | c :: l ->
    aux
      (match c with
      | Ast.OfCategory c ->
        check OfCategory ;
        { acc with of_category = c :: acc.of_category }
      | Ast.Translation t ->
        check Translation ;
        { acc with translation = t :: acc.translation }
      | Ast.Add a ->
        check Add ;
        { acc with add = a :: acc.add }
      | Ast.CompatibleWith c ->
        check CompatibleWith ;
        { acc with compatible_with = c :: acc.compatible_with }
      | Ast.LetPlayer l ->
        check LetPlayer ;
        { acc with let_player = l :: acc.let_player }
      | Ast.ProvideRelation p ->
        check ProvideRelation ;
        { acc with provide_relation = p :: acc.provide_relation }
      | Ast.ProvideAttribute p ->
        check ProvideAttribute ;
        { acc with provide_attribute = p :: acc.provide_attribute }
      | Ast.ProvideContact p ->
        check ProvideContact ;
        { acc with provide_contact = p :: acc.provide_contact }) l
  in aux empty_block

let category_to_id i id =
  match Utils.Id.get_id i.current_state.categories_names id with
  | Some name -> name
  | None -> assert false

let prepare_declaration i =
  let category_names_to_id_set l =
    Utils.PSet.from_list (List.map (category_to_id i) l) in
  let declare_instance declare extract update constructor en name block =
    let block = convert_block name [OfCategory] block in
    let (id, state) =
      declare (extract i.current_state.constructor_information) name in
    let id = constructor id in
    if PMap.mem id i.current_state.attribute_dependencies then
      raise (DefinedTwice (en, name)) ;
    (* TODO: attributes_to_be_defined *)
    { i with current_state =
      { i.current_state with
          constructor_information =
            update i.current_state.constructor_information state ;
          attribute_dependencies =
            PMap.add id (category_names_to_id_set block.of_category)
              i.current_state.attribute_dependencies } } in
  let declare_category name block =
    let block =
      convert_block attribute [OfCategory; Translation] block in
    let (id, categories_names) = TODO i.categories_names name in
    if PMap.mem id i.current_state.category_dependencies then
      raise (DefinedTwice ("category", name)) ;
    if List.mem name block.of_category then
      raise (CircularDependency name) ;
    let (cat_dep, att_dep, constr_dep) =
      try PMap.find id i.categories_to_be_defined
      with Not_found -> (Utils.PSet.empty, Utils.PSet.empty, Utils.PSet.empty) in
    (* TODO: Deal with these dependencies *)
    (* TODO: Store the translations in a generic type for translations. *)
    { i with
        categories_to_be_defined =
          Utils.PSet.remove name i.categories_to_be_defined ;
        current_state =
          { i.current_state with
              categories_names = categories_names ;
              category_dependencies =
                (* TODO: Also fetch the dependencies’s dependencies. *)
                PMap.add name (category_names_to_id_set block.of_category)
                  i.current_state.category_dependencies } } in
  function
  | Ast.DeclareInstance (Ast.Attribute, attribute, block) ->
    declare_instance State.PlayerAttribute.declare_attribute
                     (fun m -> m.State.player)
                     (fun i state -> { i with State.player = state })
                     (fun id -> State.PlayerAttribute id) "attribute"
                     attribute block
  | Ast.DeclareInstance (Ast.Contact, contact, block) ->
    declare_instance State.ContactAttribute.declare_attribute
                     (fun m -> m.State.contact)
                     (fun i state -> { i with State.contact = state })
                     (fun id -> State.ContactAttribute id) "contact"
                     contact block
  | Ast.DeclareConstructor (kind, attribute, constructor, block) ->
    let block =
      convert_block attribute [OfCategory; Translation; Add;
                               CompatibleWith] block in
    (* TODO *)
    i
  | Ast.DeclareCategory (name, block) ->
    declare_category name block
  | Ast.DeclareElement (name, block) ->
    let block =
      convert_block attribute [OfCategory; LetPlayer; ProvideRelation;
                               ProvideAttribute; ProvideContact] block in
    let i =
      { i with elements_names = Utils.Id.map_insert i.elements_names name } in
    TODO

let normalise i = i (*TODO*)

let prepare_declarations i l =
  normalise (List.fold_left prepare_declaration i l)

