
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
    category_dependencies : (Utils.Id.t, string list) PMap.t (** For each category,
                                                              * associates its list
                                                              * of category dependencies.
                                                              * It has to be given as
                                                              * string as some might
                                                              * not yet have been loaded. **) ;
    attribute_dependencies : (State.attribute, string list) PMap.t (** Similarly, the
                                                                    * dependencies
                                                                    * of each attribute. **) ;
    (* TODO *)
  }

(* TODO: Why not storing directly [string] as identifiers as soon as found
 * and storing the identifiers of the waiting things to be defined?
 * This would enable to embed the type [state] (or at least the informations
 * about categories, attributes, and constructors) directly inside the type
 * [intermediary]. *)
type intermediary = {
    current_state : state ; (** The current state. **)
    categories_to_be_defined : Utils.Id.t Utils.PSet.t (** A set of categories expected
                                                        * to be declared. **) ;
                                                        (* TODO: Instead of a set, put a
                                                         * map from category identifiers
                                                         * to three sets: a set of categories,
                                                         * of attributes, and of attribute
                                                         * constructors that depends on them
                                                         * (and the same from attributes (to be
                                                         * defined) to constructors).
                                                         * That way, one can directly propagates
                                                         * the category dependencies along the
                                                         * parsing, without having to go back.
                                                         * This map is constantly updated. *)
    (* TODO: We need to store a lot more information. *)
  }

let empty_state = {
    categories_names = Utils.Id.map_create () ;
    elements_names = Utils.Id.map_create () ;
    constructor_information = State.empty_constructor_maps ;
    category_dependencies = PMap.empty ;
    attribute_dependencies = PMap.empty ;
    (* TODO *)
  }

let empty_intermediary = {
    current_state = empty_state ;
    categories_to_be_defined = Utils.PSet.empty ;
    (* TODO *)
  }

let categories_to_be_defined i =
  Utils.PSet.map (fun id ->
    match Utils.Id.map_inverse i.current_state.categories_names id with
    | Some c -> c
    | None -> assert false) i.categories_to_be_defined

let is_intermediary_final i =
  Utils.PSet.is_empty i.categories_to_be_defined

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

let prepare_declaration i = function
  | Ast.DeclareInstance (Ast.Attribute, attribute, block) ->
    let block =
      convert_block attribute [OfCategory] block in
    let (id, state) =
      State.PlayerAttribute.declare_attribute
        i.current_state.constructor_information.State.player attribute in
    let id = State.PlayerAttribute id in
    if PMap.mem id i.current_state.attribute_dependencies then
      raise (DefinedTwice ("attribute", attribute)) ;
    { i with current_state =
      { i.current_state with
          constructor_information =
             { i.current_state.constructor_information with State.player = state } ;
           attribute_dependencies =
             PMap.add id block.of_category i.current_state.attribute_dependencies } }
  | Ast.DeclareInstance (Ast.Contact, contact, block) ->
    let block =
      convert_block contact [OfCategory] block in
    let (id, state) =
      State.ContactAttribute.declare_attribute
        i.current_state.constructor_information.State.contact contact in
    let id = State.ContactAttribute id in
    if PMap.mem id i.current_state.attribute_dependencies then
      raise (DefinedTwice ("contact", contact)) ;
    { i with current_state =
      { i.current_state with
          constructor_information =
            { i.current_state.constructor_information with State.contact = state } ;
          attribute_dependencies =
            PMap.add id block.of_category i.current_state.attribute_dependencies } }
  | Ast.DeclareConstructor (kind, attribute, constructor, block) ->
    let block =
      convert_block attribute [OfCategory; Translation; Add;
                               CompatibleWith] block in
    (* TODO: Check if the attribute has already been defined.
     * If yes, parse the block now.  Otherwise, put it in a list to be parsed later on. *)
    TODO
  | Ast.DeclareCategory (name, block) ->
    let block =
      convert_block attribute [OfCategory; Translation] block in
    if Utils.Id.get_id i.categories_names name <> None then
      raise (DefinedTwice ("category", name)) ;
    let i =
      { i with categories_names = Utils.Id.map_insert i.categories_names name ;
               categories_to_be_defined =
                 Utils.PSet.remove name i.categories_to_be_defined ;
               category_dependencies =
                 PMap.add name block.of_category i.category_dependencies } in
    (* TODO: Store the translations in a generic type for translations. *)
    TODO
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

