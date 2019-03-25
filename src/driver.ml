
let parse_lexbuf fileName lexbuf =
  lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = fileName ;
      Lexing.pos_lnum = 1 ;
      Lexing.pos_bol = 0 ;
      Lexing.pos_cnum = 0
    } ;
  try Parser.main Lexer.read lexbuf with
  | Parser.Error ->
    failwith ("Error: Parser error " ^ Lexer.current_position lexbuf ^ ".")
  | Lexer.SyntaxError msg ->
    failwith ("Error: Lexer error " ^ Lexer.current_position lexbuf ^ ": " ^ msg)
  | e ->
    failwith ("Error during parsing " ^ Lexer.current_position lexbuf ^ ": "
              ^ Printexc.to_string e)

let parse_relation str =
  let lexbuf = Lexing.from_string str in
  lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = "`" ^ str ^ "'" ;
      Lexing.pos_lnum = 1 ;
      Lexing.pos_bol = 0 ;
      Lexing.pos_cnum = 0
    } ;
  try Parser.relation Lexer.read lexbuf with
  | Parser.Error ->
    failwith ("Error: Parser error " ^ Lexer.current_position lexbuf ^ ".")
  | Lexer.SyntaxError msg ->
    failwith ("Error: Lexer error " ^ Lexer.current_position lexbuf ^ ": " ^ msg)
  | e ->
    failwith ("Error during parsing " ^ Lexer.current_position lexbuf ^ ": "
              ^ Printexc.to_string e)


(** Separates each components of the type [Ast.command] into a separate list. **)
type block = {
    of_category : string list ;
    translation : Ast.translation list ;
    add : Ast.add list ;
    compatible_with : string list ;
    let_player : Ast.let_player list ;
    provide_relation : Ast.provide_relation list ;
    provide_attribute : Ast.provide_attribute list ;
    provide_contact : Ast.provide_contact list ;
    add_difficulty : (bool * string) list ;
    add_complexity : (bool * string) list ;
    event_kind : string list ;
    provide_event : Ast.provide_event list ;
    event_constraint : Ast.event_constraint list
  }

let empty_block = {
    of_category = [] ;
    translation = [] ;
    add = [] ;
    compatible_with = [] ;
    let_player = [] ;
    provide_relation = [] ;
    provide_attribute = [] ;
    provide_contact = [] ;
    add_difficulty = [] ;
    add_complexity = [] ;
    event_kind = [] ;
    provide_event = [] ;
    event_constraint = []
  }

type state = {
    category_names : string Id.map (** All declared category names. **) ;
    event_names : string Id.map (** All declared event names. **) ;
    elements_names : string Id.map (** All declared element names. **) ;
    constructor_information : State.constructor_maps (** The constructor maps. **) ;
    (* LATER: There may be a way to factorise these dependency structures *)
    category_dependencies : (Id.t, Id.t PSet.t) PMap.t
      (** For each category, associates its set of category dependencies.
       * Note that this list is global: it also comprises the dependencies
       * of each dependencies, and so on. **) ;
    attribute_dependencies : (State.attribute, Id.t PSet.t) PMap.t
      (** Similarly, the dependencies of each attribute. **) ;
    constructor_dependencies : (State.constructor, Id.t PSet.t) PMap.t
      (** Similarly, the dependencies of each constructor. **) ;
    elements_dependencies : (Id.t, Id.t PSet.t) PMap.t
      (** Similarly, the dependencies of each element. **) ;
    event_dependencies : (Id.t, Id.t PSet.t) PMap.t
      (** Similarly, the (category) dependencies of each events. **) ;
    event_event_dependencies : (Id.t, Id.t PSet.t) PMap.t
      (** Events introduce a second level of graphs, as events also
       * depend on other events.
       * This is what this map stores. **) ;
    elements : (Id.t, Element.t) PMap.t ;
    translations : Translation.element
  }

type intermediary = {
    current_state : state ; (** The current state. **)
    categories_to_be_defined :
      (Id.t, Id.t PSet.t
                   * Id.t PSet.t
                   * State.attribute PSet.t
                   * State.constructor PSet.t) PMap.t
      (** The set of categories expected to be declared.
       * For each of these, we also put thee sets to which the category dependencies
       * should be attached: they are respectively the set of category identifiers,
       * of events, of attributes, and of constructors. **) ;
    events_to_be_defined : (Id.t, Id.t PSet.t) PMap.t
      (** Similarly, the set of events to be declared.
       * For each event, we also associate the other events depending on it. **) ;
    attributes_to_be_defined :
      (State.attribute, State.constructor PSet.t) PMap.t
      (** Similarly, the set of attributes expected to be declared and their
       * dependent constructors. **) ;
    constructors_to_be_defined : State.constructor PSet.t
      (** A set of constructors expected to be defined. **) ;
    tags_to_be_defined : (Translation.language * Translation.tag) PSet.t
      (** Similarly, a set of tags expected to be defined. **) ;
    declared_tags : (Translation.language * Translation.tag) PSet.t
      (** In contrary to categories, attributes, and constructors,
       * tags are not associated any information.
       * We thus need to explicitely track which were defined. **) ;
    waiting_elements : (Id.t * string * block) list
      (** An element, waiting to be treated. **)
  }

let empty_state = {
    category_names = Id.map_create () ;
    event_names = Id.map_create () ;
    elements_names = Id.map_create () ;
    constructor_information = State.empty_constructor_maps ;
    category_dependencies = PMap.empty ;
    attribute_dependencies = PMap.empty ;
    constructor_dependencies = PMap.empty ;
    elements_dependencies = PMap.empty ;
    event_dependencies = PMap.empty ;
    event_event_dependencies = PMap.empty ;
    elements = PMap.empty ;
    translations = Translation.empty_element
  }

let empty_intermediary = {
    current_state = empty_state ;
    categories_to_be_defined = PMap.empty ;
    events_to_be_defined = PMap.empty ;
    attributes_to_be_defined = PMap.empty ;
    constructors_to_be_defined = PSet.empty ;
    tags_to_be_defined = PSet.empty ;
    declared_tags = PSet.empty ;
    waiting_elements = []
  }

let categories_to_be_defined i =
  PSet.map (fun id ->
      Utils.assert_option __LOC__
        (Id.map_inverse i.current_state.category_names id))
    (PSet.domain i.categories_to_be_defined)

let events_to_be_defined i =
  PSet.map (fun id ->
      Utils.assert_option __LOC__
        (Id.map_inverse i.current_state.event_names id))
    (PSet.domain i.events_to_be_defined)

let attributes_to_be_defined i =
  PSet.partition_map (function
      | State.PlayerAttribute id ->
        Utils.Left (Utils.assert_option __LOC__
          (State.PlayerAttribute.attribute_name
            i.current_state.constructor_information.State.player id))
      | State.ContactAttribute id ->
        Utils.Right (Utils.assert_option __LOC__
          (State.ContactAttribute.attribute_name
            i.current_state.constructor_information.State.contact id)))
    (PSet.domain i.attributes_to_be_defined)

let constructors_to_be_defined i =
  PSet.partition_map (function
      | State.PlayerConstructor c ->
        let i = i.current_state.constructor_information.State.player in
        let a =
          Utils.assert_option __LOC__
            (State.PlayerAttribute.constructor_attribute i c) in
        let a =
          Utils.assert_option __LOC__
            (State.PlayerAttribute.attribute_name i a) in
        let c =
          Utils.assert_option __LOC__
            (State.PlayerAttribute.constructor_name i c) in
        Utils.Left (a, c)
      | State.ContactConstructor c ->
        let i = i.current_state.constructor_information.State.contact in
        let a =
          Utils.assert_option __LOC__
            (State.ContactAttribute.constructor_attribute i c) in
        let a =
          Utils.assert_option __LOC__
            (State.ContactAttribute.attribute_name i a) in
        let c =
          Utils.assert_option __LOC__
            (State.ContactAttribute.constructor_name i c) in
        Utils.Right (a, c)) i.constructors_to_be_defined

let tags_to_be_defined i =
  PSet.map (fun (lg, tag) ->
    (Translation.iso639 lg, Translation.print_tag tag)) i.tags_to_be_defined

let is_intermediary_final i =
  PMap.is_empty i.categories_to_be_defined
  && PMap.is_empty i.events_to_be_defined
  && PMap.is_empty i.attributes_to_be_defined
  && PSet.is_empty i.constructors_to_be_defined
  && PSet.is_empty i.tags_to_be_defined

let all_categories i =
  Id.map_fold (fun _ id l -> id :: l) [] i.category_names

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
  | AddDifficulty
  | AddComplexity
  | EventKind
  | ProvideEvent
  | EventConstraint

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
  | AddDifficulty -> "difficulty provision"
  | AddComplexity -> "complexity provision"
  | EventKind -> "event kind declaration"
  | ProvideEvent -> "event provision"
  | EventConstraint -> "event constraint"

exception UnexpectedCommandInBlock of string * string

exception DefinedTwice of string * string * string option

exception Undeclared of string * string * string option

exception CircularDependency of string * string

exception SelfRelation of string * string

exception TranslationError of string * string * Ast.translation

exception VacuumElement of string


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
        { acc with provide_contact = p :: acc.provide_contact }
      | Ast.AddDifficulty (d, p) ->
        check AddDifficulty ;
        let l = List.map (fun p -> (d, p)) p in
        { acc with add_difficulty = l @ acc.add_difficulty }
      | Ast.AddComplexity (d, p) ->
        check AddComplexity ;
        let l = List.map (fun p -> (d, p)) p in
        { acc with add_complexity = l @ acc.add_complexity }
      | Ast.EventKind kind ->
        check EventKind ;
        { acc with event_kind = kind :: acc.event_kind }
      | Ast.ProvideEvent p ->
        check ProvideEvent ;
        { acc with provide_event = p :: acc.provide_event }
      | Ast.EventConstraint p ->
        check EventConstraint ;
        { acc with event_constraint = p :: acc.event_constraint }) l
  in aux empty_block

(** Takes a list of category names and returns a set of category identifiers,
 * as well as the possibly-changed [category_names] field. **)
let category_names_to_id_set state l =
  List.fold_left (fun (category_names, s) name ->
      let (id, category_names) = Id.map_insert_t category_names name in
      (category_names, PSet.add id s))
    (state.category_names, PSet.empty) l

(** Takes a set of categories and returns a set of categories
 * (the original set plus their dependencies).
 * It also takes a boolean [throw] indicating whether it should
 * raise an exception if a category has not been declared yet. **)
let dependencies_of_dependencies throw state s =
  PSet.merge s
   (PSet.flatten (PSet.map (fun id ->
     try PMap.find id state.category_dependencies
     with Not_found ->
       if throw then raise Not_found
       else PSet.empty) s))

(** A useful composition of [dependencies_of_dependencies] and
 * [category_names_to_id_set]. **)
let category_names_to_dep_dep throw state l =
  let (category_names, s) = category_names_to_id_set state l in
  (category_names, dependencies_of_dependencies throw state s)

(** The corresponding three functions, but for the event graph. **)

let event_names_to_id_set state l =
  List.fold_left (fun (event_names, s) name ->
      let (id, event_names) = Id.map_insert_t event_names name in
      (event_names, PSet.add id s))
    (state.event_names, PSet.empty) l

let event_dependencies_of_dependencies state s =
  PSet.merge s
   (PSet.flatten (PSet.map (fun id ->
     try PMap.find id state.event_event_dependencies
     with Not_found -> PSet.empty) s))

let event_names_to_dep_dep state l =
  let (event_names, s) = event_names_to_id_set state l in
  (event_names, event_dependencies_of_dependencies state s)

 (** Some subfunctions of [prepare_declaration] and [parse_element] use similar
 * functions, only depending on whether called on attributes or contacts.
 * The following tuples store each instantiations of the needed functions. **)
let attribute_functions =
  (State.PlayerAttribute.declare_attribute,
   State.PlayerAttribute.declare_constructor,
   State.PlayerAttribute.get_attribute,
   State.PlayerAttribute.get_constructor,
   State.PlayerAttribute.declare_compatibility,
   (fun m -> m.State.player),
   (fun i state -> { i with State.player = state }),
   (fun id -> State.PlayerAttribute id),
   (fun id -> State.PlayerConstructor id),
   "attribute")
let contact_functions =
  (State.ContactAttribute.declare_attribute,
   State.ContactAttribute.declare_constructor,
   State.ContactAttribute.get_attribute,
   State.ContactAttribute.get_constructor,
   State.ContactAttribute.declare_compatibility,
   (fun m -> m.State.contact),
   (fun i state -> { i with State.contact = state }),
   (fun id -> State.ContactAttribute id),
   (fun id -> State.ContactConstructor id),
   "contact")

(** States whether a category has been defined. **)
let category_exists state id =
  PMap.mem id state.category_dependencies

(** States whether an event has been defined. **)
let event_exists state id =
  PMap.mem id state.event_dependencies

(** States whether an attribute has been defined. **)
let attribute_exists state id =
  PMap.mem id state.attribute_dependencies

(** This function parses basically everything but elements in a declaration. **)
let prepare_declaration i =
  (** Updates the given dependencies [dependencies] by adding the set of
   * categories [deps] to the set [ldeps] of items.
   * Each [ldeps] have thus already been declared when calling this function,
   * just that due to missing dependencies, their dependencies need to be extended
   * by [deps]. **)
  let update_dependencies dependencies ldeps deps =
    PSet.fold (fun o dependencies ->
        let s =
          try PMap.find o dependencies
          with Not_found -> assert false in
        PMap.add o (PSet.merge s deps) dependencies)
      dependencies ldeps in
  (** Updates the field [categories_to_be_defined] by applying a function
    * [update] to each of the values present in [deps].
    * If the mapping doesn’t exists, it will first be initialised with
    * empty sets. **)
  let update_categories_to_be_defined deps update =
    PSet.fold (fun c categories_to_be_defined ->
      if category_exists i.current_state c then categories_to_be_defined
      else
        let sets =
          try PMap.find c categories_to_be_defined
          with Not_found ->
            (PSet.empty, PSet.empty,
             PSet.empty, PSet.empty) in
        PMap.add c (update sets) categories_to_be_defined)
      i.categories_to_be_defined deps in
  (** Similar to [update_categories_to_be_defined], but for the event-dependency
   * gtaph. **)
  let update_events_to_be_defined deps update =
    PSet.fold (fun ev events_to_be_defined ->
      if event_exists i.current_state ev then events_to_be_defined
      else
        let sets =
          try PMap.find ev events_to_be_defined
          with Not_found -> PSet.empty in
        PMap.add ev (update sets) events_to_be_defined)
      i.events_to_be_defined deps in
  (** Declare attribute and contact instances.
   * See the declarations [attribute_functions] and [contact_functions]
   * to understand the large tuple argument. **)
  let declare_instance (declare, _, _, _, _, extract, update, constructor, _, en)
      name block =
    let block = convert_block name [OfCategory; Translation] block in
    let (id, state) =
      declare (extract i.current_state.constructor_information) name in
    let id = constructor id in
    if attribute_exists i.current_state id then
      raise (DefinedTwice (en, name, None)) ;
    let (category_names, deps) =
      category_names_to_dep_dep false i.current_state block.of_category in
    (** We consider each constructor dependent on this attribute. **)
    let constr_deps =
      try PMap.find id i.attributes_to_be_defined
      with Not_found -> PSet.empty in
    (** We inform each undefined category that this attribute and its dependencies
     * depends on it. **)
    let categories_to_be_defined =
      update_categories_to_be_defined deps (fun (cats, events, attrs, constrs) ->
        (cats, events,
         PSet.add id attrs, PSet.merge constrs constr_deps)) in
    (** We also update each constructors depending on this instance. **)
    let constructor_dependencies =
      update_dependencies i.current_state.constructor_dependencies
        constr_deps deps in
    let translations =
      let translations =
        Translation.add i.current_state.translations.Translation.attribute
          Translation.generic id name in
      List.fold_left (fun translations tr ->
        let (lg, tags, items) = tr in
        if tags <> [] then
          raise (TranslationError (en, name, tr)) ;
        let str =
          String.concat "" (List.map (function
            | Translation.Direct str -> str
            | _ ->
              raise (TranslationError (en, name, tr))) items) in
        Translation.add translations lg id str) translations block.translation in
    { i with
        categories_to_be_defined = categories_to_be_defined ;
        attributes_to_be_defined = PMap.remove id i.attributes_to_be_defined ;
        current_state =
          { i.current_state with
              constructor_information =
                update i.current_state.constructor_information state ;
              category_names = category_names ;
              constructor_dependencies = constructor_dependencies ;
              attribute_dependencies =
                PMap.add id deps i.current_state.attribute_dependencies ;
              translations =
                { i.current_state.translations with Translation.attribute =
                    translations } } } in
  (** Declare constructor instances.
   * Similar to [declare_instance], see the declarations [attribute_functions] and
   * [contact_functions] to understand the large tuple argument. **)
  let declare_constructor (declare, declare_constructor, _, _,
        declare_compatibility, extract, update,
        attribute_constructor, constructor_constructor, en)
      attribute_name constructor block =
    let block =
      convert_block attribute_name [OfCategory; Translation; Add;
                                    CompatibleWith] block in
    let (attribute, state) =
      declare (extract i.current_state.constructor_information) attribute_name in
    let (id, state) =
      declare_constructor state attribute constructor in
    let constructors_to_be_defined =
      let id = constructor_constructor id in
      PSet.remove id i.constructors_to_be_defined in
    let (state, constructors_to_be_defined) =
      List.fold_left (fun (state, constructors_to_be_defined) constructor' ->
          let (id', state) = declare_constructor state attribute constructor' in
          let constructors_to_be_defined =
            let id' = constructor_constructor id' in
            if PMap.mem id' i.current_state.constructor_dependencies then
              constructors_to_be_defined
            else PSet.add id' constructors_to_be_defined in
          (declare_compatibility state attribute id id', constructors_to_be_defined))
        (state, constructors_to_be_defined) block.compatible_with in
    let id = constructor_constructor id in
    let attribute = attribute_constructor attribute in
    if PMap.mem id i.current_state.constructor_dependencies then
      raise (DefinedTwice (en ^ " constructor", constructor, Some attribute_name)) ;
    let (category_names, deps) =
      category_names_to_dep_dep false i.current_state block.of_category in
    (** If the associated attribute is already defined, we fetch its dependencies,
     * otherwise, we leave a note for it to add these dependencies when finally
     * defined. **)
    let (deps, attributes_to_be_defined) =
    try
      (PSet.merge deps
         (PMap.find attribute i.current_state.attribute_dependencies),
       i.attributes_to_be_defined)
    with Not_found ->
      let constrs =
        try PMap.find attribute i.attributes_to_be_defined
        with Not_found -> PSet.empty in
      (deps, PMap.add attribute (PSet.add id constrs)
               i.attributes_to_be_defined) in
    (** We inform each undefined category that this attribute and its dependencies
     * depends on it. **)
    let categories_to_be_defined =
      update_categories_to_be_defined deps (fun (cats, events, attrs, constrs) ->
        (cats, events, attrs, PSet.add id constrs)) in
    let (translations, tags_to_be_defined) =
      let translations =
        Translation.gadd i.current_state.translations.Translation.constructor
          Translation.generic [] id constructor in
      List.fold_left (fun (translations, tags_to_be_defined) tr ->
          let (lg, tags, items) = tr in
          let str =
            String.concat "" (List.map (function
              | Translation.Direct str -> str
              | _ ->
                raise (TranslationError (en, constructor, tr))) items) in
          try (Translation.gadd translations lg tags id str,
               List.fold_left (fun tags_to_be_defined (_, tag) ->
                   if PSet.mem (lg, tag) i.declared_tags then
                     tags_to_be_defined
                   else PSet.add (lg, tag) tags_to_be_defined)
                 tags_to_be_defined tags)
          with Translation.ConflictingCommands _ ->
            raise (TranslationError (en, constructor, tr)))
        (translations, i.tags_to_be_defined) block.translation in
    let add =
      List.fold_left (fun add (lg, tag) ->
          let s =
            try PMap.find (id, lg) add
            with Not_found -> PSet.empty in
          PMap.add (id, lg) (PSet.add tag s) add)
        i.current_state.translations.Translation.add block.add in
    { i with
        categories_to_be_defined = categories_to_be_defined ;
        attributes_to_be_defined = attributes_to_be_defined ;
        constructors_to_be_defined = constructors_to_be_defined ;
        tags_to_be_defined = tags_to_be_defined ;
        current_state =
          { i.current_state with
              constructor_information =
                update i.current_state.constructor_information state ;
              constructor_dependencies =
                PMap.add id deps i.current_state.constructor_dependencies ;
              translations =
                { i.current_state.translations with
                    Translation.constructor = translations ;
                    Translation.add = add } } } in
  function
  | Ast.DeclareInstance (Ast.Attribute, attribute, block) ->
    declare_instance attribute_functions attribute block
  | Ast.DeclareInstance (Ast.Contact, contact, block) ->
    declare_instance contact_functions contact block
  | Ast.DeclareConstructor (Ast.Attribute, attribute, constructor, block) ->
    declare_constructor attribute_functions attribute constructor block
  | Ast.DeclareConstructor (Ast.Contact, attribute, constructor, block) ->
    declare_constructor contact_functions attribute constructor block
  | Ast.DeclareCategory (name, block) ->
    let block =
      convert_block name [OfCategory; Translation] block in
    let (category_names, deps) =
      category_names_to_dep_dep false i.current_state block.of_category in
    let (id, category_names) =
      Id.map_insert_t category_names name in
    if category_exists i.current_state id then
      raise (DefinedTwice ("category", name, None)) ;
    if PSet.mem id deps then
      raise (CircularDependency ("category", name)) ;
    (** We consider each elements dependent on this category. **)
    let (cat_dep, event_dep, att_dep, constr_dep) =
      try PMap.find id i.categories_to_be_defined
      with Not_found ->
        (PSet.empty, PSet.empty, PSet.empty, PSet.empty) in
    (** We inform each undefined category that this category and its dependencies
     * depends on it. **)
    let categories_to_be_defined =
      update_categories_to_be_defined deps (fun (cats, events, attrs, constrs) ->
        (PSet.add id (PSet.merge cats cat_dep),
         PSet.merge events event_dep,
         PSet.merge attrs att_dep,
         PSet.merge constrs constr_dep)) in
    let categories_to_be_defined =
      PMap.remove id categories_to_be_defined in
    (** We propagate the local dependencies to all items that waited to know
     * about them. **)
    let category_dependencies =
      update_dependencies (PMap.add id deps i.current_state.category_dependencies)
        cat_dep deps in
    let event_dependencies =
      update_dependencies i.current_state.event_dependencies event_dep deps in
    let attribute_dependencies =
      update_dependencies i.current_state.attribute_dependencies att_dep deps in
    let constructor_dependencies =
      update_dependencies i.current_state.constructor_dependencies
        constr_dep deps in
    let category_translation =
      List.fold_left (fun translation tr ->
          let (lg, tags, items) = tr in
          if tags <> [] then
            raise (TranslationError ("category", name, tr)) ;
          let str =
            String.concat "" (List.map (function
              | Translation.Direct str -> str
              | _ ->
                raise (TranslationError ("category", name, tr))) items) in
          Translation.add translation lg id str)
        (Translation.add i.current_state.translations.Translation.category
          Translation.generic id name)
        block.translation in
    { i with
        categories_to_be_defined = categories_to_be_defined ;
        current_state =
          { i.current_state with
              category_names = category_names ;
              category_dependencies = category_dependencies ;
              event_dependencies = event_dependencies ;
              attribute_dependencies = attribute_dependencies ;
              constructor_dependencies = constructor_dependencies ;
              translations =
                { i.current_state.translations with Translation.category =
                    category_translation } } }
  | Ast.DeclareElement (name, block) ->
    (match Id.get_id i.current_state.elements_names name with
     | None -> ()
     | Some _ -> raise (DefinedTwice ("element", name, None))) ;
    let (id, elements) =
      Id.map_insert_t i.current_state.elements_names name in
    let block =
      convert_block name [OfCategory; LetPlayer; ProvideRelation;
                          ProvideAttribute; ProvideContact;
                          AddDifficulty; AddComplexity;
                          ProvideEvent] block in
    { i with
        waiting_elements = (id, name, block) :: i.waiting_elements ;
        current_state =
          { i.current_state with elements_names = elements } }
  | Ast.DeclareCase (lang, tag) ->
    if PSet.mem (lang, tag) i.declared_tags then
      raise (DefinedTwice ("tag", Translation.print_tag tag,
                           Some (Translation.iso639 lang))) ;
    { i with
        declared_tags = PSet.add (lang, tag) i.declared_tags ;
        tags_to_be_defined = PSet.remove (lang, tag) i.tags_to_be_defined }
  | Ast.DeclareEventKind (kind, block) ->
    let block =
      convert_block kind [OfCategory; EventKind] block in
    let (category_names, deps) =
      category_names_to_dep_dep false i.current_state block.of_category in
    let (event_names, event_deps) =
      event_names_to_dep_dep i.current_state block.event_kind in
    let (id, event_names) =
      Id.map_insert_t event_names kind in
    if event_exists i.current_state id then
      raise (DefinedTwice ("event", kind, None)) ;
    if PSet.mem id event_deps then
      raise (CircularDependency ("event", kind)) ;
    let event_dep =
      try PMap.find id i.events_to_be_defined
      with Not_found -> PSet.empty in
    (** We inform each relevant undefined category and event that this event
     * and its dependencies depends on them. **)
    let categories_to_be_defined =
      update_categories_to_be_defined deps (fun (cats, events, attrs, constrs) ->
        (cats, PSet.add id events, attrs, constrs)) in
    let events_to_be_defined =
      update_events_to_be_defined event_deps (fun events ->
        PSet.add id (PSet.merge events event_dep)) in
    let events_to_be_defined =
      PMap.remove id events_to_be_defined in
    (** We propagate the local dependencies (in both graphs) to all events that
     * waited to know about them. **)
    let event_dependencies =
      update_dependencies
        (PMap.add id deps i.current_state.event_dependencies) event_dep deps in
    let event_event_dependencies =
      update_dependencies
        (PMap.add id event_deps i.current_state.event_event_dependencies)
        event_dep event_deps in
    { i with
        categories_to_be_defined = categories_to_be_defined ;
        events_to_be_defined = events_to_be_defined ;
        current_state =
          { i.current_state with
              category_names = category_names ;
              event_names = event_names ;
              event_dependencies = event_dependencies ;
              event_event_dependencies = event_event_dependencies } }

let prepare_declarations i l =
  List.fold_left prepare_declaration i l

let get_category_dependencies s id =
  PMap.find id s.category_dependencies

let get_attribute_dependencies s id =
  PMap.find id s.attribute_dependencies

let get_constructor_dependencies s id =
  PMap.find id s.constructor_dependencies

(** Parses generates an element from a [state] and a [block]. **)
let parse_element st element_name block =
  let get_constructor_dependencies cid =
    try get_constructor_dependencies st cid
    with Not_found -> assert false in
  (** Before anything, we get the number and names of each declared players. **)
  let n = List.length block.let_player in
  let player_names =
    List.fold_left (fun player_names name ->
        if Id.get_id player_names name <> None then
          raise (DefinedTwice ("player", name, Some element_name)) ;
        Id.map_insert player_names name)
      (Id.map_create ())
      (Utils.list_map_filter fst block.let_player) in
  let get_player p =
    match Id.get_id player_names p with
    | None -> raise (Undeclared ("player", p, Some element_name))
    | Some i -> i in
  (** We first consider relations. **)
  let relations =
    (** We create this triangle of relations.
     * Each index is associated with its greatest non-[neutral] index plus one,
     * or [0] is the entire array is filled with [neutral]. **)
    let triangle =
      Array.init n (fun i -> (Array.make i Relation.neutral, 0)) in
    let write i j r =
      if r <> Relation.neutral then (
        if i = j then (
          let player_name =
            Utils.assert_option __LOC__ (Id.map_inverse player_names i) in
          raise (SelfRelation (player_name, element_name))) ;
        let i = Id.to_array i in
        let j = Id.to_array j in
        let (i, j, r) =
          if i < j then (i, j, r)
          else (j, i, Relation.reverse r) in
        let (a, m) = triangle.(j) in
        a.(i) <- Relation.compose a.(i) r ;
        triangle.(j) <- (a, max m (i + 1))) in
    List.iter (fun (td, r) ->
      match td with
      | Ast.Between (p1, p2) ->
        write (get_player p1) (get_player p2) r
      | Ast.FromTo (p1, p2) ->
        write (get_player p1) (get_player p2)
          (Relation.asymmetrical r Relation.neutral)) block.provide_relation ;
    (** We now try to minimize each array.
     * Note that we could try to change player identifiers to optimize
     * space even more, but this might take additional resources needlessly. **)
    Array.map (fun (a, m) -> Array.sub a 0 m) triangle in
  (** We define our current element and constraints over other players,
   * and we will update them by considering each declaration. **)
  let elementBase =
    Array.map (fun a -> {
        Element.constraints = [] ;
        Element.events = [] ;
        Element.relations = a ;
        Element.added_objective = State.zero_objective
      }) relations in
  let otherPlayers = ref [] in
  let (_, deps) =
    try category_names_to_dep_dep true st block.of_category
    with Not_found ->
      try
        let c =
          List.find (fun c ->
            match Id.get_id st.category_names c with
            | None -> true
            | Some id ->
              not (category_exists st id)) block.of_category in
        raise (Undeclared ("category", c, Some element_name))
      with Not_found -> assert false in
  (** Now that the basic information have been gotten, we can add any constraint
   * using this function with side effects. **)
  let add_constraint p c =
    let p = Id.to_array p in
    elementBase.(p) <-
      { elementBase.(p) with Element.constraints =
                               c :: elementBase.(p).Element.constraints } in
  let add_constraint_other c =
    otherPlayers := c :: !otherPlayers in
  let add_constraint_all c =
    (** Exploding [Ast.AllPlayers] into a list of [Ast.DestinationPlayer] and
     * [Ast.AllOtherPlayers] tends to make non-acceptable combinations appear.
     * The [ok] function filters these out. **)
    let ok p =
      match c with
      | Element.Contact (_, Some p', _) -> p <> p'
      | _ -> true in
    List.iter (fun p ->
      if ok p then
        add_constraint (Id.from_array p) c) (Utils.seq n) ;
    add_constraint_other c in
  (** Retrieve the attribute identifier given by this name.
   * At this stage, it has to be defined.
   * See the declarations [attribute_functions] and [contact_functions] to
   * understand the large tuple argument.**)
  let get_attribute_id (_, _, get_attribute, _, _, get_state, _, _ , _, en) name =
    match get_attribute (get_state st.constructor_information) name with
    | None -> raise (Undeclared (en, name, Some element_name))
    | Some id -> id in
  (** Similar to [get_attribute_id], but for constructors. **)
  let get_constructor_id (_, _, get_attribute, get_constructor, _, get_state,
        _, _ , _, en) aid name =
    match get_constructor (get_state st.constructor_information) aid name with
    | None -> raise (Undeclared (en ^ " constructor", name, Some element_name))
    | Some id -> id in
  (** Merges the current dependencies with the ones of the given constructor.
   * Note that the corresponding attribute’s dependencies already have been
   * reported to the constructor: there is no need for an additional merge. **)
  let merge_with_constructor_dependencies deps cid =
    PSet.merge deps (get_constructor_dependencies cid) in
  (** An alternative to [merge_with_constructor_dependencies] when given a list. **)
  let merge_with_constructor_dependencies_list deps =
    List.fold_left (fun deps cid ->
      merge_with_constructor_dependencies deps cid) deps in
  (** There are places where instead of wanting the union of all dependencies, we
   * only want to perform their intersection (typically, if an element requires
   * one of several conditions, this only adds the intersection of the category
   * dependencies of each conditions to the element. **)
  let intersect_with_constructor_dependencies deps cid =
    PSet.inter deps (get_constructor_dependencies cid) in
  (** An alternative to [intersect_with_constructor_dependencies] when given a
   * list.
   * We however still want to merge this intersection with the old dependencies. **)
  let intersect_with_constructor_dependencies_list deps = function
    | [] -> raise (VacuumElement element_name)
    | cid :: cidl ->
      PSet.merge deps
        (List.fold_left (fun deps cid ->
            intersect_with_constructor_dependencies deps cid)
          (get_constructor_dependencies cid) cidl) in
  (** We now consider added difficulty and complexity. **)
  List.iter (fun (d, p) ->
      let d = if d then 1 else -1 in
      let p = Id.to_array (get_player p) in
      let o = elementBase.(p).Element.added_objective in
      elementBase.(p) <-
        { elementBase.(p) with
            Element.added_objective =
              { o with State.difficulty = d + o.State.difficulty } })
    block.add_difficulty ;
  List.iter (fun (d, p) ->
      let d = if d then 1 else -1 in
      let p = Id.to_array (get_player p) in
      let o = elementBase.(p).Element.added_objective in
      elementBase.(p) <-
        { elementBase.(p) with
            Element.added_objective =
              { o with State.complexity = d + o.State.complexity } })
    block.add_complexity ;
  (** We now consider attributes. **)
  let deps =
    List.fold_left (fun deps pa ->
        let aid = get_attribute_id attribute_functions pa.Ast.attribute_name in
        let cid =
          List.map (get_constructor_id attribute_functions aid)
            pa.Ast.attribute_value in
        let c =
          Element.Attribute (aid,
            State.Fixed_value (cid, pa.Ast.attribute_strictness)) in
        let _ =
          match pa.Ast.attribute_player with
          | Ast.DestinationPlayer p -> add_constraint (get_player p) c
          | Ast.AllOtherPlayers -> add_constraint_other c
          | Ast.AllPlayers -> add_constraint_all c in
        merge_with_constructor_dependencies_list deps
          (List.map (fun id -> State.PlayerConstructor id) cid))
      deps block.provide_attribute in
  (** We then consider contacts. **)
  let deps =
    List.fold_left (fun deps pc ->
        let aid = get_attribute_id contact_functions pc.Ast.contact_name in
        let cid =
          List.map (get_constructor_id contact_functions aid)
            pc.Ast.contact_value in
        let c p2 =
          Element.Contact (aid, Option.map (fun p ->
              Id.to_array p) p2,
            State.Fixed_value (cid, pc.Ast.contact_strictness)) in
        (** Call the function [add] on all requested destinations,
         * except [p1] if there.
         * No error is thrown: the goal is only to avoid two [any player]
         * commands used together to yield any conflict. **)
        let add_except p1 add =
          let add_single p2 =
            if p1 <> Some p2 then add (Some p2) in function
          | Ast.DestinationPlayer p2 -> add_single (get_player p2)
          | Ast.AllOtherPlayers -> add None
          | Ast.AllPlayers ->
            List.iter add_single (List.map Id.from_array (Utils.seq n)) ;
            add None in
        let add = function
          | Ast.DestinationPlayer p1n ->
            let p1 = get_player p1n in
            add_except None (fun p2 ->
              if p2 = Some p1 then
                raise (SelfRelation (p1n, element_name)) ;
              add_constraint p1 (c p2))
          | Ast.AllOtherPlayers ->
            add_except None (fun p2 -> add_constraint_other (c p2))
          | Ast.AllPlayers -> fun p2 ->
            List.iter (fun p1 ->
                let p1 = Id.from_array p1 in
                add_except (Some p1) (fun p2 -> add_constraint p1 (c p2)) p2)
              (Utils.seq n) ;
            add_except None (fun p2 -> add_constraint_other (c p2)) p2 in
        let _ =
          match pc.Ast.contact_destination with
          | Ast.FromTo (p1, p2) -> add p1 p2
          | Ast.Between (p1, p2) -> add p1 p2 ; add p2 p1 in
        merge_with_constructor_dependencies_list deps
          (List.map (fun id -> State.ContactConstructor id) cid))
      deps block.provide_contact in
  (** We finally consider player constraints. **)
  let deps =
    List.fold_left (fun deps (p, pc) ->
        let consider_constraints add_constraint =
          List.fold_left (fun deps -> function
            | Ast.HasAttribute (a, n, c) ->
              let aid = get_attribute_id attribute_functions a in
              let cid = List.map (get_constructor_id attribute_functions aid) c in
              let cid =
                if n then (
                  let total =
                    Utils.assert_option __LOC__
                      (State.PlayerAttribute.constructors
                        st.constructor_information.player aid) in
                  List.filter (fun c -> not (List.mem c cid)) total
                ) else cid in
              add_constraint (Element.Attribute (aid, State.One_value_of cid)) ;
              intersect_with_constructor_dependencies_list deps
                (List.map (fun id -> State.PlayerConstructor id) cid)
            | Ast.HasContact (a, p', n, c) ->
              let aid = get_attribute_id contact_functions a in
              let cid = List.map (get_constructor_id contact_functions aid) c in
              let cid =
                if n then (
                  let total =
                    Utils.assert_option __LOC__
                      (State.ContactAttribute.constructors
                        st.constructor_information.contact aid) in
                  List.filter (fun c -> not (List.mem c cid)) total
                ) else cid in
              add_constraint
                (Element.Contact (aid, Some (Id.to_array (get_player p')),
                  State.One_value_of cid)) ;
              intersect_with_constructor_dependencies_list deps
                (List.map (fun id -> State.ContactConstructor id) cid)) deps pc in
        match p with
        | None -> consider_constraints add_constraint_other
        | Some p ->
          consider_constraints (add_constraint (get_player p)))
      deps block.let_player in
  (* TODO: events *)
  ((elementBase, !otherPlayers), deps)

let parse i =
  let (elements, elements_dependencies) =
    List.fold_left (fun (elements, elements_dependencies) (id, name, block) ->
        let (element, deps) = parse_element i.current_state name block in
        (PMap.add id element elements, PMap.add id deps elements_dependencies))
      (i.current_state.elements, i.current_state.elements_dependencies)
      i.waiting_elements in
  { i.current_state with
      elements_dependencies = elements_dependencies ;
      elements = elements }

let get_translations s = s.translations

let get_constructor_maps s = s.constructor_information

let elements s = s.elements

let get_element_dependencies s e =
  PMap.find e s.elements_dependencies

let get_all_elements s cats maxPlayers =
  PMap.foldi (fun e deps el ->
    if PSet.for_all (fun c -> PSet.mem c cats) deps then (
      let (et, _) = PMap.find e s.elements in
      if Array.length et <= maxPlayers then
        e :: el
      else el)
    else el) s.elements_dependencies []

