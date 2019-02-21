
let parse_lexbuf fileName lexbuf =
  lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = fileName ;
      Lexing.pos_lnum = 1 ;
      Lexing.pos_bol = 0 ;
      Lexing.pos_cnum = 0
    } ;
  try Parser.main Lexer.read lexbuf with
  | Parser.Error ->
    failwith ("Error: Parser error " ^
              Lexer.current_position lexbuf ^ ".")
  | Lexer.SyntaxError msg ->
    failwith ("Error: Lexer error " ^
              Lexer.current_position lexbuf ^ ": " ^ msg)


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
    category_names : string Utils.Id.map (** All declared category names. **) ;
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
      (State.constructor, Utils.Id.t Utils.PSet.t) PMap.t
      (** Similarly, the dependencies of each constructor. **) ;
    elements_dependencies :
      (Utils.Id.t, Utils.Id.t Utils.PSet.t) PMap.t
      (** Similarly, the dependencies of each element. **) ;
    elements : (Utils.Id.t, Element.t) PMap.t ;
    category_translation : Utils.Id.t Translation.t
      (** All translations for categories. **)
    (* TODO: Translations. *)
  }

type intermediary = {
    current_state : state ; (** The current state. **)
    categories_to_be_defined :
      (Utils.Id.t, Utils.Id.t Utils.PSet.t
                   * State.attribute Utils.PSet.t
                   * State.constructor Utils.PSet.t) PMap.t
      (** The set of categories expected to be declared.
       * For each of these, we also put thee sets to which the category dependencies
       * should be attached: they are respectively the set of category identifiers,
       * of attributes, and of constructors. **) ;
    attributes_to_be_defined :
      (State.attribute, State.constructor Utils.PSet.t) PMap.t
      (** Similarly, the set of attributes expected to be declared and their
       * dependent constructors. **) ;
    waiting_elements : (Utils.Id.t * string * block) list
      (** An element, waiting to be treated. **)
  }

let empty_state = {
    category_names = Utils.Id.map_create () ;
    elements_names = Utils.Id.map_create () ;
    constructor_information = State.empty_constructor_maps ;
    category_dependencies = PMap.empty ;
    attribute_dependencies = PMap.empty ;
    constructor_dependencies = PMap.empty ;
    elements_dependencies = PMap.empty ;
    elements = PMap.empty ;
    category_translation = Translation.empty
  }

let empty_intermediary = {
    current_state = empty_state ;
    categories_to_be_defined = PMap.empty ;
    attributes_to_be_defined = PMap.empty ;
    waiting_elements = []
  }

let categories_to_be_defined i =
  Utils.PSet.map (fun id ->
    match Utils.Id.map_inverse i.current_state.category_names id with
    | Some c -> c
    | None -> assert false) (Utils.PSet.domain i.categories_to_be_defined)

let attributes_to_be_defined i =
  Utils.PSet.partition_map (function
    | State.PlayerAttribute id ->
      (match State.PlayerAttribute.attribute_name
        i.current_state.constructor_information.State.player id with
      | Some c -> Utils.Left c
      | None -> assert false)
    | State.ContactAttribute id ->
      (match State.ContactAttribute.attribute_name
        i.current_state.constructor_information.State.contact id with
      | Some c -> Utils.Right c
      | None -> assert false)) (Utils.PSet.domain i.attributes_to_be_defined)

let is_intermediary_final i =
  PMap.is_empty i.categories_to_be_defined
  && PMap.is_empty i.attributes_to_be_defined

let all_categories i =
  Utils.Id.map_fold (fun _ id l -> id :: l) [] i.category_names

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

exception DefinedTwice of string * string * string option

exception Undeclared of string * string * string option

exception CircularDependency of string

exception SelfRelation of string * string

exception TranslationError of string * string * Ast.language * Ast.language_tag list


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

(** Takes a list of category names and returns a set of category identifiers,
 * as well as the possibly-changed [category_names] field. **)
let category_names_to_id_set state l =
  List.fold_left (fun (category_names, s) name ->
      let (id, category_names) = Utils.Id.map_insert_t category_names name in
      (category_names, Utils.PSet.add id s))
    (state.category_names, Utils.PSet.empty) l

(** Takes a set of categories and returns a set of categories
 * (the original set plus their dependencies).
 * It also takes a boolean [throw] indicating whether it should
 * raise an exception if a category has not been declared yet. **)
let dependencies_of_dependencies throw state s =
  Utils.PSet.merge s
   (Utils.PSet.flatten (Utils.PSet.map (fun id ->
     try PMap.find id state.category_dependencies
     with Not_found ->
       if throw then raise Not_found
       else Utils.PSet.empty) s))

(** A useful composition of [dependencies_of_dependencies] and
 * [category_names_to_id_set]. **)
let category_names_to_dep_dep throw state l =
  let (category_names, s) = category_names_to_id_set state l in
  (category_names, dependencies_of_dependencies throw state s)

 (** Some subfunctions of [prepare_declaration] and [parse_element] use similar
 * functions, only depending on whether called on attributes or contacts.
 * The following tuples store each instantiations of the needed functions. **)
let attribute_functions =
  (State.PlayerAttribute.declare_attribute,
   State.PlayerAttribute.declare_constructor,
   State.PlayerAttribute.get_attribute,
   State.PlayerAttribute.get_constructor,
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
   (fun m -> m.State.contact),
   (fun i state -> { i with State.contact = state }),
   (fun id -> State.ContactAttribute id),
   (fun id -> State.ContactConstructor id),
   "contact")

(** States whether a category has been defined. **)
let category_exists state id =
  PMap.mem id state.category_dependencies

(** States whether an attribute has been defined. **)
let attribute_exists state id =
  PMap.mem id state.attribute_dependencies

(** This function parses basically everything but elements in a declaration. **)
let prepare_declaration i =
  (** Updates the given dependencies [dependencies] by adding the set of
   * categories [deps] to the set [ldeps] of objects. **)
  let update_dependencies dependencies ldeps deps =
    Utils.PSet.fold (fun o dependencies ->
        let s =
          try PMap.find o dependencies
          with Not_found -> assert false in
        PMap.add o (Utils.PSet.merge s deps) dependencies)
      dependencies ldeps in
  (** Updates the field [categories_to_be_defined] by applying a function
    * [update] to each of the values present in [deps].
    * If the mapping doesn’t exists, it will first be initialised with
    * empty sets. **)
  let update_categories_to_be_defined deps update =
    Utils.PSet.fold (fun c categories_to_be_defined ->
      if category_exists i.current_state c then categories_to_be_defined
      else
        let sets =
          try PMap.find c categories_to_be_defined
          with Not_found ->
            (Utils.PSet.empty, Utils.PSet.empty, Utils.PSet.empty) in
        PMap.add c (update sets) categories_to_be_defined)
      i.categories_to_be_defined deps in
  (** Declare attribute and contact instances.
   * See the declarations [attribute_functions] and [contact_functions]
   * to understand the large tuple argument. **)
  let declare_instance (declare, _, _, _, extract, update, constructor, _, en)
      name block =
    let block = convert_block name [OfCategory] block in
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
      with Not_found -> Utils.PSet.empty in
    (** We inform each undefined category that this attribute and its dependencies
     * depends on it. **)
    let categories_to_be_defined =
      update_categories_to_be_defined deps (fun (cats, attrs, constrs) ->
        (cats, Utils.PSet.add id attrs, Utils.PSet.merge constrs constr_deps)) in
    (** We also update each dependent constructors **)
    let constructor_dependencies =
      update_dependencies i.current_state.constructor_dependencies
        constr_deps deps in
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
                PMap.add id deps i.current_state.attribute_dependencies } } in
  (** Declare constructor instances.
   * Similar to [declare_instance], see the declarations [attribute_functions] and
   * [contact_functions] to understand the large tuple argument. **)
  let declare_constructor (declare, declare_constructor, _, _, extract, update,
        attribute_constructor, constructor_constructor, en)
      attribute_name constructor block =
    let block =
      convert_block attribute_name [OfCategory; Translation; Add;
                                    CompatibleWith] block in
    let (attribute, state) =
      declare (extract i.current_state.constructor_information) attribute_name in
    let (id, state) =
      declare_constructor state attribute constructor in
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
      (Utils.PSet.merge deps (PMap.find attribute
                                i.current_state.attribute_dependencies),
       i.attributes_to_be_defined)
    with Not_found ->
      let constrs =
        try PMap.find attribute i.attributes_to_be_defined
        with Not_found -> Utils.PSet.empty in
      (deps, PMap.add attribute (Utils.PSet.add id constrs)
               i.attributes_to_be_defined) in
    (** We inform each undefined category that this attribute and its dependencies
     * depends on it. **)
    let categories_to_be_defined =
      update_categories_to_be_defined deps (fun (cats, attrs, constrs) ->
        (cats, attrs, Utils.PSet.add id constrs)) in
    (* TODO: Deal with [Translation], [Add], and [CompatibleWith]. *)
    { i with
        categories_to_be_defined = categories_to_be_defined ;
        attributes_to_be_defined = attributes_to_be_defined ;
        current_state =
          { i.current_state with
              constructor_information =
                update i.current_state.constructor_information state ;
                constructor_dependencies =
                  PMap.add id deps i.current_state.constructor_dependencies } } in
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
      Utils.Id.map_insert_t category_names name in
    if category_exists i.current_state id then
      raise (DefinedTwice ("category", name, None)) ;
    if Utils.PSet.is_in id deps then
      raise (CircularDependency name) ;
    (** We consider each elements dependent on this category. **)
    let (cat_dep, att_dep, constr_dep) =
      try PMap.find id i.categories_to_be_defined
      with Not_found -> (Utils.PSet.empty, Utils.PSet.empty, Utils.PSet.empty) in
    (** We inform each undefined category that this category and its dependencies
     * depends on it. **)
    let categories_to_be_defined =
      update_categories_to_be_defined deps (fun (cats, attrs, constrs) ->
        (Utils.PSet.add id (Utils.PSet.merge cats cat_dep),
         Utils.PSet.merge attrs att_dep,
         Utils.PSet.merge constrs constr_dep)) in
    let categories_to_be_defined =
      PMap.remove id categories_to_be_defined in
    (** We propagate the local dependencies to all elements that waited to know
     * about them. **)
    let category_dependencies =
      update_dependencies (PMap.add id deps i.current_state.category_dependencies)
        cat_dep deps in
    let attribute_dependencies =
      update_dependencies i.current_state.attribute_dependencies att_dep deps in
    let constructor_dependencies =
      update_dependencies i.current_state.constructor_dependencies
        constr_dep deps in
    let category_translation =
      List.fold_left (fun translation (lg, tags, items) ->
          if tags <> [] then
            raise (TranslationError ("category", name, lg, tags)) ;
          let str =
            String.concat "" (List.map (function
              | Ast.TranslationString (str, []) -> str
              | _ ->
                raise (TranslationError ("category", name, lg, tags))) items) in
          Translation.add translation lg id str)
        i.current_state.category_translation block.translation in
    { i with
        categories_to_be_defined = categories_to_be_defined ;
        current_state =
          { i.current_state with
              category_names = category_names ;
              category_dependencies = category_dependencies ;
              attribute_dependencies = attribute_dependencies ;
              constructor_dependencies = constructor_dependencies ;
              category_translation = category_translation } }
  | Ast.DeclareElement (name, block) ->
    (match Utils.Id.get_id i.current_state.elements_names name with
     | None -> ()
     | Some _ -> raise (DefinedTwice ("element", name, None))) ;
    let (id, elements) =
      Utils.Id.map_insert_t i.current_state.elements_names name in
    let block =
      convert_block name [OfCategory; LetPlayer; ProvideRelation;
                          ProvideAttribute; ProvideContact] block in
    { i with
        waiting_elements = (id, name, block) :: i.waiting_elements ;
        current_state =
          { i.current_state with elements_names = elements } }

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
  (** Before anything, we get the number and names of each declared players. **)
  let n = List.length block.let_player in
  let player_names =
    List.fold_left (fun player_names (name, _) ->
        if Utils.Id.get_id player_names name <> None then
          raise (DefinedTwice ("player", name, Some element_name)) ;
        Utils.Id.map_insert player_names name)
      (Utils.Id.map_create ()) block.let_player in
  let get_player p =
    match Utils.Id.get_id player_names p with
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
            match Utils.Id.map_inverse player_names i with
            | Some name -> name
            | None -> assert false in
          raise (SelfRelation (player_name, element_name))) ;
        let i = Utils.Id.to_array i in
        let j = Utils.Id.to_array j in
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
  let element = Array.map (fun a -> ([], [], a)) relations in
  let (_, deps) =
    try category_names_to_dep_dep true st block.of_category
    with Not_found ->
      try
        let c =
          List.find (fun c ->
            match Utils.Id.get_id st.category_names c with
            | None -> true
            | Some id ->
              not (category_exists st id)) block.of_category in
        raise (Undeclared ("category", c, Some element_name))
      with Not_found -> assert false in
  (** Now that the basic information have been gotten, we can add any constraint
   * using this function with side effects. **)
  let add_constraint p c =
    let p = Utils.Id.to_array p in
    let (cl, el, rs) = element.(p) in
    element.(p) <- (c :: cl, el, rs) in
  (** Retrieve the attribute identifier given by this name.
   * At this stage, it has to be defined.
   * See the declarations [attribute_functions] and [contact_functions] to
   * understand the large tuple argument.**)
  let get_attribute_id (_, _, get_attribute, _, get_state, _, _ , _, en) name =
    match get_attribute (get_state st.constructor_information) name with
    | None -> raise (Undeclared (en, name, Some element_name))
    | Some id -> id in
  (** Similar to [get_attribute_id], but for constructors. **)
  let get_constructor_id (_, _, get_attribute, get_constructor, get_state,
        _, _ , _, en) aid name =
    match get_constructor (get_state st.constructor_information) aid name with
    | None -> raise (Undeclared (en ^ " constructor", name, Some element_name))
    | Some id -> id in
  (** Merges the current dependencies with the ones of the given constructor.
   * Note that the corresponding attribute’s dependencies already have been
   * reported to the constructor: there is no need for an additional merge. **)
  let merge_with_constructor_dependencies deps cid =
    let ndeps =
      try get_constructor_dependencies st cid
      with Not_found -> assert false in
    Utils.PSet.merge deps ndeps in
  (** We now consider attributes. **)
  let deps =
    List.fold_left (fun deps pa ->
        let aid = get_attribute_id attribute_functions pa.Ast.attribute_name in
        let cid =
          get_constructor_id attribute_functions aid pa.Ast.attribute_value in
        add_constraint (get_player pa.Ast.attribute_player)
          (Element.Attribute (aid,
            State.Fixed_value (cid, pa.Ast.attribute_strictness))) ;
        merge_with_constructor_dependencies deps (State.PlayerConstructor cid))
      deps block.provide_attribute in
  (** We then consider contacts. **)
  let deps =
    List.fold_left (fun deps pc ->
        let aid = get_attribute_id contact_functions pc.Ast.contact_name in
        let cid = get_constructor_id contact_functions aid pc.Ast.contact_value in
        let add p1 p2 =
          add_constraint (get_player p1)
            (Element.Contact (aid, Utils.Id.to_array (get_player p2),
              State.Fixed_value (cid, pc.Ast.contact_strictness))) in
        let _ =
          match pc.Ast.contact_destination with
          | Ast.FromTo (p1, p2) -> add p1 p2
          | Ast.Between (p1, p2) -> add p1 p2 ; add p2 p1 in
        merge_with_constructor_dependencies deps (State.ContactConstructor cid))
      deps block.provide_contact in
  (** We finally consider player constraints. **)
  let deps =
    List.fold_left (fun deps (p, pc) ->
      let p = get_player p in
      List.fold_left (fun deps -> function
          | Ast.HasAttribute (a, c) ->
            let aid = get_attribute_id attribute_functions a in
            let cid = get_constructor_id attribute_functions aid c in
            add_constraint p
              (Element.Attribute (aid, State.One_value_of [cid])) ;
            merge_with_constructor_dependencies deps (State.PlayerConstructor cid)
          | Ast.HasContact (a, p', c) ->
            let aid = get_attribute_id contact_functions a in
            let cid = get_constructor_id contact_functions aid c in
            add_constraint p
              (Element.Contact (aid, Utils.Id.to_array (get_player p'),
                State.One_value_of [cid])) ;
            merge_with_constructor_dependencies deps (State.ContactConstructor cid))
        deps pc) deps block.let_player in
  (element, deps)

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

let translates_category s = s.category_translation

let elements s = s.elements

let get_element_dependencies s e =
  PMap.find e s.elements_dependencies

let get_all_elements s cats maxPlayers =
  PMap.foldi (fun e deps el ->
    if Utils.PSet.for_all (fun c -> Utils.PSet.is_in c cats) deps then (
      let et = PMap.find e s.elements in
      if Array.length et <= maxPlayers then
        e :: el
      else el)
    else el) s.elements_dependencies []

