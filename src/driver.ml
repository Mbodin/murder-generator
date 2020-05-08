
open Libutils

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


(** Separates each components of the type [Ast.command] into a separate list. *)
type block = {
    of_category : string list ;
    translation : Ast.translation list ;
    description : Ast.description list ;
    sentence : Ast.block list ;
    add : Ast.add list ;
    compatible_with : string list ;
    let_player : Ast.let_player list ;
    let_object : Ast.let_object list ;
    provide_relation : Ast.provide_relation list ;
    provide_attribute : Ast.provide_attribute list ;
    provide_contact : Ast.provide_contact list ;
    add_difficulty : (bool * string) list ;
    add_complexity : (bool * string) list ;
    event_kind : string list ;
    provide_event : Ast.provide_event list ;
    event_constraint : Ast.event_constraint list ;
    attribute_of : Ast.attribute_of list ;
    contact_from_to : Ast.contact_from_to list
  }

let empty_block = {
    of_category = [] ;
    translation = [] ;
    description = [] ;
    sentence = [] ;
    add = [] ;
    compatible_with = [] ;
    let_player = [] ;
    let_object = [] ;
    provide_relation = [] ;
    provide_attribute = [] ;
    provide_contact = [] ;
    add_difficulty = [] ;
    add_complexity = [] ;
    event_kind = [] ;
    provide_event = [] ;
    event_constraint = [] ;
    attribute_of = [] ;
    contact_from_to = []
  }

(** The type of categories. *)
type category = Id.t

(** The type of events. *)
type event = Id.t

(** The type of element identifier. *)
type element = Id.t

(** A module type for depencency graphs. *)
module type Dependencies = sig

    (** A type to store the dependency graph. *)
    type t

    (** The type of objects whose dependencies we are storing here. *)
    type o

    (** An empty graph. *)
    val empty : t

    (** Add a dependency: the first given object needs the second one. *)
    val add_dependency : t -> o -> o -> t

    (** Add a whole set of dependencies at once. *)
    val add_dependencies : t -> o -> o PSet.t -> t

    exception CircularDependency of o

    (** Compute all dependencies as sets.
       Return [CircularDependency] if there is a circular dependency. *)
    val get_all_dependencies : t -> (o, o PSet.t) PMap.t

  end

(** Adding to [Id.t] the operations required by OCamlGraph. *)
module Id = struct
    include Id
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

(** Instanciate the module type [Graph] for the given singature. *)
module MakeDependencies (C : Graph.Sig.COMPARABLE) : Dependencies with type o = C.t = struct

    type o = C.t

    module Graph = Graph.Persistent.Digraph.Concrete (C)

    type t = Graph.t

    let empty = Graph.empty

    let add_dependency = Graph.add_edge

    let add_dependencies g o =
      PSet.fold (fun o' g -> add_dependency g o o') g

    exception CircularDependency of o

    let get_all_dependencies g =
      (** Compute a category’s dependencies, knowing that the elements of [already_seen]
         depends on the current category. *)
      let rec add_deps already_seen o m =
        if PSet.mem o already_seen then
          raise (CircularDependency o) ;
        let add = add_deps (PSet.add o already_seen) in
        if PMap.mem o m then m
        else
          (** We first require that all the dependencies of the current node
             have been computed. *)
          let m = Graph.fold_succ add g o m in
          (** We can now compute their union and add it to the result. *)
          let deps =
            Graph.fold_succ (fun o s ->
              try let s' = PMap.find o m in
                  PSet.merge s' s
              with Not_found -> assert false) g o PSet.empty in
          PMap.add o deps m in
      let add = add_deps PSet.empty in
      Graph.fold_vertex add g PMap.empty

  end

module CategoryGraph = MakeDependencies (Id)
module EventGraph = MakeDependencies (Id)

type import_information = {
    constructor_maps : Attribute.constructor_maps ;
    event_id : (string, event) PMap.t ;
    event_informations : (event, bool * bool * int Events.translation) PMap.t ;
    event_kinds : (event, (int, int Events.kind PSet.t) PMap.t) PMap.t
  }

type state = {
    category_names : string Id.map (** All declared category names. *) ;
    event_names : string Id.map (** All declared event names. *) ;
    element_names : string Id.map (** All declared element names. *) ;
    import_information : import_information
      (** Some generic information, more or less only important for importation. *) ;
    category_dependencies : (category, category PSet.t) PMap.t
      (** The dependencies of each category. *) ;
    attribute_dependencies : (Attribute.attributes, category PSet.t) PMap.t
      (** For each attribute, the set of category it depends on. *) ;
    constructor_dependencies : (Attribute.constructors, category PSet.t) PMap.t
      (** Similarly, the dependencies of each constructor. *) ;
    object_dependencies : (Attribute.object_constructor, category PSet.t) PMap.t
      (** Similarly, the dependencies of each object kind. *) ;
    elements_dependencies : (element, category PSet.t) PMap.t
      (** Similarly, the dependencies of each element. *) ;
    event_dependencies : (event, category PSet.t) PMap.t
      (** Similarly, the (category) dependencies of each events. *) ;
    event_event_dependencies : (event, event PSet.t) PMap.t
      (** Events introduce a second level of graphs, as events also
         depend on other events. *) ;
    elements : (element, Element.t) PMap.t (** All declared elements. *) ;
    translations : Translation.element (** Global variables for translations. *)
  }

(** For each datatype, this type stores which elements have been defined,
   and which have been used.
   All used elements need to be eventually defined, and unused defined elements
   are not critical, but probably indicate a mistake. *)
type 'a collection = {
    defined : 'a PSet.t ;
    used : 'a PSet.t
  }

type intermediary = {
    current_state : state
      (** The current state.
         Only direct dependencies are here stored.
         Once the function [parse] is called and the state lived dettached
         from the [intermediary] type, all dependencies are complete. *) ;
    category_graph : CategoryGraph.t
      (** The graph of category dependencies. *) ;
    event_graph : EventGraph.t
      (** The graph of event dependencies. *) ;
    (** For each elements, we store what has currently been seen defined and used. *)
    category_collection : category collection ;
    event_collection : event collection ;
    attribute_collection : Attribute.attributes collection ;
    constructor_collection : Attribute.constructors collection ;
    tag_collection : (Translation.language * Translation.tag) collection ;
    waiting_elements : (element * History.status * string * block) list
      (** An element, which will only be treated once all the other declarations
         have been proccessed. *)
  }

(** A useful shortcut. *)
let intermediary_constructor_maps i =
  i.current_state.import_information.constructor_maps

(** The empty collection. *)
let empty_collection = {
    defined = PSet.empty ;
    used = PSet.empty
  }

(** All the objects in a collection that are used without being yet defined. *)
let to_be_defined c =
  PSet.diff c.used c.defined

(** State that an object is now defined. *)
let now_defined c o =
  { c with defined = PSet.add o c.defined }

(** State that an object is being used. *)
let is_used c o =
  { c with used = PSet.add o c.used }

(** State that a set of objects is being used. *)
let are_used c d =
  { c with used = PSet.merge d c.used }

let empty_state = {
    category_names = Id.map_create () ;
    event_names = Id.map_create () ;
    element_names = Id.map_create () ;
    import_information = {
        constructor_maps = Attribute.empty_constructor_maps ;
        event_id = PMap.empty ;
        event_informations = PMap.empty ;
        event_kinds = PMap.empty
      } ;
    category_dependencies = PMap.empty ;
    attribute_dependencies =
      (** The attribute constructor [Attribute.object_type] is always present
         and has no dependencies. *)
      PMap.add (Attribute.PlayerAttribute Attribute.object_type) PSet.empty PMap.empty ;
    constructor_dependencies = PMap.empty ;
    object_dependencies = PMap.empty ;
    elements_dependencies = PMap.empty ;
    event_dependencies = PMap.empty ;
    event_event_dependencies = PMap.empty ;
    elements = PMap.empty ;
    translations = Translation.empty_element
  }

let empty_intermediary = {
    current_state = empty_state ;
    category_graph = CategoryGraph.empty ;
    event_graph = EventGraph.empty ;
    category_collection = empty_collection ;
    event_collection = empty_collection ;
    attribute_collection =
      is_used empty_collection (Attribute.PlayerAttribute Attribute.object_type) ;
    constructor_collection = empty_collection ;
    tag_collection = empty_collection ;
    waiting_elements = []
  }

let categories_to_be_defined i =
  PSet.map (fun id ->
      Utils.assert_option __LOC__
        (Id.map_inverse i.current_state.category_names id))
    (to_be_defined i.category_collection)

let events_to_be_defined i =
  PSet.map (fun id ->
      Utils.assert_option __LOC__
        (Id.map_inverse i.current_state.event_names id))
    (to_be_defined i.event_collection)

let attributes_to_be_defined i =
  PSet.partition_map (function
      | Attribute.PlayerAttribute id ->
        Utils.Left (Utils.assert_option __LOC__
          (Attribute.PlayerAttribute.attribute_name
            (intermediary_constructor_maps i).Attribute.player id))
      | Attribute.ContactAttribute id ->
        Utils.Right (Utils.assert_option __LOC__
          (Attribute.ContactAttribute.attribute_name
            (intermediary_constructor_maps i).Attribute.contact id)))
    (to_be_defined i.attribute_collection)

let constructors_to_be_defined i =
  PSet.partition_map (function
      | Attribute.PlayerConstructor c ->
        let i = (intermediary_constructor_maps i).Attribute.player in
        let a =
          Utils.assert_option __LOC__
            (Attribute.PlayerAttribute.constructor_attribute i c) in
        let a =
          Utils.assert_option __LOC__
            (Attribute.PlayerAttribute.attribute_name i a) in
        let c =
          Utils.assert_option __LOC__
            (Attribute.PlayerAttribute.constructor_name i c) in
        Utils.Left (a, c)
      | Attribute.ContactConstructor c ->
        let i = (intermediary_constructor_maps i).Attribute.contact in
        let a =
          Utils.assert_option __LOC__
            (Attribute.ContactAttribute.constructor_attribute i c) in
        let a =
          Utils.assert_option __LOC__
            (Attribute.ContactAttribute.attribute_name i a) in
        let c =
          Utils.assert_option __LOC__
            (Attribute.ContactAttribute.constructor_name i c) in
        Utils.Right (a, c))
    (to_be_defined i.constructor_collection)

let tags_to_be_defined i =
  PSet.map (fun (lg, tag) ->
      (Translation.iso639 lg, Translation.print_tag tag))
    (to_be_defined i.tag_collection)

let is_intermediary_final i =
  let is_empty2 (s1, s2) = PSet.is_empty s1 && PSet.is_empty s2 in
  PSet.is_empty (categories_to_be_defined i)
  && PSet.is_empty (events_to_be_defined i)
  && is_empty2 (attributes_to_be_defined i)
  && is_empty2 (constructors_to_be_defined i)
  && PSet.is_empty (tags_to_be_defined i)

let all_categories i =
  Id.map_fold (fun _ id l -> id :: l) [] i.category_names

(** This type is used as an internal type to express the kind of expected
   command type in a given block. *)
type command_type =
  | OfCategory
  | Translation
  | Description
  | Sentence
  | Add
  | CompatibleWith
  | LetPlayer
  | LetObject
  | ProvideRelation
  | ProvideAttribute
  | ProvideContact
  | AddDifficulty
  | AddComplexity
  | EventKind
  | ProvideEvent
  | EventConstraint
  | AttributeOf
  | ContactFromTo

(** Converts command types to string, for easier-to-understand error messages. *)
let command_type_to_string = function
  | OfCategory -> "category"
  | Translation -> "translation"
  | Description -> "description"
  | Sentence -> "sentence"
  | Add -> "add"
  | CompatibleWith -> "category compatibility"
  | LetPlayer -> "player declaration"
  | LetObject -> "object declaration"
  | ProvideRelation -> "relation provision"
  | ProvideAttribute -> "attribute provision"
  | ProvideContact -> "contact provision"
  | AddDifficulty -> "difficulty provision"
  | AddComplexity -> "complexity provision"
  | EventKind -> "event kind declaration"
  | ProvideEvent -> "event provision"
  | EventConstraint -> "event constraint"
  | AttributeOf -> "attribute kind"
  | ContactFromTo -> "contact kind"

exception UnexpectedCommandInBlock of string * string

exception DefinedTwice of string * string * string

exception Undeclared of string * string * string

exception CircularDependency of string * string

exception SelfRelation of string * string

exception TranslationError of string * string * Ast.translation

exception VacuumElement of string

exception UnsatisfyableEventSequence of string


(** Converts an [Ast.block] into a [block].
   It takes a list of command types and checks that only these are present
   in the given block: all the other kinds will thus be empty lists.
   This function reverses the order of declaration in the block. *)
let convert_block block_name expected =
  let check c =
    if not (List.mem c expected) then
      raise (UnexpectedCommandInBlock (block_name, command_type_to_string c)) in
  let rec aux acc = function
  | [] -> {
      of_category = List.rev acc.of_category ;
      translation = List.rev acc.translation ;
      description = List.rev acc.description ;
      sentence = List.rev acc.sentence ;
      add = List.rev acc.add ;
      compatible_with = List.rev acc.compatible_with ;
      let_player = List.rev acc.let_player ;
      let_object = List.rev acc.let_object ;
      provide_relation = List.rev acc.provide_relation ;
      provide_attribute = List.rev acc.provide_attribute ;
      provide_contact = List.rev acc.provide_contact ;
      add_difficulty = List.rev acc.add_difficulty ;
      add_complexity = List.rev acc.add_complexity ;
      event_kind = List.rev acc.event_kind ;
      provide_event = List.rev acc.provide_event ;
      event_constraint = List.rev acc.event_constraint ;
      attribute_of = List.rev acc.attribute_of ;
      contact_from_to = List.rev acc.contact_from_to
    }
  | c :: l ->
    aux
      (match c with
      | Ast.OfCategory c ->
        check OfCategory ;
        { acc with of_category = c :: acc.of_category }
      | Ast.Translation t ->
        check Translation ;
        { acc with translation = t :: acc.translation }
      | Ast.Description d ->
        check Description ;
        { acc with description = d :: acc.description }
      | Ast.Sentence b ->
        check Sentence ;
        { acc with sentence = b :: acc.sentence }
      | Ast.Add a ->
        check Add ;
        { acc with add = a :: acc.add }
      | Ast.CompatibleWith c ->
        check CompatibleWith ;
        { acc with compatible_with = c :: acc.compatible_with }
      | Ast.LetPlayer l ->
        check LetPlayer ;
        { acc with let_player = l :: acc.let_player }
      | Ast.LetObject l ->
        check LetObject ;
        { acc with let_object = l :: acc.let_object }
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
        { acc with event_constraint = p :: acc.event_constraint }
      | Ast.AttributeOf k ->
        check AttributeOf ;
        { acc with attribute_of = k :: acc.attribute_of }
      | Ast.ContactFromTo k ->
        check ContactFromTo ;
        { acc with contact_from_to = k :: acc.contact_from_to }) l in
  aux empty_block

(** Take a list of category names and return a set of category identifiers,
   as well as the possibly-changed [category_names] field. *)
let category_names_to_id_set state l =
  List.fold_left (fun (category_names, s) name ->
      let (id, category_names) = Id.map_insert_t category_names name in
      (category_names, PSet.add id s))
    (state.category_names, PSet.empty) l

(** Similar to [category_names_to_id_set], but for the event graph. *)
let event_names_to_id_set state l =
  List.fold_left (fun (event_names, s) name ->
      let (id, event_names) = Id.map_insert_t event_names name in
      (event_names, PSet.add id s))
    (state.event_names, PSet.empty) l


(** Declare an object name using its special attribute.
   It takes and propagates the main constructor map. *)
let name_object m name =
  let (o, m') =
    Attribute.PlayerAttribute.name_constructor m.Attribute.player Attribute.object_type name in
  (o, Attribute.PlayerAttribute.set_constructor_maps m m')

(** Converts an [Ast.kind] to an [Attribute.attribute_kind list].
   It takes as argument the constructor map [m] and returns it.
   It also returns a list of used objects. *)
let kind_to_attribute_kind m = function
  | Ast.Any -> (m, [], Attribute.attribute_any)
  | Ast.AnyObject -> (m, [], [Attribute.AnyObject])
  | Ast.Player -> (m, [], [Attribute.Player])
  | Ast.Object id ->
    let (o, m) = name_object m id in
    (m, [o], [Attribute.Object o])

(** Similar to [kind_to_attribute_kind], but takes a disjunctive list as argument. *)
let kinds_to_attribute_kind m =
  List.fold_left (fun (m, used, l) k ->
    let (m, used', l') = kind_to_attribute_kind m k in
    (m, used' @ used, l' @ l)) (m, [], [])

(** Use the [attribute_of] field of blocks to provide an attribute kind.
   As for [kind_to_attribute_kind], the [m] argument is propagated along the way. *)
let get_kind_attribute m b =
  let l = b.attribute_of in
  if l = [] then (m, [], Attribute.attribute_any)
  else kinds_to_attribute_kind m (List.concat l)

(** Use the [contact_from_to] field of blocks to provide a contact kind.
   As for [kind_to_attribute_kind], the [m] argument is propagated along the way. *)
let get_kind_contact m b =
  let l = b.contact_from_to in
  if l = [] then (m, [], Attribute.contact_any)
  else
    let aux m (f, t) =
      let (m, usedf, f) = kinds_to_attribute_kind m f in
      let (m, usedt, t) = kinds_to_attribute_kind m t in
      let r =
        List.concat (List.map (fun f ->
          List.map (fun t -> {
              Attribute.kind_from = f ;
              Attribute.kind_to = t
            }) t) f) in
      (m, usedf @ usedt, r) in
    List.fold_left (fun (m, used, l) k ->
      let (m, used', r) = aux m k in
      (m, used' @ used, r @ l)) (m, [], []) l

(** State whether an object has been defined in a collection. *)
let has_been_defined collection o =
  PSet.mem o collection.defined

(** State whether a category has been defined. *)
let category_defined state =
  has_been_defined state.category_collection

(** State whether an event has been defined. *)
let event_defined state =
  has_been_defined state.event_collection

(** State whether an attribute has been defined. *)
let attribute_defined state =
  has_been_defined state.attribute_collection

(** State whether a constructor has been defined. *)
let constructor_defined state =
  has_been_defined state.constructor_collection

(** State whether an object has been defined. *)
let object_defined state id =
  constructor_defined state (Attribute.PlayerAttribute.to_constructors id)

(** State whether a tag has been defined for a language. *)
let tag_defined state =
  has_been_defined state.tag_collection

(** This function parses basically everything but elements in a declaration. *)
let prepare_declaration i =
  (** Declare attribute and contact instances. *)
  let declare_instance (type k) (module A : Attribute.Attribute with type kind = k)
      kind_annot get_kind name internal block =
    let block = convert_block name [OfCategory; Translation; kind_annot] block in
    let state = intermediary_constructor_maps i in
    let (state, object_used, kind) = get_kind state block in
    let object_used =
      PSet.from_list (List.map Attribute.PlayerAttribute.to_constructors object_used) in
    let (id, state) =
      let (id, state') =
        A.declare_attribute (A.get_constructor_maps state) name internal kind in
      (id, A.set_constructor_maps state state') in
    let id = A.to_attributes id in
    if attribute_defined i id then
      raise (DefinedTwice (A.name, name, "")) ;
    let (category_names, deps) = category_names_to_id_set i.current_state block.of_category in
    let translations =
      let translations =
        Translation.add i.current_state.translations.Translation.attribute
          Translation.generic id name in
      List.fold_left (fun translations tr ->
        let (lg, tags, items) = tr in
        if tags <> [(None, Translation.base)] then
          raise (TranslationError (A.name, name, tr)) ;
        let str =
          String.concat "" (List.map (function
            | Translation.Direct str -> str
            | _ ->
              raise (TranslationError (A.name, name, tr))) items) in
        Translation.add translations lg id str) translations block.translation in
    { i with
        category_collection = are_used i.category_collection deps ;
        attribute_collection = now_defined i.attribute_collection id ;
        constructor_collection = are_used i.constructor_collection object_used ;
        current_state =
          { i.current_state with
              import_information =
                { i.current_state.import_information with
                    constructor_maps = state } ;
              category_names = category_names ;
              attribute_dependencies =
                PMap.add id deps i.current_state.attribute_dependencies ;
              translations =
                { i.current_state.translations with Translation.attribute =
                    translations } } } in
  (** Declare constructor instances. *)
  let declare_constructor (type c) (module A : Attribute.Attribute with type constructor = c)
      (get_player_attribute : string -> c -> Attribute.PlayerAttribute.constructor)
      attribute_name constructor internal block =
    let block =
      convert_block attribute_name [OfCategory; Translation; Add; CompatibleWith] block in
    let (attribute, state) =
      A.name_attribute (A.get_constructor_maps (intermediary_constructor_maps i)) attribute_name in
    let (idp, state) = A.declare_constructor state attribute constructor internal in
    let id = A.to_constructors idp in
    if constructor_defined i id then
      raise (DefinedTwice (A.name ^ " constructor", constructor, attribute_name)) ;
    let (compatibilities, state) =
      List.fold_left (fun (compatibilities, state) constructor' ->
          let (id', state) = A.name_constructor state attribute constructor' in
          let id' = A.to_constructors id' in
          (PSet.add id' compatibilities, state)) (PSet.empty, state) block.compatible_with in
    let attribute = A.to_attributes attribute in
    let (category_names, deps) = category_names_to_id_set i.current_state block.of_category in
    let (translations, used_tags) =
      let translations =
        Translation.gadd i.current_state.translations.Translation.constructor
          Translation.generic [] id constructor in
      List.fold_left (fun (translations, used_tags) tr ->
          let (lg, tags, items) = tr in
          let str =
            String.concat "" (List.map (function
              | Translation.Direct str -> str
              | _ ->
                raise (TranslationError (A.name, constructor, tr))) items) in
          try (Translation.gadd translations lg tags id str,
               PSet.merge (PSet.from_list (Utils.list_map_filter (fun (_, tag) ->
                 if tag = Translation.base then None
                 else Some (lg, tag)) tags)) used_tags)
          with Translation.ConflictingCommands _ ->
            raise (TranslationError (A.name, constructor, tr)))
        (translations, PSet.empty) block.translation in
    let add =
      List.fold_left (fun add (lg, tag) ->
          let m =
            try PMap.find lg add
            with Not_found -> PMap.empty in
          let id = get_player_attribute attribute_name idp in
          let s =
            try PMap.find id m
            with Not_found -> PSet.empty in
          let s = PSet.add tag s in
          let m = PMap.add id s m in
          PMap.add lg m add)
        i.current_state.translations.Translation.add block.add in
    { i with
        category_collection = are_used i.category_collection deps ;
        attribute_collection = is_used i.attribute_collection attribute ;
        constructor_collection =
          are_used (now_defined i.constructor_collection id) compatibilities ;
        tag_collection = are_used i.tag_collection used_tags ;
        current_state =
          { i.current_state with
              import_information =
                { i.current_state.import_information with
                    constructor_maps =
                      A.set_constructor_maps (intermediary_constructor_maps i) state } ;
              constructor_dependencies =
                PMap.add id deps i.current_state.constructor_dependencies ;
              category_names = category_names ;
              translations =
                { i.current_state.translations with
                    Translation.constructor = translations ;
                    Translation.add = add } } } in
  function
  | Ast.DeclareInstance (Ast.Attribute, attribute, internal, block) ->
    declare_instance (module Attribute.PlayerAttribute)
      AttributeOf get_kind_attribute attribute internal block
  | Ast.DeclareInstance (Ast.Contact, contact, internal, block) ->
    declare_instance (module Attribute.ContactAttribute)
      ContactFromTo get_kind_contact contact internal block
  | Ast.DeclareConstructor (Ast.Attribute, attribute, constructor, internal, block) ->
    declare_constructor (module Attribute.PlayerAttribute) (fun _ -> Utils.id)
      attribute constructor internal block
  | Ast.DeclareConstructor (Ast.Contact, attribute, constructor, internal, block) ->
    declare_constructor (module Attribute.ContactAttribute) (fun name _ ->
        raise (UnexpectedCommandInBlock (name, "add")))
      attribute constructor internal block
  | Ast.DeclareObject (name, internal, block) ->
    let block =
      convert_block name [OfCategory; Translation (* TODO: CompatibleWith *)] block in
    let state = intermediary_constructor_maps i in
    let (id, state) =
      let (id, state') =
        Attribute.PlayerAttribute.declare_constructor state.Attribute.player
          Attribute.object_type name internal in
      (id, Attribute.PlayerAttribute.set_constructor_maps state state') in
    if object_defined i id then
      raise (DefinedTwice ("object", name, "")) ;
    let idp = Attribute.PlayerConstructor id in
    let (category_names, deps) = category_names_to_id_set i.current_state block.of_category in
    let translations =
      let translations =
        Translation.gadd i.current_state.translations.Translation.constructor
          Translation.generic [] idp name in
      List.fold_left (fun translations tr ->
        let (lg, tags, items) = tr in
        if tags <> [(None, Translation.base)] then
          raise (TranslationError ("object", name, tr)) ;
        let str =
          String.concat "" (List.map (function
            | Translation.Direct str -> str
            | _ ->
              raise (TranslationError ("object", name, tr))) items) in
        try Translation.gadd translations lg [] idp str
        with Translation.ConflictingCommands _ ->
          raise (TranslationError ("object", name, tr))) translations block.translation in
    (** In addition to the object name, we declare a special attribute constructor
       for the dummy attribute [Attribute.object_type]. *)
    let (idp, state) =
      let (idp, state') =
        Attribute.PlayerAttribute.declare_constructor state.Attribute.player
          Attribute.object_type name true in
      (idp, Attribute.PlayerAttribute.set_constructor_maps state state') in
    let idp = Attribute.PlayerConstructor idp in
    { i with
        category_collection = are_used i.category_collection deps ;
        constructor_collection = now_defined i.constructor_collection idp ;
        current_state =
          { i.current_state with
              import_information =
                { i.current_state.import_information with
                    constructor_maps = state } ;
              object_dependencies =
                PMap.add id deps i.current_state.object_dependencies ;
              constructor_dependencies =
                PMap.add idp deps i.current_state.constructor_dependencies ;
              category_names = category_names ;
              translations =
                { i.current_state.translations with
                    Translation.constructor = translations } } }
  | Ast.DeclareCategory (name, block) ->
    let block =
      convert_block name [OfCategory; Translation; Description] block in
    let (category_names, deps) = category_names_to_id_set i.current_state block.of_category in
    let (id, category_names) = Id.map_insert_t category_names name in
    if category_defined i id then
      raise (DefinedTwice ("category", name, "")) ;
    let translations =
      List.fold_left (fun translation tr ->
          let (lg, tags, items) = tr in
          if tags <> [(None, Translation.base)] then
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
    let descriptions =
      List.fold_left (fun translation (lg, desc) -> Translation.add translation lg id desc)
        (Translation.add i.current_state.translations.Translation.category_description
          Translation.generic id "")
        block.description in
    { i with
        category_collection = are_used (now_defined i.category_collection id) deps ;
        category_graph = CategoryGraph.add_dependencies i.category_graph id deps ;
        current_state =
          { i.current_state with
              category_names = category_names ;
              translations =
                { i.current_state.translations with
                    Translation.category = translations ;
                    Translation.category_description = descriptions } } }
  | Ast.DeclareElement (status, name, block) ->
    (match Id.get_id i.current_state.element_names name with
     | None -> ()
     | Some _ -> raise (DefinedTwice ("element", name, ""))) ;
    let (id, elements) =
      Id.map_insert_t i.current_state.element_names name in
    let block =
      convert_block name [OfCategory; LetPlayer; LetObject;
                          ProvideRelation; ProvideAttribute; ProvideContact;
                          AddDifficulty; AddComplexity;
                          ProvideEvent] block in
    { i with
        waiting_elements = (id, status, name, block) :: i.waiting_elements ;
        current_state =
          { i.current_state with element_names = elements } }
  | Ast.DeclareCase (lang, tag) ->
    if tag_defined i (lang, tag) then
      raise (DefinedTwice ("tag", Translation.print_tag tag,
                           Translation.iso639 lang)) ;
    { i with tag_collection = now_defined i.tag_collection (lang, tag) }
  | Ast.DeclareEventKind (kind, block) ->
    let block =
      convert_block kind [OfCategory; EventKind] block in
    let (category_names, cat_deps) =
      category_names_to_id_set i.current_state block.of_category in
    let (event_names, event_deps) =
      event_names_to_id_set i.current_state block.event_kind in
    let (id, event_names) = Id.map_insert_t event_names kind in
    if event_defined i id then
      raise (DefinedTwice ("event", kind, "")) ;
    { i with
        category_collection = are_used i.category_collection cat_deps ;
        event_collection = are_used (now_defined i.event_collection id) event_deps ;
        event_graph = EventGraph.add_dependencies i.event_graph id event_deps ;
        current_state =
          { i.current_state with
              category_names = category_names ;
              event_names = event_names ;
              event_dependencies =
                PMap.add id cat_deps i.current_state.event_dependencies } }

let prepare_declarations i l =
  List.fold_left prepare_declaration i l

let get_category_dependencies s id =
  try PMap.find id s.category_dependencies
  with Not_found -> assert false

let get_attribute_dependencies s id =
  try PMap.find id s.attribute_dependencies
  with Not_found -> assert false

let get_constructor_dependencies s id =
  try PMap.find id s.constructor_dependencies
  with Not_found -> assert false

(** Fill the [Events.event_id] identifier. *)
let event_id = Id.new_id_function ()

(** Fill the [Element.id] identifier. *)
let element_id = Id.new_id_function ()

(** Generates an element from a [state] and a [block].
   The state is meant to have stabilised at this point: we assumed that it won’t be changed
   in the future at this point. *)
let parse_element st element_name status block =
  let get_constructor_dependencies = get_constructor_dependencies st in
  (** Given an event block, return all its (transitive) dependencies. *)
  let get_event_dependencies block =
    let (event_names, s) = event_names_to_id_set st block.event_kind in
    PSet.merge (PSet.flat_map (fun e ->
      try PMap.find e st.event_event_dependencies
      with Not_found ->
        let e = Utils.assert_option __LOC__ (Id.map_inverse event_names e) in
        raise (Undeclared ("event kind", e, element_name))) s) s in
  (** Before anything, we get the number and names of each declared players and objects. *)
  let nb_players = List.length block.let_player in
  let player_names =
    List.fold_left (fun player_names name ->
        if Id.get_id player_names name <> None then
          raise (DefinedTwice ("player", name, element_name)) ;
        Id.map_insert player_names name)
      (Id.map_create ())
      (Utils.list_map_filter fst block.let_player) in
  let nb_objects = List.length block.let_object in
  let object_names =
    List.fold_left (fun object_names name ->
        if Id.get_id object_names name <> None then
          raise (DefinedTwice ("object", name, element_name)) ;
        if Id.get_id player_names name <> None then
          raise (DefinedTwice ("player and object", name, element_name)) ;
        Id.map_insert object_names name)
      (Id.map_create ())
      (List.map (fun (_, name, _) -> name) block.let_object) in
  (** Getting the name of players and objects from their identifier. *)
  let get_player_name p =
    Utils.assert_option __LOC__ (Id.map_inverse player_names p) in
  let get_object_name o =
    Utils.assert_option __LOC__ (Id.map_inverse object_names o) in
  let _get_name = function
    | Utils.Left p -> get_player_name p
    | Utils.Right o -> get_object_name o in
  (** Getting an identifier for players and objects from their name. *)
  let get_player_option p = Id.get_id player_names p in
  let get_player p =
    match get_player_option p with
    | Some p -> p
    | None -> raise (Undeclared ("player", p, element_name)) in
  let get_player_array p = Id.to_array (get_player p) in
  let get_object_option o = Id.get_id object_names o in
  let get_object o =
    match get_object_option o with
    | Some o -> o
    | None -> raise (Undeclared ("object", o, element_name)) in
  let get_object_array o = Id.to_array (get_object o) in
  let get_po_id po =
    match get_player_option po with
    | Some p -> Utils.Left (Id.to_array p)
    | None ->
      match get_object_option po with
      | Some o -> Utils.Right (Id.to_array o)
      | None -> raise (Undeclared ("player or object", po, element_name)) in
  (** We pre-parse the element’s events, as they might contain declarations. *)
  let events =
    List.map (fun (bl, ph, t, l, b) ->
      (bl, ph, t, List.map get_player l,
       convert_block element_name
         [Translation; Sentence; ProvideAttribute; ProvideContact;
          EventKind; EventConstraint] b)) block.provide_event in
  (** We first consider relations. *)
  let relations =
    (** We create this triangle of relations.
       Each index is associated with its greatest non-[neutral] index plus one,
       or [0] is the entire array is filled with [neutral]. *)
    let triangle =
      Array.init nb_players (fun i -> (Array.make i Relation.neutral, 0)) in
    let write i j r =
      if r <> Relation.neutral then (
        if i = j then (
          raise (SelfRelation (get_player_name i, element_name))) ;
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
      | Ast.Between l ->
        Option.may (fun p ->
          raise (SelfRelation (p, element_name))) (Utils.is_uniq_witness l) ;
        let l = List.map get_player l in
        List.iter (fun (p1, p2) -> write p1 p2 r)
          (List.filter (fun (p1, p2) -> p1 < p2) (Utils.list_square l l))
      | Ast.FromTo (p1, p2) ->
        write (get_player p1) (get_player p2)
          (Relation.asymmetrical r Relation.neutral)) block.provide_relation ;
    (** We now try to minimize each array.
       Note that we could try to change player identifiers to optimize
       space even more, but this might take additional resources needlessly. *)
    Array.map (fun (a, m) -> Array.sub a 0 m) triangle in
  (** We define our current element and constraints over other players,
     and we will update them by considering each declaration. *)
  let elementBase =
    Array.map (fun a -> {
        Element.constraints = [] ;
        Element.relations = a ;
        Element.added_objective = State.zero_objective
      }) relations in
  let otherPlayers = ref [] in
  let elementObjects = Array.make nb_objects [] in
  let deps =
    let (category_names, s) = category_names_to_id_set st block.of_category in
    PSet.merge (PSet.flat_map (fun c ->
      try PMap.find c st.category_dependencies
      with Not_found ->
        let c = Utils.assert_option __LOC__ (Id.map_inverse category_names c) in
        raise (Undeclared ("category", c, element_name))) s) s in
  (** We also consider the category dependencies forced by events. *)
  let deps =
    let edeps =
      List.fold_left (fun edeps (_, _, _, _, eblock) ->
          PSet.merge (get_event_dependencies eblock) edeps)
        PSet.empty events in
    let edeps_cat =
      PSet.flat_map (fun eid ->
        try PMap.find eid st.event_dependencies
        with Not_found -> assert false) edeps in
    PSet.merge edeps_cat deps in
  (** Now that we have all the basic information, we can add any constraint
     using this function with side effects. *)
  let add_constraint p c =
    match p with
    | Utils.Left p ->
      elementBase.(p) <-
        { elementBase.(p) with Element.constraints =
                                 c :: elementBase.(p).Element.constraints }
    | Utils.Right o ->
      elementObjects.(o) <- c :: elementObjects.(o) in
  let add_constraint_other c =
    otherPlayers := c :: !otherPlayers in
  let add_constraint_all c =
    (** Exploding [Ast.AllPlayers] into a list of [Ast.DestinationPlayer] and
       [Ast.AllOtherPlayers] tends to make non-acceptable combinations appear.
       The [ok] function filters these out. *)
    let ok p =
      match c with
      | Element.Contact (_, Some p', _) -> Utils.Left p <> p'
      | _ -> true in
    List.iter (fun p ->
      if ok p then
        add_constraint (Utils.Left p) c) (Utils.seq nb_players) ;
    add_constraint_other c in
  (** Retrieve the attribute identifier given by this name.
     At this stage, it has to be defined. *)
  let get_attribute_id (type a) (module A : Attribute.Attribute with type attribute = a) name : a =
    match A.get_attribute (A.get_constructor_maps st.import_information.constructor_maps) name with
    | None -> raise (Undeclared (A.name, name, element_name))
    | Some id -> id in
  (** Similar to [get_attribute_id], but for constructors. *)
  let get_constructor_id (type a c)
        (module A : Attribute.Attribute with type attribute = a and type constructor = c)
        aid name : c =
    match A.get_constructor (A.get_constructor_maps st.import_information.constructor_maps)
            aid name with
    | None -> raise (Undeclared (A.name ^ " constructor", name, element_name))
    | Some id -> id in
  (** Merges the current dependencies with the ones of the given constructor.
     Note that the corresponding attribute’s dependencies already have been
     reported to the constructor: there is no need for an additional merge. *)
  let merge_with_constructor_dependencies deps cid =
    PSet.merge deps (get_constructor_dependencies cid) in
  (** An alternative to [merge_with_constructor_dependencies] when given a list. *)
  let merge_with_constructor_dependencies_list deps =
    List.fold_left (fun deps cid ->
      merge_with_constructor_dependencies deps cid) deps in
  (** There are places where instead of wanting the union of all dependencies, we
     only want to perform their intersection (typically, if an element requires
     one of several conditions, this only adds the intersection of the category
     dependencies of each conditions to the element. *)
  let intersect_with_constructor_dependencies deps cid =
    PSet.inter deps (get_constructor_dependencies cid) in
  (** An alternative to [intersect_with_constructor_dependencies] when given a list.
     We however still want to merge this intersection with the old dependencies. *)
  let intersect_with_constructor_dependencies_list deps = function
    | [] -> raise (VacuumElement element_name)
    | cid :: cidl ->
      PSet.merge deps
        (List.fold_left (fun deps cid ->
            intersect_with_constructor_dependencies deps cid)
          (get_constructor_dependencies cid) cidl) in
  (** We now consider added difficulty and complexity. *)
  List.iter (fun (d, p) ->
      let d = if d then 1 else -1 in
      let p = get_player_array p in
      let o = elementBase.(p).Element.added_objective in
      elementBase.(p) <-
        { elementBase.(p) with
            Element.added_objective =
              { o with State.difficulty = d + o.State.difficulty } })
    block.add_difficulty ;
  List.iter (fun (d, p) ->
      let d = if d then 1 else -1 in
      let p = get_player_array p in
      let o = elementBase.(p).Element.added_objective in
      elementBase.(p) <-
        { elementBase.(p) with
            Element.added_objective =
              { o with State.complexity = d + o.State.complexity } })
    block.add_complexity ;
  (** We now consider attributes. *)
  let attributes =
    (** Attribute declarations may be placed inside events to mean that
       it was a particular event that triggered this declaration. *)
    List.fold_left (fun l (_, _, _, _, b) ->
      b.provide_attribute @ l) block.provide_attribute events in
  let deps =
    List.fold_left (fun deps pa ->
        let aid = get_attribute_id (module Attribute.PlayerAttribute) pa.Ast.attribute_name in
        let cid =
          List.map (get_constructor_id (module Attribute.PlayerAttribute) aid)
            pa.Ast.attribute_value in
        let c =
          Element.Attribute (aid,
            State.Fixed_value (cid, pa.Ast.attribute_strictness)) in
        let _ =
          match pa.Ast.attribute_player with
          | Ast.DestinationPlayer p -> add_constraint (get_po_id p) c
          | Ast.AllOtherPlayers -> add_constraint_other c
          | Ast.AllPlayers -> add_constraint_all c in
        merge_with_constructor_dependencies_list deps
          (List.map (fun id -> Attribute.PlayerConstructor id) cid))
      deps attributes in
  (** We then consider contacts. *)
  let contacts =
    (** Similarly to attributes, contact declarations may be placed
       inside events. *)
    List.fold_left (fun l (_, _, _, _, b) ->
      b.provide_contact @ l) block.provide_contact events in
  let deps =
    List.fold_left (fun deps pc ->
        let aid = get_attribute_id (module Attribute.ContactAttribute) pc.Ast.contact_name in
        let cid =
          List.map (get_constructor_id (module Attribute.ContactAttribute) aid)
            pc.Ast.contact_value in
        let constr p2 =
          Element.Contact (aid, p2,
            State.Fixed_value (cid, pc.Ast.contact_strictness)) in
        (** Call the function [add] on all requested destinations,
           except [p1] if there.
           No error is thrown: the goal is only to avoid two [any player]
           commands used together to yield any conflict. *)
        let add_except p1 add =
          let add_single p2 =
            if p1 <> Some p2 then add (Some p2) in function
          | Ast.DestinationPlayer p2 -> add_single (get_po_id p2)
          | Ast.AllOtherPlayers -> add None
          | Ast.AllPlayers ->
            List.iter add_single (List.map (fun p -> Utils.Left p) (Utils.seq nb_players)) ;
            add None in
        let add = function
          | Ast.DestinationPlayer p1n ->
            let p1 = get_po_id p1n in
            add_except None (fun p2 ->
              if p2 = Some p1 then
                raise (SelfRelation (p1n, element_name)) ;
              add_constraint p1 (constr p2))
          | Ast.AllOtherPlayers ->
            add_except None (fun p2 -> add_constraint_other (constr p2))
          | Ast.AllPlayers -> fun p2 ->
            List.iter (fun p1 ->
                let p1 = Utils.Left p1 in
                add_except (Some p1) (fun p2 -> add_constraint p1 (constr p2)) p2)
              (Utils.seq nb_players) ;
            add_except None (fun p2 -> add_constraint_other (constr p2)) p2 in
        let _ =
          match pc.Ast.contact_destination with
          | Ast.FromTo (p1, p2) -> add p1 p2
          | Ast.Between l ->
            Option.may (fun d ->
              let p =
                match d with
                | Ast.DestinationPlayer p -> p
                | Ast.AllOtherPlayers -> "all other players"
                | Ast.AllPlayers -> "all players" in
              raise (SelfRelation (p, element_name))) (Utils.is_uniq_witness l) ;
            List.iter (fun (p1, p2) -> add p1 p2)
              (List.filter (fun (p1, p2) -> p1 <> p2) (Utils.list_square l l)) in
        merge_with_constructor_dependencies_list deps
          (List.map (fun id -> Attribute.ContactConstructor id) cid))
      deps contacts in
  (** We then consider player and object constraints. *)
  let consider_constraints add_constraint =
    List.fold_left (fun deps -> function
        | Ast.HasAttribute (a, n, c) ->
          let aid = get_attribute_id (module Attribute.PlayerAttribute) a in
          let cid = List.map (get_constructor_id (module Attribute.PlayerAttribute) aid) c in
          let cid =
            if n then (
              (** At this stage, all the constructors have been defined. *)
              let all =
                Utils.assert_option __LOC__
                  (Attribute.PlayerAttribute.constructors
                    st.import_information.constructor_maps.Attribute.player aid) in
              List.filter (fun c -> not (List.mem c cid)) all
            ) else cid in
          add_constraint (Element.Attribute (aid, State.One_value_of cid)) ;
          intersect_with_constructor_dependencies_list deps
            (List.map (fun id -> Attribute.PlayerConstructor id) cid)
        | Ast.HasContact (a, p', n, c) ->
          let aid = get_attribute_id (module Attribute.ContactAttribute) a in
          let cid = List.map (get_constructor_id (module Attribute.ContactAttribute) aid) c in
          let cid =
            if n then (
              (** At this stage, all the constructors have been defined. *)
              let all =
                Utils.assert_option __LOC__
                  (Attribute.ContactAttribute.constructors
                    st.import_information.constructor_maps.Attribute.contact aid) in
              List.filter (fun c -> not (List.mem c cid)) all
            ) else cid in
          let p' = Option.map get_po_id p' in
          add_constraint
            (Element.Contact (aid, p', State.One_value_of cid)) ;
          intersect_with_constructor_dependencies_list deps
            (List.map (fun id ->
              Attribute.ContactConstructor id) cid)) deps in
  let deps =
    List.fold_left (fun deps (p, pc) ->
        let add_constraint =
          match p with
          | None -> add_constraint_other
          | Some p -> add_constraint (Utils.Left (get_player_array p)) in
        let deps' = consider_constraints add_constraint pc in
        PSet.merge deps deps')
      deps block.let_player in
  let deps =
    List.fold_left (fun deps (kind, o, pc) ->
        let add_constraint = add_constraint (Utils.Right (get_object_array o)) in
        let kind_id =
          let m = st.import_information.constructor_maps.Attribute.player in
          match Attribute.PlayerAttribute.get_constructor m Attribute.object_type kind with
          | Some id -> id
          | None -> raise (Undeclared ("object kind", kind, element_name)) in
        add_constraint (Element.Attribute (Attribute.object_type, State.One_value_of [kind_id])) ;
        let deps' = consider_constraints add_constraint pc in
        PSet.merge deps deps')
      deps block.let_object in
  (** We finally consider events. *)
  (* TODO: Update for objects. *)
  let evs =
    List.mapi (fun i (blocking, phantom, t, attendees, block) ->
      let event_name = element_name ^ "#" ^ string_of_int i in
      let kinds =
        let kinds =
          let deps = PSet.map Events.kind_of_id (get_event_dependencies block) in
          List.fold_left (fun kinds p -> PMap.add (Id.to_array p) deps kinds)
            PMap.empty attendees in
        let add p k kinds =
          let s =
            try PMap.find p kinds
            with Not_found -> PSet.empty in
          PMap.add p (PSet.add k s) kinds in
        let kinds =
          List.fold_left (fun kinds pa ->
             let p =
               match pa.Ast.attribute_player with
               | Ast.DestinationPlayer p -> get_player_array p
               | _ ->
                 raise (UnexpectedCommandInBlock (event_name,
                         "destination of attribute")) in
             let aid = get_attribute_id (module Attribute.PlayerAttribute) pa.Ast.attribute_name in
             add p (Events.kind_of_attribute aid) kinds)
           kinds block.provide_attribute in
        let kinds =
          List.fold_left (fun kinds pa ->
            let aid = get_attribute_id (module Attribute.ContactAttribute) pa.Ast.contact_name in
            let get_player = function
              | Ast.DestinationPlayer p -> Id.to_array (get_player p)
              | _ ->
                raise (UnexpectedCommandInBlock (event_name,
                        "destination of contact")) in
            let add p p' =
              let p = get_player p in
              let p' = get_player p' in
              add p (Events.kind_of_contact aid p') in
            match pa.Ast.contact_destination with
            | Between l ->
              Option.may (function
                | Ast.DestinationPlayer p ->
                  raise (SelfRelation (p, element_name))
                | _ -> assert false) (Utils.is_uniq_witness l) ;
              List.fold_left (fun kinds (p1, p2) -> add p1 p2 kinds) kinds
                (List.filter (fun (p1, p2) -> p1 <> p2) (Utils.list_square l l))
            | FromTo (p1, p2) -> add p1 p2 kinds) kinds block.provide_contact in
        kinds in
      let translation =
        let translations =
          if block.translation <> [] && block.sentence <> [] then
            raise (TranslationError ("translation item around sentences",
                                     event_name, List.hd block.translation)) ;
          if phantom then (
            if block.translation <> [] then
              raise (TranslationError ("translation in phantom event",
                                       event_name, List.hd block.translation)) ;
            if block.sentence <> [] then
              raise (UnexpectedCommandInBlock (event_name,
                                               "sentence in phantom event")) ;
            []
          ) else (
            if block.translation <> [] then [block.translation]
            else
              List.map (fun block ->
                  (convert_block event_name [Translation] block).translation)
                block.sentence
          ) in
        let translation =
          Translation.sadd Translation.sempty Translation.generic [] (-1)
            [Translation.Direct event_name] in
        (List.length translations,
         Utils.list_fold_lefti (fun i translation sentence ->
           List.fold_left (fun translation (lg, commands, tr) ->
               let tr = List.map (Translation.sitem_map get_player_array) tr in
               Translation.sadd translation lg commands i tr)
             translation sentence) translation translations) in
      let (constraints_none, constraints_some) =
        List.fold_left (fun (none, some) c ->
            let (mns, bns) =
              if c.Ast.constraint_any then
                (some, fun some -> (none, some))
              else
                (none, fun none -> (none, some)) in
            let (fba, uba) =
              if c.Ast.constraint_after then
                (snd, fun ba after -> (fst ba, after))
              else
                (fst, fun ba before -> (before, snd ba)) in
            let k =
              match c.Ast.constraint_kind with
              | Ast.Kind k ->
                let k =
                  match Id.get_id st.event_names k with
                  | None -> raise (Undeclared ("event kind", k, element_name))
                  | Some id -> id in
                PSet.singleton (Events.kind_of_id k)
              | Ast.KindAttribute a ->
                PSet.singleton (Events.kind_of_attribute
                  (get_attribute_id (module Attribute.PlayerAttribute) a))
              | Ast.KindContact (c, l) ->
                let c = get_attribute_id (module Attribute.ContactAttribute) c in
                PSet.from_list (List.map (fun p ->
                  let p = get_player_array p in
                  Events.kind_of_contact c p) l) in
            bns (List.fold_left (fun m p ->
                    let p = get_player_array p in
                    let ba =
                      try PMap.find p m
                      with Not_found -> (PSet.empty, PSet.empty) in
                    PMap.add p (uba ba (PSet.merge k (fba ba))) m) mns
                  c.Ast.constraint_players)) (PMap.empty, PMap.empty)
          block.event_constraint in
      let attendees = List.map Id.to_array attendees in {
        Events.event_blocking = blocking ;
        Events.event_phantom = phantom ;
        Events.event_id = event_id () ;
        Events.event_type = t ;
        Events.event_attendees = PSet.from_list attendees ;
        Events.all_attendees = Utils.seq nb_players ;
        Events.event_kinds = kinds ;
        Events.constraints_none = constraints_none ;
        Events.constraints_some = constraints_some ;
        Events.translation = translation
      }) events in
  (** We check that there is no contradiction within these events. *)
  let rec check f acc = function
    | [] -> None
    | e :: l ->
      match PSet.fold (fun c -> function
                | Utils.Left newacc ->
                  let k =
                    try PMap.find c e.Events.event_kinds
                    with Not_found -> PSet.empty in
                  let constr =
                    try PMap.find c e.Events.constraints_none
                    with Not_found -> (PSet.empty, PSet.empty) in
                  let constr = f constr in
                  let previous_kinds =
                    try PMap.find c acc
                    with Not_found -> PSet.empty in
                  if PSet.is_empty (PSet.inter constr previous_kinds) then
                    Utils.Left (PMap.add c (PSet.merge k previous_kinds) newacc)
                  else Utils.Right c
                | r -> r)
              (Utils.Left acc) e.Events.event_attendees with
      | Utils.Right c ->
        Some (Events.print_event e ^ ", " ^ get_player_name (Id.from_array c))
      | Utils.Left acc -> check f acc l in
  let check direction f acc evs =
    match check f acc evs with
    | Some name ->
      raise (UnsatisfyableEventSequence (name ^ ", " ^ direction))
    | None -> () in
  check "forward" fst PMap.empty evs ;
  check "backward" snd PMap.empty (List.rev evs) ;
  ({ Element.status = status ;
     Element.players = elementBase ;
     Element.others = !otherPlayers ;
     Element.objects = elementObjects ;
     Element.events = evs ;
     Element.id = element_id () }, deps)

let parse i =
  (** We first compute all the dependencies. *)
  let i =
    let st = i.current_state in
    let categories =
      try CategoryGraph.get_all_dependencies i.category_graph
      with CategoryGraph.CircularDependency c ->
        let c = Utils.assert_option __LOC__ (Id.map_inverse st.category_names c) in
        raise (CircularDependency ("category", c)) in
    let st = { st with category_dependencies = categories } in
    let update m =
      PMap.map (fun s ->
        PSet.merge
          (PSet.flat_map (fun c ->
           try PMap.find c categories
           with Not_found -> assert false) s) s) m in
    let st =
      { st with
          attribute_dependencies = update st.attribute_dependencies ;
          constructor_dependencies = update st.constructor_dependencies ;
          object_dependencies = update st.object_dependencies ;
          event_dependencies = update st.event_dependencies } in
    let events =
      try CategoryGraph.get_all_dependencies i.event_graph
      with CategoryGraph.CircularDependency e ->
        let e = Utils.assert_option __LOC__ (Id.map_inverse st.event_names e) in
        raise (CircularDependency ("event", e)) in
    let st = { st with event_event_dependencies = events } in
    { i with current_state = st } in
  (** We then parse all the elements. *)
  let (elements, elements_dependencies, import_information) =
    List.fold_left (fun (elements, elements_dependencies, import_information)
        (id, status, name, block) ->
          let (element, deps) = parse_element i.current_state name status block in
          let import_information =
            List.fold_left (fun import_information ev ->
                let name = Events.print_event ev in
                let id = ev.Events.event_id in
                { import_information with
                    event_id = PMap.add name id import_information.event_id ;
                    event_informations =
                      PMap.add id (ev.Events.event_phantom,
                                   ev.Events.event_blocking,
                                   ev.Events.translation)
                        import_information.event_informations ;
                    event_kinds =
                      PMap.add id ev.Events.event_kinds
                        import_information.event_kinds })
              import_information element.Element.events in
          (PMap.add id element elements,
           PMap.add id deps elements_dependencies,
           import_information))
      (i.current_state.elements,
       i.current_state.elements_dependencies,
       i.current_state.import_information) i.waiting_elements in
  { i.current_state with
      elements_dependencies = elements_dependencies ;
      elements = elements ;
      import_information = import_information }

let get_translations s = s.translations

let get_import_information s = s.import_information

let get_constructor_maps s = s.import_information.constructor_maps

let elements s = s.elements

let get_element_name s =
  Id.map_inverse s.element_names

let get_element_dependencies s e =
  try PMap.find e s.elements_dependencies
  with Not_found -> assert false

let get_all_elements s lg cats maxPlayers =
  PMap.foldi (fun e deps el ->
    if PSet.for_all (fun c -> PSet.mem c cats) deps then (
      let element = PMap.find e s.elements in
      let l = Array.length element.Element.players in
      if l <= maxPlayers && Element.is_translatable element lg then e :: el else el)
    else el) s.elements_dependencies []

let total_number_of_elements s =
  Enum.count (PMap.enum s.elements)

let number_of_elements s lg =
  Enum.count (Enum.filter (fun (_, e) -> Element.is_translatable e lg) (PMap.enum s.elements))

