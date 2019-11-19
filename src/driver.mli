(** Module Driver.
 * This module treats the abstract syntax tree produced by the Parser module
 * into something that can be used later on. **)

(** From the file name and the lexing buffer, calls the parser. **)
val parse_lexbuf : string -> Lexing.lexbuf -> Ast.declaration list

(** Parses a relation obtained through [Relation.to_string]. **)
val parse_relation : string -> Relation.t

(** A type to store intermediate informations about parsing. **)
type intermediary

(** States that the block named as the first string has an invalid
 * command in its block, which is described by the second string. **)
exception UnexpectedCommandInBlock of string * string

(** An error indicating that something of kind described by the first
 * string has been defined twice, the second string being its name,
 * and the third describe its enclosing block. **)
exception DefinedTwice of string * string * string

(** An error indicating that something of kind described by the first
 * string has been used without being declared, the second string being its name,
 * and the third describe its enclosing block. **)
exception Undeclared of string * string * string

(** An error indicating that there is a loop definitions.
 * The first argument is the definition kind, the second its name. **)
exception CircularDependency of string * string

(** An error indicating that a player has been associated a relation to
 * itself in the given element. **)
exception SelfRelation of string * string

(** An unexpected translation item has been given in an element of the kind
 * described by the first argument, whose name is the second argument. **)
exception TranslationError of string * string * Ast.translation

(** The given element can’t be applied (typically because some constraints
 * are incompatible). **)
exception VacuumElement of string

(** If a sequence of event is not satisfyable in an element, report the element.
 * A sequence of event may not be satisfyable if for instance an event prevents
 * an event of a given kind to appear after it, but that this precise one actually
 * appears in the event. **)
exception UnsatisfyableEventSequence of string

(** Treating the data sent from the parser is done in two phases.
 * The first phase is to assimilate each globally declared names
 * into identifiers.
 * It expects the previously computed [intermediary] type.
 * This function might throw [UnexpectedCommandInBlock] or [DefinedTwice]. **)
val prepare_declarations : intermediary -> Ast.declaration list -> intermediary

(** An initial empty [intermediary] element. **)
val empty_intermediary : intermediary

(** States whether the [intermediary] type is waiting for other definitions
 * to be complete.
 * This function is usually called once each file have been loaded and merged
 * into an [intermediate]: if this function then returns [false], an error
 * has been made somewhere. **)
val is_intermediary_final : intermediary -> bool

(** Return a set of missing categories. **)
val categories_to_be_defined : intermediary -> string PSet.t

(** Return a set of missing events. **)
val events_to_be_defined : intermediary -> string PSet.t

(** Return a set of missing attributes and contacts. **)
val attributes_to_be_defined : intermediary -> (string PSet.t * string PSet.t)

(** Return a set of missing constructors for attributes and contacts.
 * The first element of each couple is the attribute name, then the constructor
 * name. **)
val constructors_to_be_defined : intermediary -> ((string * string) PSet.t * (string * string) PSet.t)

(** Return a set of missing tags.
 * The first element of each couple is the language code, then the tag. **)
val tags_to_be_defined : intermediary -> (string * string) PSet.t

(** This type stores all the informations returned by [parse]. **)
type state

(** Once each file have been parsed and put into a single [intermediary] result,
 * one can treat the full data and produce a final state.
 * This function might throw any of the exceptions defined in this file. **)
val parse : intermediary -> state

(** This map stores the identifiers of each element. **)
val elements : state -> (Id.t, Element.t) PMap.t

(** Given an element identifier, returns its name. **)
val get_element_name : state -> Id.t -> string option

(** Takes an element identifier as argument as returns the set of categories
 * that this element depends on. **)
val get_element_dependencies : state -> Id.t -> Id.t list

(** Return all defined elements in this state that are compatible with
 * the provided language, the chosen set of categories (given as
 * identifiers) and at most this number of players. **)
val get_all_elements : state -> Translation.language -> Id.t PSet.t -> int -> Id.t list

(** Once the state has been built, categories can be listed. **)
val all_categories : state -> Id.t list

(** Returns the number of elements defined in the state. **)
val total_number_of_elements : state -> int

(** Returns the number of elements available for a particular language. **)
val number_of_elements : state -> Translation.language -> int

(** Takes a category identifier and returns the list of categories
 * that this category depends on. **)
val get_category_dependencies : state -> Id.t -> Id.t PSet.t

(** Takes an attribute and returns the list of categories
 * that this attribute depends on. **)
val get_attribute_dependencies : state -> Attribute.attribute -> Id.t PSet.t

(** Takes a constructor and returns the list of categories
 * that this constructor depends on. **)
val get_constructor_dependencies : state -> Attribute.constructor -> Id.t PSet.t

(** Takes an element identifier and returns the list of categories
 * that this element depends on. **)
val get_element_dependencies : state -> Id.t -> Id.t PSet.t

(** Return the translations for elements. **)
val get_translations : state -> Translation.element

(** Return the constructor maps generated while loading the files. **)
val get_constructor_maps : state -> Attribute.constructor_maps

(** Necessary information to import a state. **)
type import_information = {
    constructor_maps : Attribute.constructor_maps
      (** Information about constructors. **) ;
    event_id : (string, Id.t) PMap.t
      (** A unique identifier for each event. **) ;
    event_informations : (Id.t, bool * bool * int Events.translation) PMap.t
      (** Information for each event, organised over event names.
       * In particular:
       * - whether it is a phantom event,
       * - whether it is a blocking event,
       * - how it is translated. **) ;
    event_kinds : (Id.t, (int, int Events.kind PSet.t) PMap.t) PMap.t
      (** The kind associated to each event, organised over event names. **)
  }

(** Return information for importation. **)
val get_import_information : state -> import_information

