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
 * and the third an optional position. **)
exception DefinedTwice of string * string * string option

(** An error indicating that something of kind described by the first
 * string has been used without being declared, the second string being its name,
 * and the third an optional position. **)
exception Undeclared of string * string * string option

(** An error indicating that there is a loop in category dependencies. **)
exception CircularDependency of string

(** An error indicating that a player has been associated a relation to
 * itself in the given element. **)
exception SelfRelation of string * string

(** An unexpected translation item has been given in an element of the kind
 * described by the first argument, whose name is the second argument. **)
exception TranslationError of string * string * Ast.translation

(** The given element can’t be applied (typically because some constraints
 * are incompatible). **)
exception VacuumElement of string

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

(** Returns a set of missing categories. **)
val categories_to_be_defined : intermediary -> string Utils.PSet.t

(** Returns a set of missing attributes and contacts. **)
val attributes_to_be_defined : intermediary -> (string Utils.PSet.t * string Utils.PSet.t)

(** This type stores all the informations returned by [parse]. **)
type state

(** Once each file have been parsed and put into a single [intermediary] result,
 * one can treat the full data and produce a final state.
 * This function might throw any of the exceptions defined in this file. **)
val parse : intermediary -> state

(** This map stores the identifiers of each element. **)
val elements : state -> (Utils.Id.t, Element.t) PMap.t

(** Takes an element identifier as argument as returns the set of categories
 * that this element depends on. **)
val get_element_dependencies : state -> Utils.Id.t -> Utils.Id.t list

(** Returns all defined elements in this state that are compatible with
 * the chosen set of categories (given as identifiers) and at most this
 * number of players. **)
val get_all_elements : state -> Utils.Id.t Utils.PSet.t -> int -> Utils.Id.t list

(** Once the state has been built, categories can be listed. **)
val all_categories : state -> Utils.Id.t list

(** Takes a category identifier and returns the list of categories
 * that this category depends on. **)
val get_category_dependencies : state -> Utils.Id.t -> Utils.Id.t Utils.PSet.t

(** Takes an attribute and returns the list of categories
 * that this attribute depends on. **)
val get_attribute_dependencies : state -> State.attribute -> Utils.Id.t Utils.PSet.t

(** Takes a constructor and returns the list of categories
 * that this constructor depends on. **)
val get_constructor_dependencies : state -> State.constructor -> Utils.Id.t Utils.PSet.t

(** Takes an element identifier and returns the list of categories
 * that this element depends on. **)
val get_element_dependencies : state -> Utils.Id.t -> Utils.Id.t Utils.PSet.t

(** Returns the translations for elements. **)
val get_translations : state -> Translation.element

(** Return the constructor maps generated while loading the files. **)
val get_constructor_maps : state -> State.constructor_maps

