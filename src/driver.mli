(** Module Driver.
 * This module treats the abstract syntax tree produced by the Parser module
 * into something that can be used later on. **)

(** A type to store intermediate informations about parsing. **)
type intermediary

(** States that the block named as a the first string has an invalid
 * command in its block, which is described by the second string. **)
exception UnexpectedCommandInBlock of string * string

(** An error indicating that something of kind described by the first
 * string has been defined twice, the second string being its name. **)
exception DefinedTwice of string * string

(** An error indicating that there is a loop in category dependencies. **)
exception CircularDependency of string

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
 * one can treat the full data and produce a final state. **)
val parse : intermediary -> state

(** This map stores the identifiers of each element. **)
val elements : state -> Element.t Utils.Id.map

(** Takes an element identifier as argument as returns the list of categories
 * that this element depends on. **)
val get_element_dependencies : state -> Utils.Id.t -> Utils.Id.t list

(** Returns all defined elements in this state that are compatible with
 * the chosen set of categories (given as identifiers). **)
val get_all_elements : state -> Utils.Id.t Utils.PSet.t -> Utils.Id.t list

(** Once the state has been built, categories can be listed. **)
val all_categories : state -> Utils.Id.t list

(** Takes a category identifier and returns the list of categories
 * that this category depends on. **)
val get_category_dependencies : state -> Utils.Id.t -> Utils.Id.t list

(** Takes an attribute and returns the list of categories
 * that this attribute depends on. **)
val get_attribute_dependencies : state -> State.attribute -> Utils.Id.t list

(* TODO: The same for constructors.
 * (* TODO: The equivalent of State.attribute, but with attribute constructors
 *    (whose dependencies might depend on their associated attribute!) *) *)

(** Takes an element identifier and returns the list of categories
 * that this element depends on. **)
val get_element_dependencies : state -> Utils.Id.t -> Utils.Id.t list

(** Returns the translations for categories. **)
val translates_category : state -> Utils.Id.t Translation.t

