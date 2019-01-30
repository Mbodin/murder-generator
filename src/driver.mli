(** Module Driver.
 * This module treats the abstract syntax tree produced by the Parser module
 * into something that can be used later on. **)

(** A type to store intermediate informations about parsing. **)
type intermediary

(** Treating the data sent from the parser is done in two phases.
 * The first phase is to assimilate each globally declared names
 * into identifiers.
 * It expects the previously computed [intermediary] type. **)
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

(** Once all files have been registered, categories can be listed. **)
val all_categories : intermediary -> Utils.Id.t list

(** Takes a category identifier as argument as returns the list of categories
 * that this category depends on. **)
val get_category_dependencies : intermediary -> Utils.Id.t -> Utils.Id.t list

(** Takes a category identifier and a language and returns the name
 * of this category in this language (or [None] if this language has
 * not been translated). **)
val translates_category : intermediary -> Utils.Id.t -> string -> string option

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

