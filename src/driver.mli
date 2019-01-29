(** Module Driver.
 * This module treats the abstract syntax tree produced by the Parser module
 * into something that can be used later on. **)

(** Treating the data sent from the parser is done in two phases.
 * The first phase is to assimilate each globally declared names
 * into identifiers.
 * It is possible to perform this computation file by file. **)
val get_declarations : Ast.declaration list -> unit

(** Once all files have been registered, categories can be listed. **)
val all_categories : unit -> Utils.Id.t list

(** Takes a category identifier as argument as returns the list of categories
 * that this category depends on. **)
val get_category_dependencies : Utils.Id.t -> Utils.Id.t list

(** Takes a category identifier and a language and returns the name
 * of this category in this language (or [None] if this language has
 * not been translated). **)
val translates_category : Utils.Id.t -> string -> string option

(** Removes all declarations that depends on categories not listed
 * in the argument set. **)
val filter_declarations : Utils.Id.t Utils.PSet.t -> Ast.declaration list -> Ast.declaration list

(** Once the declarations have been filtered, we can register each constructors. **)
val register_constructors : Ast.declaration list -> unit

(** This type stores all the informations returned by [parse]. **)
type state

(** Once each constructors have been registered, one can parse
 * the abstract syntax tree. **)
val parse : Ast.declaration list -> state

(** Returns all defined elements in this state. **)
val get_all_elements : state -> Utils.Id.t list

(** This map stores the identifiers of each element.
 * It must be called after all calls to [get_declarations] have been done. **)
val elements : state -> Element.t Utils.Id.map

(** Takes an element identifier as argument as returns the list of categories
 * that this element depends on. **)
val get_element_dependencies : state -> Utils.Id.t -> Utils.Id.t list

