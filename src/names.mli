(** Module Names
 * This module deals with name generation. **)

(** The type of name generator. **)
type t

(** Name generator may also generate properties, that is, attribute constructors
 * associated to the name. **)
type property = Attribute.PlayerAttribute.constructor

(** A dummy generator that always generate the empty name. **)
val empty : t

(** Given the content of a name file, returns a name generator. **)
val import : Attribute.PlayerAttribute.constructor_map -> string -> t

(** Generate a random name.
 * It takes as an argument a set of properties that the name has to be compatible with
 * and returns a list of properties that the name additionally fulfill. **)
val generate : t -> property PSet.t -> string * property list

(** Translate the generator. **)
val translate : t -> unit Translation.t

(** State whether this generator is marked as default for this particular language. **)
val is_default : t -> Translation.language -> bool

