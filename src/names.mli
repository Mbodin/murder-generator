(** Module Names
 * This module deals with name generation. **)

(** The type of name generator. **)
type t

(** A dummy generator that always generate the empty name. **)
val empty : t

(** Given the content of a name file, returns a name generator. **)
val import : string -> t

(** Generate a random name. **)
val generate : t -> string

(** Translate the generator. **)
val translate : t -> unit Translation.t

(** States whether this generator is marked as default for this particular language. **)
val is_default : t -> Translation.language -> bool

