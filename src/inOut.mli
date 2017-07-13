(** Module InOut
 * Contains functions to input or output messages.
 * This module typically comes in various version depending on the context where the program is executed. **)


(** Print a message on the console. **)
val message : string -> unit

(** If a place of the program should never be executed, call this function instead of assert false and return a default value: this will help users declare a bug. **)
val should_not_happen : string -> unit

(** A type to change already written messages. **)
type t

(** Print a message on a new line, and return an object to change the message. **)
val print : string -> t

(** Update the message adressed by the object. **)
val update : t -> string -> unit

(** Print more than one messages on the same line, each of which is associated with an object. **)
val prints : string list -> t list

