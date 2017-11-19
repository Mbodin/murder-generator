(** Module State
 * Describes the state of the solver. **)

type character = History.character

type relation_state
(** A mapping from pairs of characters to Relation.t. **)

val get_relation_state : relation_state -> character -> character -> Relations.t
(** Returns the relation between the two characters.
 * The relations are usually symmetrical, but note how the asymmetrical
 * relation between c1 and c2 is represented by Asymmetrical (r1, r2)
 * where r1 is the relation from the point of view of c1.
 * If a mapping is not present, it returns the default relation
 * Relations.Basic Relations.Neutral. *)

val write_relation_state : relation_state -> character -> character -> Relations.t -> unit
(** Non-functionally update the relation state. **)

(** The following exception is returned if one tries to write or read
 * a relation between two identical characters. **)
exception SelfRelation

val create_relation_state : int -> relation_state
(** Creates an empty relation state for the given number n of characters,
 * each indexed from 0 to n - 1. **)

type t =
  relation_state * History.state

val get_relation : t -> character -> character -> Relations.t

val write_relation : t -> character -> character -> Relations.t -> unit

val create_state : int -> t
(** Creates an empty state for the given number n of characters,
 * each indexed from 0 to n - 1. **)

