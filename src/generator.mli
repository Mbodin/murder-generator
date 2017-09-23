(** Module Generator
 * Generates some new relations between characters. **)

type character = int

type relation_state
(** A mapping from pairs of characters to Relation.t. **)

val get_relation : relation_state -> characters -> characters -> Relations.t
(** Returns the relation between the two characters.
 * The relations are usually symmetrical, but note how the asymetrical
 * relation betweeen c1 and c2 is represented by Asymetrical (r1, r2)
 * where r1 is the relation from the point of view of c1.
 * If a mapping is not present, it returns the default relation
 * Relations.Basic Relations.Neutral. *)

val write_relation : relation_state -> characters -> characters -> Relations.t -> unit
(** Non-functionnaly update the relation state. **)

(** The following exception is returned if one tries to write or read
 * a relation between two identical characters. **)
exception SelfRelation

val create_relation_state : int -> relation_state
(** Creates an empty relation state for the given number n of characters,
 * each indexed from 0 to n - 1. **)

type state =
    relation_state * History.state

type generator : state -> character -> character list -> (character * character * Relations.t) list * (character * History.t) list
(** A relation generator.
 * It takes a character, which is usually a character whose current relation
 * complexity and difficulty is far from what the associated player expects.
 * It also takes a list of character to be prioritarily used in relations
 * (because their own complexity and difficulty are also far from from the
 * target complexity and difficulty).
 * It then returns a list a relations to be added, as well as some history
 * associated to this relation.
 * Note that these relations won’t necessary be applied: if the solver
 * considers these relations to be too complex in combinaison to the current
 * ones, or that the histories are incompatibles with the current history of
 * players, the solver might ask an other generator. **)

