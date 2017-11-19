(** Module Generator
 * Generates some new relations between characters. **)

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

type state =
    relation_state * History.state

val get_relation : state -> character -> character -> Relations.t

val write_relation : state -> character -> character -> Relations.t -> unit

val create_state : int -> state
(** Creates an empty state for the given number n of characters,
 * each indexed from 0 to n - 1. **)

(** Generators communicate with the solver by stating when they should
 * be called again. **)
type usage_flag =
  | Reuse_as_soon_as_possible (** If possible reuse this generator in the next iteration. **)
  | Reuse_later (** Places this generator at the end of the generator list. **)
  | Do_not_reuse (** Remove this generator from the generator list. **)

type generator =
  state -> character -> character list -> (character * character * Relations.t) list * (character * History.t) list * usage_flag
(** The type of relation generator.
 * It takes a character, which is usually a character whose current relation
 * complexity and difficulty is far from what the associated player expects.
 * It also takes a list of character to be used in priority in relations
 * (because their own complexity and difficulty are also far from from the
 * target complexity and difficulty).
 * It then returns a list a relations to be added, as well as some history
 * associated to this relation.
 * Note that these relations won’t necessary be applied: if the solver
 * considers these relations to be too complex in combination to the current
 * ones, or that the histories are incompatibles with the current history of
 * players, the solver might ask an other generator.
 * The final flag indicates when this generator should be used again. **)

val solver_step : state -> (generator * bool) Utils.two_direction_list -> state * (generator * bool) Utils.two_direction_list
(** Uses the given generator to update the state and the generator list.
 * Each generator is associated a boolean to know whether its last call was
 * successful (or that it has not yet been called): when every generator
 * is associated the value false, it is time to stop. **)

val solver : state -> generator list list -> state
(** Run the solver until no one generator are left. **)

