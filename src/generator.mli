(** Module Generator
 * Generates some new relations between characters. **)

(* FIXME: This file may disappear at some point. *)

type character = State.character

(** The type of generators. **)
type t

(** Generators communicate with the solver by stating when they should
 * be called again. **)
type usage_flag =
  | Reuse_as_soon_as_possible (** If possible reuse this generator in the next iteration. **)
  | Reuse_later (** Places this generator at the end of the generator list. **)
  | Do_not_reuse (** Remove this generator from the generator list. **)

val apply : t -> State.t -> character -> character list -> (character * character * Relations.t) list * (character * History.t) list * usage_flag * t
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
 * The flag indicates when this generator should be used again.
 * Generators may carry internal variables to be updated: the returned
 * generator is used for this. **)

