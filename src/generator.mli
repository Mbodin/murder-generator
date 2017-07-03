(** Module Generator
 * Generates some new relations between characters. **)

type character = int

type relation_state
(** A mapping from pairs of characters to Relation.t.
 * The pairs are always ordered, with the smallest character first: the relations
 * are usually symmetrical, and the asymetrical relation betweeen c1 and c2 is
 * represented by Asymetrical (r1, r2) where r1 is the relation from the point of
 * view of c1.
 * If a mapping is not present, it returns the default relation Relation.Basic Relation.Neutral. *)

type state =
    relation_state * History.state

type generator : state -> character -> character list -> (character * character * Relation.t) list * (character * History.t) list
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

