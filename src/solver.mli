(** Module Solver
 * Iterate over story elements. **)

(** A global register of existing elements and meta-informations. **)
type global

(** An empty register.
 * It takes a parameter between [0.] and [1.] indicate how much power can be
 * placed in solving the constraints. **)
val empty_global : float -> global

(** Register that an element exists. **)
val register_element : global -> Element.t -> global

(** Only conserves from the registered elements the one satisfying
 * the given predicate. **)
val filter_elements : global -> (Element.t -> bool) -> global

(** The target difficulty and simplicity measures for each player.
 * See the Relation module for more information. **)
type objective = {
    difficulty : int ;
    complexity : int
  }

(** Evaluates the provided relation state with respect to the target
 * difficulty and simplicity (an objective for each player of the state).
 * The higher, the better the grade.
 * Note that most grades are negative. **)
val evaluate : objective array -> State.relation_state -> int

(** Same as [evaluate], but takes a state as argument instead of a
 * relationnal state. **)
val evaluate_state : objective array -> State.t -> int

(** Tries to optimise the state to make it match the objectives. **)
val solve : global -> State.t -> objective array -> State.t Lwt.t

