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

(** Evaluates the provided relation state with respect to the target
 * difficulty and simplicity (an objective for each player of the state).
 * The higher, the better the grade.
 * Note that most grades are negative. **)
val evaluate : State.objective array -> State.relation_state -> int

(** Same as [evaluate], but takes a state as argument instead of a
 * relationnal state. **)
val evaluate_state : State.objective array -> State.t -> int

(** Tries to optimise an empty state to make it match the objectives.
 * It features a pausing function returning a [Lwt.t] value called regularly.
 * This pausing function is given a float as an argument: when it reaches [1.], it means
 * that the solver is very close to a solution. **)
val solve : (float -> unit Lwt.t) -> global -> State.objective array -> State.t Lwt.t

(** As for [solve], but it takes an initial state and some information about
 * what needs to be changed. **)
val solve_with_difference : (float -> unit Lwt.t) -> global -> State.t -> Element.attribute_differences -> State.objective array -> State.t Lwt.t

