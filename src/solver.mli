(** Module Solver
 * Iterate over story elements. **)

(** A global register of existing elements and meta-informations. **)
type global

(** An empty register. **)
val empty_global : global

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

(*
val solver_step : State.t -> (Generator.t * bool) Utils.BidirectionalList.t -> State.t * (Generator.t * bool) Utils.BidirectionalList.t
(** Uses the given generator to update the state and the generator list.
 * Each generator is associated a boolean to know whether its last call was
 * successful (or that it has not yet been called): when every generator
 * is associated the value false, it is time to stop. **)
*)

(** Runs the solver until no one generator are left. **)
val solver : global -> State.t -> objective array -> State.t

