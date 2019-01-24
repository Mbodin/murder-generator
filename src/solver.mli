(** Module Solver
 * Iterate over story elements. **)

(** Evaluates the provided relation state with respect to the target
 * difficulty and simplicity given to each player.
 * The higher, the better the grade.
 * Note that most grades are negative. **)
val evaluate : State.relation_state -> int

(*
val solver_step : State.t -> (Generator.t * bool) Utils.BidirectionalList.t -> State.t * (Generator.t * bool) Utils.BidirectionalList.t
(** Uses the given generator to update the state and the generator list.
 * Each generator is associated a boolean to know whether its last call was
 * successful (or that it has not yet been called): when every generator
 * is associated the value false, it is time to stop. **)
*)

(** Runs the solver until no one generator are left. **)
val solver : (*TODO: all the constructors. State.constructor_map ->*) State.t -> Element.t list -> State.t

