(** Module Solver
 * Iterate over generators. **)

(*
val solver_step : State.t -> (Generator.t * bool) Utils.BidirectionalList.t -> State.t * (Generator.t * bool) Utils.BidirectionalList.t
(** Uses the given generator to update the state and the generator list.
 * Each generator is associated a boolean to know whether its last call was
 * successful (or that it has not yet been called): when every generator
 * is associated the value false, it is time to stop. **)
*)

(** Runs the solver until no one generator are left. **)
val solver : State.constructor_map -> State.t -> Element.t list -> State.t

