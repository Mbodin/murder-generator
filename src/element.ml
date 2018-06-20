
type character = Utils.Id.t

type character_constraint =
  | Attribute of State.attribute * State.value
  | Contact of State.contact * character * State.contact_value

type element =
  (player_constraint list
   * (State.attribute * State.value) list
   * (State.contact * int * State.contact_value) list
   * History.event list
   * Relations.t array
  ) array

let compatible_and_progress state e i =
  Array.fold_left (function
    | None -> fun _ -> None
    | Some b -> fun i ->
        TODO) (Some false) i

let apply state e i =
  TODO

