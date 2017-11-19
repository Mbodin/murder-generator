
type character = State.character

type frequency =
  | Always
  | Between of int * int

type effect = TODO

type t =
  frequency * effect

type usage_flag =
  | Reuse_as_soon_as_possible
  | Reuse_later
  | Do_not_reuse

let apply_effect e s c cl = TODO

let apply_frequency = function
  | Always -> (Reuse_as_soon_as_possible, Always)
  | Between (min, max) ->
    if max <= 1 then
      (Do_not_reuse, Between (0, 0))
    else if min > 1 then
      (Reuse_as_soon_as_possible, Between (min - 1, max - 1))
    else if Utils.rand 1 max = 1 then
      (Do_not_reuse, Between (0, 0))
    else (Reuse_later, Between (1, max - 1))

let apply (f, e) s c cl =
  let (cl, hl, e) = apply_effect e s c cl in
  let (u, f) = apply_frequency f in
  (cl, hl, u, (f, e))

