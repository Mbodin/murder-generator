
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

let apply_frequency success = function
  | Always -> (Reuse_as_soon_as_possible, Always)
  | Between (min, max) ->
    if max <= (if success then 1 else 0) then
      (Do_not_reuse, Between (0, 0))
    else if min > 1 then
      (Reuse_as_soon_as_possible,
        let (min, max) =
          if success then (min - 1, max - 1) else (min, max) in
        Between (min, max))
    else if Utils.rand 1 max = 1 then
      (Do_not_reuse, Between (0, 0))
    else (Reuse_later, Between (1, if success then max - 1 else max))

let apply (f, e) s c cl =
  let (cl, hl, e, success) = apply_effect e s c cl in
  let (u, f) = apply_frequency success f in
  (cl, hl, u, (f, e))

