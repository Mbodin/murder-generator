
type basic =
    | Neutral
    | Hate
    | Trust
    | Chaotic
    | Undetermined

type t =
    | Basic of basic
    | Asymetrical of basic * basic
    | Explosive of t * t

let basic_complexity = function
    | Neutral -> 0
    | Hate -> 1
    | Trust -> 1
    | Chaotic -> 1
    | Undetermined -> 2

let rec complexity = function
    | Basic r -> basic_complexity r
    | Asymetrical (r1, r2) -> max (complexity r1) (complexity r2)
    | Explosive (r1, r2) -> 1 + complexity r1 + complexity r2

let basic_difficulty = function
    | Neutral -> 0
    | Hate -> 2
    | Trust -> -1
    | Chaotic -> 1
    | Undetermined -> 1

let rec difficulty = function
    | Basic r -> basic_difficulty r
    | Asymetrical (Basic Neutral, r) | Asymetrical (r, Basic Neutral) -> difficulty r - 1
    | Asymetrical (r1, r2) -> max (difficulty r1) (difficulty r2)
    | Explosive (r1, r2) -> 1 + difficulty r1 + difficulty r2

let is_explosive = function
    | Explosive (_, _) -> true
    | _ -> false

