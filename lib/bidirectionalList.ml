
type 'a t = 'a list * 'a list
(** (ll, lr) represents the list ll @ List.rev lr. **)

let from_list l = (l, [])
let to_list (ll, lr) = ll @ List.rev lr

let empty = from_list []

let is_empty = function
  | ([], []) -> true
  | _ -> false

let length (ll, lr) = List.length ll + List.length lr

let add_left e (ll, lr) = (e :: ll, lr)
let add_right (ll, lr) e = (ll, e :: lr)

let match_left = function
  | (e :: ll, lr) -> Some (e, (ll, lr))
  | ([], lr) ->
    match List.rev lr with
    | [] -> None
    | e :: ll -> Some (e, (ll, []))

let match_right = function
  | (ll, e :: lr) -> Some ((ll, lr), e)
  | (ll, []) ->
    match List.rev ll with
    | [] -> None
    | e :: lr -> Some (([], lr), e)

let for_all p (ll, lr) =
  List.for_all p ll && List.for_all p lr

let exists p (ll, lr) =
  List.exists p ll || List.exists p lr

let filter f (ll, lr) = (List.filter f ll, List.filter f lr)

