
let _ = Random.self_init ()

let id x = x

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

type ('a, 'b) plus =
  | Left of 'a
  | Right of 'b

let error_monad o f =
  match o with
  | Left e -> Left e
  | Right v -> f v

let rec unfold f i =
  match f i with
  | None -> []
  | Some (v, j) ->
      v :: unfold f j

let seq i =
  (* We assume i >= 0. *)
  unfold (fun j ->
    if i = j then None
    else Some (j, j + 1)) 0

exception NegativeWeigth
exception InternalError

let select l =
  let s = List.fold_left (+) 0 (List.map fst l) in
  if s <= 0 then raise NegativeWeigth
  else
    let rec search t = function
      | [] -> raise InternalError
      | (p, v) :: l ->
        if p >= t then v
        else search (t - p) l
    in search (Random.int s) l

let select_any l =
  List.nth l (Random.int (List.length l))

let sum = List.fold_left (+) 0
let array_sum = Array.fold_left (+) 0

let array_count f = Array.fold_left (fun v x -> v + if f x then 1 else 0) 0


type idt = int

let new_id_function () =
  let current = ref (-1) in
  fun () ->
    incr current ;
    !current

let new_id = new_id_function ()

