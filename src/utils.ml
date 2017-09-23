
let _ = Random.self_init ()

let id x = x

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

let if_option = function
  | None -> fun _ -> None
  | Some x -> fun f -> f x

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

let idt_to_array = id


type _ idt_map =
  | Idt_map : ('a, idt) PMap.t * (unit -> idt) -> 'a idt_map
  | Idt_int : idt idt_map (* No need to create new identifiers for integers! *)

let idt_map_create _ =
  Idt_map (PMap.empty, (new_id_function ()))

let idt_idt_map_create =
  Idt_int
let idt_int_map_create =
  idt_idt_map_create

let idt_map_insert_idt (type a) : a idt_map -> a -> idt * a idt_map = function
  | Idt_map (m, f) -> fun o ->
    let i = f () in
    (i, Idt_map (PMap.add o i m, f))
  | Idt_int -> fun i ->
    (i, Idt_int)

let idt_map_insert m e =
  snd (idt_map_insert_idt m e)

let get_id (type a) : a idt_map -> a -> idt option = function
  | Idt_map (m, f) -> fun o ->
    (try Some (PMap.find o m)
     with Not_found -> None)
  | Idt_int -> fun i ->
    Some i


type 'a union_find =
  'a idt_map * (idt, idt) PMap.t

let create_union_find _ = idt_map_create (), PMap.empty

let create_union_find_idt = idt_idt_map_create, PMap.empty
let create_union_find_int = create_union_find_idt

let insert_idt (m, p) e =
  let (i, m) =
    match get_id m e with
    | None ->
      idt_map_insert_idt m e
    | Some i -> (i, m)
  in (i, (m, PMap.add i i p))

let insert mp e =
  snd (insert_idt mp e)

let find (m, p) e =
  let rec aux p i =
    let pi = PMap.find i p in
    if i = pi then
      (i, (m, p))
    else let (pi', (m, p)) = aux p pi in
      (pi', (m, PMap.add i pi' p))
  in try
    option_map (aux p) (get_id m e)
  with Not_found -> None

let find_insert mp e =
  match find mp e with
  | Some r -> r
  | None ->
    insert_idt mp e

let merge_idt mp e1 e2 =
  let (i1, mp) = find_insert mp e1 in
  let (i2, (m, p)) = find_insert mp e2 in
  (m, PMap.add i1 i2 p)

let merge mp e1 e2 =
  snd (merge mp e1 e2)

