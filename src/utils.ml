
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

let seq_range min max =
  (* We assume i >= 0. *)
  unfold (fun j ->
    if j = max then None
    else Some (j, j + 1)) min

let seq = seq_range 0

let seq_incl i = seq (i + 1)

let uniq l =
  let rec aux = function
    | a :: b :: l when a = b -> aux (b :: l)
    | a :: l -> a :: aux l
    | [] -> [] in
  aux (List.sort compare l)

let rec repeat i e =
  if i = 0 then []
  else e :: repeat (i - 1) e

let rec list_update i e l =
  match i, l with
  | 0, [] -> [e]
  | 0, _ :: l -> e :: l
  | i, a :: l -> a :: list_update (i - 1) e l
  | _, [] -> raise Not_found

let rec list_remove i = function
  | [] -> raise Not_found
  | _ :: l when i = 0 -> l
  | a :: l -> a :: list_remove (i - 1) l

let positive_mod a b =
  ((a mod b) + b) mod b

let square x = x * x

exception NegativeWeigth
exception InternalError

let take l =
  let s = List.fold_left (+) 0 (List.map fst l) in
  if s <= 0 then raise NegativeWeigth
  else
    let rec search t = function
      | [] -> raise InternalError
      | (p, v) :: l ->
        if p >= t then (v, l)
        else
          let (r, l) = search (t - p) l in
          (r, (p, v) :: l)
    in search (Random.int s) l

let select l = fst (take l)

let rand min max =
  min + Random.int (max - min + 1)

let select_any l =
  List.nth l (Random.int (List.length l))

let take_any l =
  let i = Random.int (List.length l) in
  (List.nth l i, list_remove i l)

let sum = List.fold_left (+) 0
let array_sum = Array.fold_left (+) 0

let array_count f = Array.fold_left (fun v x -> v + if f x then 1 else 0) 0
let array_for_all f = Array.fold_left (fun v x -> v && f x) true


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
  (i2, (m, PMap.add i1 i2 p))

let merge mp e1 e2 =
  snd (merge_idt mp e1 e2)


type 'a two_direction_list = 'a list * 'a list
(** (ll, lr) represents the list ll @ List.rev lr. **)

let two_direction_list_from_list l = (l, [])
let two_direction_list_to_list (ll, lr) = ll @ List.rev lr

let two_direction_list_is_empty = function
  | ([], []) -> true
  | _ -> false

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

