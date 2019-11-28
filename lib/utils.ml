
open ExtList
open ExtString

let _ = Random.self_init ()

let assert_defend = true


let id x = x

let compose f g x = f (g x)

let apply_option o f =
  Option.map f o

let if_option = function
  | None -> fun _ -> None
  | Some x -> fun f -> f x

let unsome_default d = function
  | None -> d
  | Some x -> x

let assert_option err = function
  | None -> failwith ("This option-type should not be [None]. " ^ err)
  | Some v -> v

type ('a, 'b) plus =
  | Left of 'a
  | Right of 'b

let error_monad o f =
  match o with
  | Left e -> Left e
  | Right v -> f v


type ('value, 'cache) cached = 'value * 'cache ref

let cached v c = (v, ref c)

let get_value = fst

let get_cache (_, c) = !c

let set_cache (_, c) v = c := v


let safe_tail = function
  | [] -> []
  | _ :: l -> l

let list_header n l =
  let l' = List.take n l in
  if l' = [] then l else l'

let rec unfold f i =
  match f i with
  | None -> []
  | Some (v, j) ->
    v :: unfold f j

let seq_range min max =
  unfold (fun j ->
    if j = max + 1 then None
    else Some (j, j + 1)) min

let seq_incl = seq_range 0

let seq i = seq_incl (i - 1)

let seq_range_array mi ma =
  Array.init (max 0 (ma - mi + 1)) (fun i -> mi + i)

let seq_incl_array = seq_range_array 0

let seq_array i = seq_incl_array (i - 1)

let uniq ?(cmp = compare) l =
  let rec aux = function
    | a :: b :: l when a = b -> aux (b :: l)
    | a :: l -> a :: aux l
    | [] -> [] in
  aux (List.sort ~cmp:cmp l)

let is_uniq_witness ?(cmp = compare) l =
  let rec aux = function
    | a :: b :: l when a = b -> Some a
    | a :: l -> aux l
    | [] -> None in
  aux (List.sort ~cmp:cmp l)

let is_uniq ?(cmp = compare) l =
  is_uniq_witness ~cmp:cmp l = None

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

let list_split i l =
  let rec aux i before l =
    if i = 0 then
      (List.rev before, l)
    else
      match l with
      | [] -> (List.rev before, [])
      | a :: l -> aux (i - 1) (a :: before) l in
  aux i [] l

let rec list_predicate_index f = function
  | [] -> None
  | a :: _ when f a -> Some 0
  | _ :: l -> Option.map ((+) 1) (list_predicate_index f l)

let list_index e = list_predicate_index ((=) e)

let list_predicate_prefix f =
  let rec aux prefix = function
    | e :: l when f e ->
      aux (e :: prefix) l
    | l -> (List.rev prefix, l) in
  aux []

let list_fold_lefti f i l =
  fst (List.fold_left (fun (a, i) e -> (f i a e, 1 + i)) (i, 0) l)

let list_map_filter f l =
  List.rev (List.fold_left (fun l e ->
    match f e with
    | None -> l
    | Some v -> v :: l) [] l)

let list_map_option f l =
  Option.map List.rev (List.fold_left (fun r e ->
    if_option r (fun l -> if_option (f e) (fun v -> Some (v :: l)))) (Some []) l)

let list_find_map_opt f l =
  try Some (List.find_map f l)
  with Not_found -> None

let rec list_partition_map f = function
  | [] -> ([], [])
  | a :: l ->
    let (l, r) = list_partition_map f l in
    match f a with
    | Left b -> (b :: l, r)
    | Right c -> (l, c :: r)

let list_associ k l =
  let (i, (_, b)) = List.findi (fun _ (a, _) -> (a = k)) l in
  (i, b)

let list_associ_opt k l =
  try Some (list_associ k l)
  with Not_found -> None

let list_map3 f l1 l2 l3 =
  let rec aux = function
    | [], [], [] -> []
    | e1 :: l1, e2 :: l2, e3 :: l3 -> f e1 e2 e3 :: aux (l1, l2, l3)
    | _ -> invalid_arg "list_map3" in
  aux (l1, l2, l3)

let list_map4 f l1 l2 l3 l4 =
  let rec aux = function
    | [], [], [], [] -> []
    | e1 :: l1, e2 :: l2, e3 :: l3, e4 :: l4 -> f e1 e2 e3 e4 :: aux (l1, l2, l3, l4)
    | _ -> invalid_arg "list_map4" in
  aux (l1, l2, l3, l4)


let shuffle l =
  List.sort ~cmp:(fun _ _ -> if Random.bool () then 1 else -1) l

let array_shuffle a =
  let a = Array.copy a in
  Array.sort (fun _ _ -> if Random.bool () then 1 else -1) a ;
  a

let rec list_match_right = function
  | [] -> None
  | e :: l ->
    match list_match_right l with
    | None -> Some ([], e)
    | Some (l, r) -> Some (e :: l, r)

let rec argmax compare = function
  | [] -> None
  | [a] -> Some a
  | a :: b :: l ->
    if compare a b > 0 then argmax compare (a :: l)
    else argmax compare (b :: l)

let list_square l =
  let rec aux stack = function
    | [] -> List.concat stack
    | b :: lb ->
      aux (List.map (fun a -> (a, b)) l :: stack) lb in
  aux []


let swap (a, b) = (b, a)

let pair_sort (a, b) =
  if a > b then (b, a)
  else (a, b)

let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let fst4 (a, _, _, _) = a
let snd4 (_, b, _, _) = b


let positive_mod a b =
  ((a mod b) + b) mod b

let square x = x * x

exception EmptyList
exception NegativeWeigth
exception InternalError

let take = function
  | [] -> raise EmptyList
  | (w, e) :: [] ->
    if w <= 0 then raise NegativeWeigth
    else (e, [])
  | l ->
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

let select_any = function
  | [] -> raise EmptyList
  | e :: [] -> e
  | l -> assert_option __LOC__ (List.nth_opt l (Random.int (List.length l)))

let take_any = function
  | [] -> raise EmptyList
  | e :: [] -> (e, [])
  | l ->
    let i = Random.int (List.length l) in
    (assert_option __LOC__ (List.nth_opt l i), list_remove i l)

let sum = List.fold_left (+) 0
let array_sum = Array.fold_left (+) 0

let average l = sum l / List.length l

let count f = List.fold_left (fun v x -> v + if f x then 1 else 0) 0
let array_count f = Array.fold_left (fun v x -> v + if f x then 1 else 0) 0

let array_fold_left2 f i a1 a2 =
  let r = ref i in
  Array.iter2 (fun v1 v2 -> r := f !r v1 v2) a1 a2 ;
  !r

let array_fold_lefti f acc a =
  snd (Array.fold_left (fun (i, acc) e -> (1 + i, f i acc e)) (0, acc) a)


let pmap_to_list m =
  PMap.foldi (fun k v l -> (k, v) :: l) m []

let rec complete_string_pre pre s n =
  if n > String.length s then
    complete_string_pre pre (pre ^ s) n
  else s

let rec complete_string_post post s n =
  if n > String.length s then
    complete_string_post post (s ^ post) n
  else s

let lazy_enum e =
  Enum.make
    (fun _ -> Enum.next (Lazy.force e))
    (fun _ -> Enum.count (Lazy.force e))
    (fun _ -> Enum.clone (Lazy.force e))

let enum_split_on_char c =
  let cs = String.make 1 c in
  let rec create ended str =
    let str = ref str in
    let ended = ref ended in
    Enum.make
      (fun _ ->
        if !ended then
          raise Enum.No_more_elements
        else (
            try
              let (before, after) = String.split !str cs in
              str := after ;
              before
            with Invalid_string ->
              ended := true ;
              !str
        ))
      (fun _ ->
         if !ended then 0
         else List.length (String.split_on_char c !str))
      (fun _ -> create !ended !str) in
  create false

