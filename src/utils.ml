
let _ = Random.self_init ()

(** Switches some asserts on. These asserts can be costly to perform. **)
let assert_defend = true


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


let safe_tail = function
  | [] -> []
  | _ :: l -> l

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

let rec list_predicate_index f = function
  | [] -> None
  | a :: _ when f a -> Some 0
  | _ :: l -> option_map ((+) 1) (list_predicate_index f l)

let list_index e = list_predicate_index ((=) e)

let shuffle l =
  List.sort (fun _ _ -> Random.int 3 - 1) l


let swap (a, b) = (b, a)

let pair_sort (a, b) =
  if a > b then (b, a)
  else (a, b)


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

let array_fold_left2 f i a1 a2 =
  let r = ref i in
  Array.iter2 (fun v1 v2 -> r := f !r v1 v2) a1 a2 ;
  !r

let array_fold_lefti f acc a =
  snd (Array.fold_left (fun (i, acc) e -> (1 + i, f i acc e)) (0, acc) a)

module Id = struct

    type t = int

    let new_id_function _ =
      let current = ref (-1) in
      fun _ ->
        incr current ;
        !current

    let new_id = new_id_function ()

    let to_array = id
    let from_array = id


    type _ map =
      | Map : ('a, t) PMap.t (** Forwards map **) * (t, 'a) PMap.t (** Reverse map **) * int (** Fresh identifier **) -> 'a map
      | Int : t map (* No need to create new identifiers for integers! *)

    let map_create _ =
      Map (PMap.empty, PMap.empty, 0)

    let t_map_create =
      Int
    let int_map_create =
      t_map_create

    let map_insert_t (type a) : a map -> a -> t * a map = function
      | Map (m, mi, f) -> fun o ->
        (try
           let i = PMap.find o m in
           (i, Map (m, mi, f))
         with Not_found ->
           let i = f in
           (i, Map (PMap.add o i m, PMap.add i o mi, 1 + f)))
      | Int -> fun i ->
        (i, Int)

    let map_insert m e =
      snd (map_insert_t m e)

    let get_id (type a) : a map -> a -> t option = function
      | Map (m, _, f) -> fun o ->
        (try Some (PMap.find o m)
         with Not_found -> None)
      | Int -> fun i ->
        Some i

    let map_inverse (type a) : a map -> t -> a option = function
      | Map (_, mi, f) -> fun i ->
        (try Some (PMap.find i mi)
         with Not_found -> None)
      | Int -> fun i ->
        Some i

  end

module UnionFind = struct

    type 'a t =
      'a Id.map * (Id.t, Id.t) PMap.t ref

    let create _ = (Id.map_create (), ref PMap.empty)

    let create_idt _ = (Id.t_map_create, ref PMap.empty)
    let create_int = create_idt

    let insert_idt (m, p) e =
      let (i, m) =
        let (i, m) = Id.map_insert_t m e in
        (i, m) in
      (i, (m, ref (PMap.add i i !p)))

    let insert mp e =
      snd (insert_idt mp e)

    (** Internal function: finds the representant identifier of a given identifier. It may raise Not_found if i is not present in the mapping. **)
    let rec representant p i =
      let pi = PMap.find i !p in
      if i = pi then pi
      else
        let pi' = representant p pi in
        if assert_defend then assert (let pi'' = PMap.find pi' !p in pi' = pi'') ;
        if pi <> pi' then p := PMap.add i pi' !p ;
        pi'

    let find (m, p) e =
      try option_map (representant p) (Id.get_id m e)
      with Not_found -> assert false

    let find_insert mp e =
      match find mp e with
      | Some i -> (i, mp)
      | None -> insert_idt mp e

    let same_class_insert mp e1 e2 =
      let (i1, mp) = find_insert mp e1 in
      let (i2, mp) = find_insert mp e2 in
      (i1 = i2, mp)

    let same_class mp e1 e2 =
      if_option (find mp e1) (fun i1 ->
        if_option (find mp e2) (fun i2 ->
          Some (i1 = i2)))

    let merge_idt mp e1 e2 =
      let (i1, mp) = find_insert mp e1 in
      let (i2, (m, p)) = find_insert mp e2 in
      if assert_defend then assert (let pi2 = PMap.find i2 !p in i2 = pi2) ;
      if assert_defend then assert (let pi1 = PMap.find i1 !p in i1 = pi1) ;
      (i2, (m, ref (PMap.add i1 i2 !p)))

    let merge mp e1 e2 =
      let mp = snd (merge_idt mp e1 e2) in
      if assert_defend then assert (same_class mp e1 e2 = Some true) ;
      mp

    let to_list (m, p) =
      let classes =
        PMap.foldi (fun i ip l -> if i = ip then i :: l else l) !p [] in
      let representants =
        List.map (Id.map_inverse m) classes in
      List.map (function
        | None -> assert false
        | Some e -> e) representants

    let fold f i mp =
      List.fold_left (fun a e -> f e a) i (to_list mp)

    let iter f =
      fold (fun e () -> f e) ()

    exception Found

    let get_one_class (m, p) =
      let r = ref None in
      try
        PMap.iter (fun i ip -> r := Some ip ; raise Found) !p ;
        None
      with Found ->
        match !r with
        | None -> assert false
        | Some i ->
          match Id.map_inverse m (representant p i) with
          | None -> assert false
          | Some v -> Some v

    let one_class (m, p) =
      match get_one_class (m, p) with
      | None -> true
      | Some e ->
        match Id.get_id m e with
        | None -> assert false
        | Some i ->
          try
            PMap.iter (fun _ ip -> if i <> representant p ip then raise Found) !p ;
            true
          with Found -> false

  end

module BidirectionalList = struct

    type 'a t = 'a list * 'a list
    (** (ll, lr) represents the list ll @ List.rev lr. **)

    let from_list l = (l, [])
    let to_list (ll, lr) = ll @ List.rev lr

    let is_empty = function
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

  end

module PSet = struct

    type 'a t = ('a, unit) PMap.t

    let empty = PMap.empty

    let is_empty = PMap.is_empty

    let add e = PMap.add e ()

    let remove = PMap.remove

    let is_in e s =
      try let _ = PMap.find e s in true
      with Not_found -> false

    let merge s1 s2 =
      PMap.foldi (fun e _ -> add e) s1 s2

    let inter s1 s2 =
      PMap.foldi (fun e _ s -> if is_in e s2 then add e s else s) s1 empty

  end

