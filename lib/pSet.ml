
(** We implement sets as a maps to [unit] values.
 * We also store the comparing function (which shall
 * be [compare] most of the time. **)
type 'a t = {
    compare : ('a -> 'a -> int) ;
    map : ('a, unit) PMap.t
  }

let create compare = {
    compare = compare ;
    map = PMap.create compare
  }

let empty = {
    compare = compare ;
    map = PMap.empty
  }

(** Creates an empty set with the same comparing function as the argument set. **)
let empty_of s = create s.compare

let is_empty s = PMap.is_empty s.map

let add e s = { s with map = PMap.add e () s.map }

let singleton e = add e empty

let remove e s = { s with map = PMap.remove e s.map }

let mem e s =
  try PMap.find e s.map ; true
  with Not_found -> false

let fold f i s =
  PMap.foldi (fun e _ -> f e) s.map i

let iter f s =
  fold (fun e _ -> f e) () s

let length s =
  fold (fun _ n -> 1 + n) 0 s

let merge s1 s2 =
  fold add s1 s2

let diff s = fold remove s

let inter s1 s2 =
  fold (fun e -> if mem e s2 then add e else Utils.id) (empty_of s1) s1

let map ?(compare = compare) f =
  fold (fun e -> add (f e)) (create compare)

let filter f s =
  fold (fun e -> if f e then add e else Utils.id) (empty_of s) s

let map_filter ?(compare = compare) f =
  fold (fun e ->
   match f e with
   | Some e -> add e
   | None -> Utils.id) (create compare)

let map_option ?(compare = compare) f =
  fold (fun e m ->
    Utils.if_option m (fun m ->
      Utils.apply_option (f e) (fun e ->
        add e m))) (Some (create compare))

let partition f s =
  fold (fun e (sy, sn) ->
    if f e then (add e sy, sn) else (sy, add e sn)) (empty_of s, empty_of s) s

let partition_map ?(comparel = compare) ?(comparer = compare) f =
  fold (fun e (sl, sr) ->
    match f e with
    | Utils.Left v -> (add v sl, sr)
    | Utils.Right v -> (sl, add v sr)) (create comparel, create comparer)

let to_list s =
  fold (fun e l -> e :: l) [] s

let from_list ?(compare = compare) l =
  List.fold_left (fun s e -> add e s) (create compare) l

let domain ?(compare = compare) m = {
    compare = compare ;
    map = PMap.map (fun _ -> ()) m
  }

let flatten ?(compare = compare) s =
  fold merge (create compare) s

let flat_map ?(compare = compare) f =
  fold (fun e -> merge (f e)) (create compare)

let for_all p =
  fold (fun e b -> b && p e) true

let exists p =
  fold (fun e b -> b || p e) false

let incl s1 s2 =
  for_all (fun e -> mem e s2) s1

