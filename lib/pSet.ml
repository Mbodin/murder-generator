
type 'a t = ('a, unit) PMap.t

let empty = PMap.empty

let is_empty = PMap.is_empty

let add e = PMap.add e ()

let singleton e = add e empty

let remove = PMap.remove

let mem e s =
  try PMap.find e s ; true
  with Not_found -> false

let fold f i s =
  PMap.foldi (fun e _ -> f e) s i

let iter f s =
  fold (fun e _ -> f e) () s

let length s =
  fold (fun _ n -> 1 + n) 0 s

let merge s = fold add s

let diff s = fold remove s

let inter s1 s2 =
  fold (fun e -> if mem e s2 then add e else Utils.id) empty s1

let map f =
  fold (fun e -> add (f e)) empty

let filter f =
  fold (fun e -> if f e then add e else Utils.id) empty

let map_filter f =
  fold (fun e ->
   match f e with
   | Some e -> add e
   | None -> Utils.id) empty

let map_option f =
  fold (fun e m ->
    Utils.if_option m (fun m ->
      Utils.apply_option (f e) (fun e ->
        add e m))) (Some empty)

let partition f =
  fold (fun e (sy, sn) ->
    if f e then (add e sy, sn) else (sy, add e sn)) (empty, empty)

let partition_map f =
  fold (fun e (sl, sr) ->
    match f e with
    | Utils.Left v -> (add v sl, sr)
    | Utils.Right v -> (sl, add v sr)) (empty, empty)

let to_list s =
  fold (fun e l -> e :: l) [] s

let from_list l =
  List.fold_left (fun s e -> add e s) empty l

let domain m = PMap.map (fun _ -> ()) m

let flatten s =
  fold merge empty s

let flat_map f =
  fold (fun e -> merge (f e)) empty

let for_all p =
  fold (fun e b -> b && p e) true

let exists p =
  fold (fun e b -> b || p e) false

let incl s1 s2 =
  for_all (fun e -> mem e s2) s1

