
type 'a t =
  'a Id.map * (Id.t, Id.t) PMap.t ref

let create _ = (Id.map_create (), ref PMap.empty)

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
  try Option.map (representant p) (Id.get_id m e)
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
  List.map (assert_option __LOC__) representants

let fold f i mp =
  List.fold_left (fun a e -> f e a) i (to_list mp)

let iter f =
  fold (fun e _ -> f e) ()

exception Found

let get_one_class (m, p) =
  let r = ref None in
  try
    PMap.iter (fun i ip -> r := Some ip ; raise Found) !p ;
    None
  with Found ->
    let i = assert_option __LOC__ !r in
    Some (assert_option __LOC__ (Id.map_inverse m (representant p i)))

let one_class (m, p) =
  match get_one_class (m, p) with
  | None -> true
  | Some e ->
    let i = assert_option __LOC__ (Id.get_id m e) in
    try
      PMap.iter (fun _ ip -> if i <> representant p ip then raise Found) !p ;
      true
    with Found -> false

