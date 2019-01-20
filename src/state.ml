
type character = Utils.Id.t

type relation_state =
  Relations.t array array

exception SelfRelation

let create_relation_state n =
  Array.init (n - 1) (fun i ->
    Array.make (n - 1 - i) (Relations.Basic Relations.Neutral, false))

let rec get_relation_state a c1 c2 =
  let c1 = Utils.Id.to_array c1 in
  let c2 = Utils.Id.to_array c2 in
  if c1 = c2 then
    raise SelfRelation
  else if c1 > c2 then
    Relations.reverse a.(c2).(c1)
  else a.(c1).(c2)

let write_relation_state a c1 c2 r =
  let c1 = Utils.Id.to_array c1 in
  let c2 = Utils.Id.to_array c2 in
  if c1 = c2 then
    raise SelfRelation
  else if c1 > c2 then
    a.(c2).(c1) <- Relations.reverse r
  else a.(c2).(c1) <- r

type attribute = Utils.Id.t
type value = Utils.Id.t

type contact = Utils.Id.t
type contact_value = Utils.Id.t

type constructor_map =
  string Utils.Id.map (** Attribute names **)
  * string Utils.Id.map (** Constructor names **)
  * (attribute, value list) PMap.t (** What constructors are associated to each attribute. **)

let empty_constructor_map =
  (Utils.Id.map_create (),
   Utils.Id.map_create (),
   PMap.empty)

let attribute_name (m, _, _) a =
  Utils.Id.map_inverse m a

let value_name (_, m, _) c =
  Utils.Id.map_inverse m c

let constructors (_, _, m) a =
  try Some (PMap.find a m)
  with Not_found -> None

let declare_attribute (mn, mc, al) a =
  let (a, mn) = Utils.Id.map_insert_t mn a in
  (a, (mn, mc, PMap.add a [] al))

let declare_constructor (mn, mc, al) a c =
  let (c, mc) = Utils.Id.map_insert_t mc c in
  let l =
    try PMap.find a al
    with Not_found -> assert false in
  (c, (mn, mc, PMap.add a (c :: l) al))

let remove_constructor (mn, mc, al) a c =
  let l =
    try PMap.find a al
    with Not_found -> assert false in
  let l = List.filter ((<>) c) l in
  (mn, mc, PMap.add a l al)

type strictness =
  | NonStrict
  | LowStrict
  | Strict

let compose_strictness s1 s2 =
  match s1, s2 with
  | Strict, _ | _, Strict -> None
  | LowStrict, LowStrict -> None
  | NonStrict, s | s, NonStrict -> Some s

type 'value attribute_value =
  | Fixed_value of 'value * strictness
  | One_value_of of 'value list

let compose_attribute_value v1 v2 =
  match v1, v2 with
  | One_value_of l1, One_value_of l2 ->
    let l3 = List.filter (fun v -> List.mem v l1) l2 in
    if l3 = [] then None else Some l3
  | One_value_of l1, Fixed_value (v2, s2) | Fixed_value (v2, s2), One_value_of l1 ->
    if List.mem v l1 then Some (Fixed_value (v2, s2) else None
  | Fixed_value (v1, s1), Fixed_value (v2, s2) ->
    if v1 = v2 then
      Utils.option_map (fun s3 -> Fixed_value (v1, s3)) (compose_strictness s1 s2)
    else None


let constraint_fixed_attribute_value v1 v2 =
  match v1, v2 with
  | One_value_of _, Fixed_value _ -> true
  | _, _ -> false

type attribute_map = (attribute, value attribute_value) PMap.t
type contact_map = (contact, (character, contact_value attribute_value) PMap.t) PMap.t

type character_state =
  (attribute_map * contact_map) array

let create_character_state n =
  Array.init n (fun i -> (PMap.empty, PMap.empty))

let get_attribute_character st c a =
  try Some (PMap.find a (fst st.(Utils.Id.to_array c)))
  with Not_found -> None

let write_attribute_character st c a v =
  let c = Utils.Id.to_array c in
  st.(c) <- (PMap.add a v (fst st.(c)), snd st.(c))

let force_get_attribute_character cm st c a =
  try PMap.find a (fst st.(Utils.Id.to_array c))
  with Not_found ->
    let l =
      match constructors cm a with
      | Some l -> l
      | None -> assert false in
    let v = One_value_of l in
    write_attribute_character st c a v ;
    v

let get_contact_character st c a ct =
  try Some (PMap.find ct (PMap.find a (snd st.(Utils.Id.to_array c))))
  with Not_found -> None

let get_all_contact_character st c a =
  try PMap.foldi (fun ct cv l -> (ct, cv) :: l)
        (PMap.find a (snd st.(Utils.Id.to_array c))) []
  with Not_found -> []

type t =
  character_state * relation_state * History.state

let get_relation (_, a, _) = get_relation_state a

let write_relation (_, a, _) = write_relation_state a

let create_state n =
  (create_character_state n, create_relation_state n, History.create_state n)

let get_character_state (st, _, _) = st

let all_players (st, _, _) =
  List.map Utils.Id.from_array (Utils.seq (Array.length st))

