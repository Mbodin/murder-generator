
type character = Utils.Id.t

type relation_state =
  Relation.t array array

exception SelfRelation

let create_relation_state n =
  Array.init (n - 1) (fun i ->
    Array.make (n - 1 - i) Relation.neutral)

let rec read_relation_state a c1 c2 =
  let c1 = Utils.Id.to_array c1 in
  let c2 = Utils.Id.to_array c2 in
  if c1 = c2 then Relation.neutral
  else if c1 > c2 then
    Relation.reverse a.(c2).(c1)
  else a.(c1).(c2)

let write_relation_state a c1 c2 r =
  let c1 = Utils.Id.to_array c1 in
  let c2 = Utils.Id.to_array c2 in
  if c1 = c2 then
    raise SelfRelation
  else if c1 > c2 then
    a.(c2).(c1) <- Relation.reverse r
  else a.(c2).(c1) <- r

module type Attribute = sig
    type attribute
    type constructor
    type constructor_map

    val empty_constructor_map : constructor_map
    val attribute_name : constructor_map -> attribute -> string option
    val constructor_name : constructor_map -> constructor -> string option
    val constructor_attribute : constructor_map -> constructor -> attribute option
    val constructors : constructor_map -> attribute -> constructor list option
    val declare_attribute : constructor_map -> string -> attribute * constructor_map
    val declare_constructor : constructor_map -> attribute -> string -> constructor * constructor_map
    val get_attribute : constructor_map -> string -> attribute option
    val get_constructor : constructor_map -> attribute -> string -> constructor option
    val remove_constructor : constructor_map -> constructor -> constructor_map
  end

module AttributeInst () =
  struct

    type attribute = Utils.Id.t
    type constructor = Utils.Id.t

    type constructor_map =
      string Utils.Id.map (** Attribute names **)
      * (attribute * string) Utils.Id.map (** The map storing each constructor.
                                           * The attribute is part of the
                                           * constructor, with the constructor
                                           * name. **)
      * (attribute, constructor list) PMap.t (** Which constructors is associated
                                              * to which attribute. **)

    let empty_constructor_map =
      (Utils.Id.map_create (),
       Utils.Id.map_create (),
       PMap.empty)

    let attribute_name (m, _, _) a =
      Utils.Id.map_inverse m a

    let constructor_name (_, m, _) c =
      Utils.option_map snd (Utils.Id.map_inverse m c)

    let constructor_attribute (_, m, _) c =
      Utils.option_map fst (Utils.Id.map_inverse m c)

    let constructors (_, _, m) a =
      try Some (PMap.find a m)
      with Not_found -> None

    let declare_attribute (mn, mc, al) a =
      let (a, mn) = Utils.Id.map_insert_t mn a in
      (a, (mn, mc, PMap.add a [] al))

    let declare_constructor (mn, mc, al) a c =
      let (c, mc) = Utils.Id.map_insert_t mc (a, c) in
      let l =
        try PMap.find a al
        with Not_found -> assert false in
      (c, (mn, mc, PMap.add a (c :: l) al))

    let get_attribute (mn, _, _) a =
      Utils.Id.get_id mn a

    let get_constructor (_, mc, _) a c =
      Utils.Id.get_id mc (a, c)

    let remove_constructor m c =
      let a = Utils.assert_option __LOC__ (constructor_attribute m c) in
      let (mn, mc, al) = m in
      let l =
        try PMap.find a al
        with Not_found -> assert false in
      let l = List.filter ((<>) c) l in
      (mn, mc, PMap.add a l al)

  end

module PlayerAttribute = AttributeInst ()

module ContactAttribute = AttributeInst ()

type constructor_maps = {
    player : PlayerAttribute.constructor_map ;
    contact : ContactAttribute.constructor_map
  }

let empty_constructor_maps = {
    player = PlayerAttribute.empty_constructor_map ;
    contact = ContactAttribute.empty_constructor_map
  }

type attribute =
  | PlayerAttribute of PlayerAttribute.attribute
  | ContactAttribute of ContactAttribute.attribute

type constructor =
  | PlayerConstructor of PlayerAttribute.constructor
  | ContactConstructor of ContactAttribute.constructor

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
  | Fixed_value of 'value list * strictness
  | One_value_of of 'value list

let compose_attribute_value v1 v2 =
  match v1, v2 with
  | One_value_of l1, One_value_of l2 ->
    let l3 = List.filter (fun v -> List.mem v l1) l2 in
    if l3 = [] then None else Some (One_value_of l3)
  | One_value_of l1, Fixed_value (l2, s2) | Fixed_value (l2, s2), One_value_of l1 ->
    let l = List.filter (fun v2 -> List.mem v2 l1) l2 in
    if l <> [] then Some (Fixed_value (l, s2)) else None
  | Fixed_value (v1, s1), Fixed_value (v2, s2) ->
    if v1 = v2 then
      Utils.option_map (fun s3 -> Fixed_value (v1, s3)) (compose_strictness s1 s2)
    else None

let attribute_value_progress v1 v2 =
  match v1, v2 with
  | One_value_of _, Fixed_value _ -> true
  | _, _ -> false

let attribute_value_can_progress = function
  | One_value_of l -> l <> []
  | Fixed_value (_, _) -> false

type attribute_map =
  (PlayerAttribute.attribute, PlayerAttribute.constructor attribute_value) PMap.t
type contact_map =
  (ContactAttribute.attribute,
    (character, ContactAttribute.constructor attribute_value) PMap.t) PMap.t

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
    let l = Utils.assert_option __LOC__ (PlayerAttribute.constructors cm a) in
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

let get_relation_state (_, a, _) = a

let read_relation st = read_relation_state (get_relation_state st)

let write_relation st = write_relation_state (get_relation_state st)

let create_state n =
  (create_character_state n, create_relation_state n, History.create_state n)

let get_character_state (st, _, _) = st

let all_players_length l =
  List.map Utils.Id.from_array (Utils.seq l)

let number_of_player (st, _, _) = Array.length st

let all_players st =
  all_players_length (number_of_player st)

(** Remember that relation state do not store self-relation and is
 * thus one cell smaller than usual arrays. **)
let number_of_player_relation_state st = 1 + Array.length st

let all_players_relation st =
  all_players_length (number_of_player_relation_state st)

