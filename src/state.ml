
type character = Id.t

type objective = {
    difficulty : int ;
    complexity : int
  }

type relation_state =
  Relation.t array array * objective array

let copy_relation_state (r, o) =
  (Array.map Array.copy r, Array.copy o)

exception SelfRelation

let zero_objective = {
    difficulty = 0 ;
    complexity = 0
  }

(** Relation states have as little cells as possible: [n - 1] for the
 * first counter, and [i + 1] for the second.
 * To get the relation between characters [c1] and [c2], with [c1 < c2],
 * one has to check the array at [.(c2 - 1).(c1)]. **)
let create_relation_state n =
  (Array.init (n - 1) (fun i ->
     Array.make (i + 1) Relation.neutral),
   Array.make n zero_objective)

let rec read_relation_state (a, _) c1 c2 =
  let c1 = Id.to_array c1 in
  let c2 = Id.to_array c2 in
  if c1 = c2 then Relation.neutral
  else if c2 > c1 then
    a.(c2 - 1).(c1)
  else Relation.reverse a.(c1 - 1).(c2)

let write_relation_state (a, rs) c1 c2 r =
  let c1 = Id.to_array c1 in
  let c2 = Id.to_array c2 in
  let (x, y, r) =
    if c1 = c2 then
      raise SelfRelation
    else if c2 > c1 then
      (c2 - 1, c1, r)
    else (c1 - 1, c2, Relation.reverse r) in
  let old = a.(x).(y) in
  a.(x).(y) <- r ;
  let dd = Relation.difficulty r - Relation.difficulty old in
  let dc = Relation.complexity r - Relation.complexity old in
  rs.(c1) <- {
      difficulty = rs.(c1).difficulty + dd ;
      complexity = rs.(c1).complexity + dc
    } ;
  rs.(c2) <- {
      difficulty = rs.(c2).difficulty + dd ;
      complexity = rs.(c2).complexity + dc
    }

let add_relation_state a c1 c2 r =
  (** As [Relation.neutral] is neutral for [Relation.compose] and that it
   * happens frequently, we first check whether we really need to update
   * anything. **)
  if r <> Relation.neutral then
    let r' = read_relation_state a c1 c2 in
    write_relation_state a c1 c2 (Relation.compose r' r)

let character_complexity (_, r) c =
  r.(Id.to_array c).complexity

let character_difficulty (_, r) c =
  r.(Id.to_array c).difficulty

let set_complexity (_, r) c v =
  r.(Id.to_array c) <- { r.(Id.to_array c) with complexity = v }

let set_difficulty (_, r) c v =
  r.(Id.to_array c) <- { r.(Id.to_array c) with difficulty = v }

let add_complexity (_, r) c d =
  let o = r.(Id.to_array c) in
  r.(Id.to_array c) <- { o with complexity = d + o.complexity }

let add_difficulty (_, r) c d =
  let o = r.(Id.to_array c) in
  r.(Id.to_array c) <- { o with difficulty = d + o.difficulty }


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

let compose_attribute_value compatible v1 v2 =
  let compatible v1 v2 = v1 = v2 || compatible v1 v2 in
  match v1, v2 with
  | One_value_of l1, One_value_of l2 ->
    let l3 = List.filter (fun v -> List.mem v l1) l2 in
    if l3 = [] then None else Some (One_value_of l3)
  | One_value_of l1, Fixed_value (l2, s2) | Fixed_value (l2, s2), One_value_of l1 ->
    let l = List.filter (fun v2 -> List.mem v2 l1) l2 in
    if l <> [] then Some (Fixed_value (l, s2)) else None
  | Fixed_value (l1, s1), Fixed_value (l2, s2) ->
    Utils.if_option (compose_strictness s1 s2) (fun s3 ->
      let l3 =
        let aux low non =
          List.filter (fun v ->
            List.exists (fun v' -> compatible v' v) non) low in
        match s1, s2 with
        | LowStrict, NonStrict -> aux l1 l2
        | NonStrict, LowStrict -> aux l2 l1
        | NonStrict, NonStrict ->
          Utils.list_map_filter (fun v ->
            try Some (ExtList.List.find_map (fun v' ->
                        if compatible v' v then Some v
                        else if compatible v v' then Some v'
                        else None) l2)
            with Not_found -> None) l1
        | _ -> assert false in
      if l3 <> [] then
        Some (Fixed_value (l3, s3))
      else None)

let attribute_value_progress v1 v2 =
  match v1, v2 with
  | One_value_of _, Fixed_value _ -> true
  | One_value_of l1, One_value_of l2 -> List.length l1 > List.length l2
  | Fixed_value (l1, _), Fixed_value (l2, _) -> List.length l1 > List.length l2
  | _, _ -> false

let attribute_value_can_progress = function
  | One_value_of l -> l <> []
  | Fixed_value (_, _) -> false


(** Mapping from attribute to its value (either [PlayerAttribute.constructor]
 * or [PlayerAttribute.constructor attribute_value]. **)
type 'value attribute_map =
  (Attribute.PlayerAttribute.attribute, 'value) PMap.t

(** Mapping for contacts to their values (either [ContactAttribute.constructor]
 * or [ContactAttribute.constructor attribute_value]. **)
type 'value contact_map =
  (character, (Attribute.ContactAttribute.attribute, 'value) PMap.t) PMap.t

(** A generic map, to be either instantiated by [character_state] when
 * solving the constraints or by [character_state_final] once solved. **)
type ('player, 'contact) generic_character_state =
  ('player attribute_map * 'contact contact_map) array

type character_state =
  (Attribute.PlayerAttribute.constructor attribute_value,
   Attribute.ContactAttribute.constructor attribute_value) generic_character_state

(** Same as [character_state], but without any doubt on the actual constructor.
 * A boolean is however added to state whether its value is fixed ([true]), or
 * whether is was just assigned this value by default ([false]) **)
type character_state_final =
  (Attribute.PlayerAttribute.constructor * bool,
   Attribute.ContactAttribute.constructor * bool) generic_character_state

let create_character_state n =
  Array.init n (fun i -> (PMap.empty, PMap.empty))

let get_all_attributes_character st c =
  fst st.(Id.to_array c)

let get_attribute_character st c a =
  try Some (PMap.find a (get_all_attributes_character st c))
  with Not_found -> None

let write_attribute_character st c a v =
  let c = Id.to_array c in
  st.(c) <- (PMap.add a v (fst st.(c)), snd st.(c))

let force_get_attribute_character cm st c a =
  match get_attribute_character st c a with
  | Some v -> v
  | None ->
    let l =
      Utils.assert_option __LOC__ (Attribute.PlayerAttribute.constructors cm a) in
    let v = One_value_of l in
    write_attribute_character st c a v ;
    v

let get_contact_character st c a ct =
  try Some (PMap.find a (PMap.find ct (snd st.(Id.to_array c))))
  with Not_found -> None

let write_contact_character st c a ct v =
  let c = Id.to_array c in
  let m =
    try PMap.find ct (snd st.(c))
    with Not_found -> PMap.empty in
  let m = PMap.add a v m in
  st.(c) <- (fst st.(c), PMap.add ct m (snd st.(c)))

let get_all_contact_character st c ct =
  Utils.pmap_to_list (
    try PMap.find ct (snd st.(Id.to_array c))
    with Not_found -> PMap.empty)

let get_all_contacts_character st c =
  PMap.map Utils.pmap_to_list (snd st.(Id.to_array c))

type t = {
    characters : character_state ;
    relations : relation_state ;
    history : History.t
  }

let copy st = {
    characters = Array.copy st.characters ;
    relations = copy_relation_state st.relations ;
    history = History.copy st.history
  }

let get_relation_state st = st.relations

let read_relation st = read_relation_state (get_relation_state st)
let write_relation st = write_relation_state (get_relation_state st)
let add_relation st = add_relation_state (get_relation_state st)

let create_state n = {
    characters = create_character_state n ;
    relations = create_relation_state n ;
    history = History.create_state n
  }

let get_character_state st = st.characters

let get_history_state st = st.history

let set_history_state st h = { st with history = h }

let apply_event st status ev =
  { st with history = History.apply st.history status ev }

let apply_events st status evs =
  { st with history = History.lapply st.history status evs }

let number_of_player st = Array.length st.characters

let all_players st =
  History.all_players_length (number_of_player st)

let number_of_player_relation_state (_, rs) = Array.length rs

let all_players_relation st =
  History.all_players_length (number_of_player_relation_state st)

type final =
  character_state_final * relation_state * History.final

(** Convert a value into a final value,
 * picking a particular possibility and marking whether the value has been fixed. **)
let select_value = function
  | Fixed_value (l, strict) -> (Utils.select_any l, true)
  | One_value_of l -> (Utils.select_any l, false)

let finalise st d =
  (Array.map (fun (a, c) ->
    (PMap.map select_value a,
     PMap.map (PMap.map select_value) c)) st.characters,
   st.relations,
   History.finalise st.history d)

let get_attribute_character_final (cst, _, _) =
  get_attribute_character cst

let get_all_attributes_character_final (cst, _, _) =
  get_all_attributes_character cst

let get_contact_character_final (cst, _, _) =
  get_contact_character cst

let get_all_contacts_character_final (cst, _, _) =
  get_all_contacts_character cst

let get_relation_state_final (_, a, _) = a

let read_relation_final st = read_relation_state (get_relation_state_final st)

let character_complexity_final (_, rst, _) =
  character_complexity rst

let character_difficulty_final (_, rst, _) =
  character_difficulty rst

let get_history_final (_, _, hst) = hst

let all_players_final (_, rst, _) = all_players_relation rst

