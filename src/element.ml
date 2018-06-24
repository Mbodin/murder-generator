
type character = Utils.Id.t

type character_constraint =
  | Attribute of State.attribute * State.value
  | Contact of State.contact * character * State.contact_value

type element =
  (character_constraint list
   * (State.attribute * State.value) list
   * (State.contact * int * State.contact_value) list
   * History.event list
   * Relations.t array
  ) array

(** Returns [None] if incompatible the current value [v2] is not
 * compatible with the proposed new value [v1], [Some false] if equal,
 * and [Some true] if [v1] is more precise than [v2]. **)
let more_precise_attribute_value v1 v2 =
  match v1, v2 with
  | State.Fixed_value v, State.Fixed_value v' ->
    if v = v' then Some false else None
  | State.Fixed_value v, State.One_value_of l ->
    if List.mem v l then Some true
    else None
  | State.One_value_of l1, State.One_value_of l2 ->
    if List.for_all (fun v -> List.mem v l2) l1 then
      Some (List.exists (fun v -> not (List.mem v l1)) l2)
    else None
  | State.One_value_of l, State.Fixed_value v ->
    if List.exists (fun v' -> v <> v') l then None
    else Some false

let respect_constraints cst conss c =
  List.fold_left (fun b -> function
    | Attribute (a, v) ->
      (match State.get_attribute_character cst c a with
       | None -> true
       | Some v' -> more_precise_attribute_value (State.Fixed_value v) v' <> None)
    | Contact (con, cha, v) ->
      let cha = inst.(cha) in (* FIXME: must be checked afterwards. *)
      match State.get_contact_character cst c con cha with
      | None -> true
      | Some v' -> more_precise_attribute_value v v' <> None) true conss

(** Takes a cell of the element and a character, and returns the equivalent
 * of [compatible_and_progress] for this particular player. **)
let compatible_and_progress_player cst conss attps c =
  if respect_constraints cst conss c then
    match List.fold_left (function
      | None -> fun _ -> None
      | Some b -> fun (a, v) ->
        match State.get_attribute_character cst c a with
        | None -> Some b
        | Some v' ->
          match more_precise_attribute_value v v' with
          | None -> None
          | Some b' -> Some (b || b')) (Some false) attps with
    | None -> None
    | Some b ->
            Some b
  else None

let compatible_and_progress st e inst =
  let cst = State.get_character_state st in
  let abort = (None, 0) in
  fst (Array.fold_left (fun (acc, i) c ->
    match acc with
    | None -> abort
    | Some b ->
      let (conss, attps, contps, evs, rs) = e.(i) in
      match compatible_and_progress_player cst conss attps c with
      | None -> abort
      | Some b' ->
        match List.fold_left (function
          | None -> fun _ -> None
          | Some b -> fun (con, cha, v) ->
              let cha = inst.(cha) in
              match State.get_contact_character cst c con with
              | None -> Some b (* FIXME: At some point, this [b] may chek things and be a [true] sometimes. *)
              | Some (cha', v') ->
                if cha = cha' && v = v' then Some b else None) (Some false) contps with
        | None -> abort
        | Some b'' ->
          (* TODO: events *)
          (Some (b || b' || b''), 1 + i)) (Some false, 0) inst)

let search_instantiation st e =
  let cst = State.get_character_state st in
  let all_players = State.all_players st in
  let possible_players_progress =
    Array.map (fun ei ->
      let (conss, attps, contps, evs, rs) = ei in
      List.filter (fun c -> compatible_and_progress_player cst conss attps c = Some true)
        all_players) e in
  let possible_players_no_progress =
    Array.map (fun ei ->
      let (conss, attps, contps, evs, rs) = ei in
      List.filter (fun c -> compatible_and_progress_player cst conss attps c = Some false)
        all_players) e in
  TODO

let apply state e i =
  TODO

