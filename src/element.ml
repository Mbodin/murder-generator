
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
 * compatible with the proposed new value [v1].
 * If they are compatible, [Some true] is returned if [v1] is strictly
 * more precise than [v2], [Some false] otherwise. **)
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

let respect_constraints_base f cst conss evs c =
  List.fold_left (fun b cons ->
    b && match cons with
    | Attribute (a, v) ->
      (match State.get_attribute_character cst c a with
       | None -> true
       | Some v' -> more_precise_attribute_value (State.Fixed_value v) v' <> None)
    | Contact (con, cha, v) ->
      f con cha v) true conss
  && (* TODO: events [evs] *) true

(** Checks whether the constraints [conss] are valid for the character [c]
 * in the character state [cst].
 * Events are also checked to be addable to the characters’s events.
 * Only local constraints are considered: no constraint depending on the
 * instantiation are checked at this point. **)
let respect_constraints =
  respect_constraints_base (fun _ _ _ -> true)

(** As [respect_constraints], but takes an instanciation and thus also checks
 * global constraints. **)
let respect_constraints_inst inst cst conss evs c =
  respect_constraints_base (fun con cha v ->
    let cha = inst.(Utils.Id.to_array cha) in
    match State.get_contact_character cst c con cha with
    | None -> true
    | Some v' ->
      more_precise_attribute_value (State.Fixed_value v) v' <> None) cst conss evs c

(** Takes a character state, some constraints and provided attributes to
 * a player, and returns the equivalent of [compatible_and_progress] for
 * this particular player.
 * Only local constraints are considered. **)
let compatible_and_progress_player cst conss attps evs c =
  if respect_constraints cst conss evs c then
    List.fold_left (function
      | None -> fun _ -> None
      | Some b -> fun (a, v) ->
        let v = State.Fixed_value v in (* TODO: Reread *)
        match State.get_attribute_character cst c a with
        | None -> Some b
        | Some v' ->
          match more_precise_attribute_value v v' with
          | None -> None
          | Some true -> Some true
          | Some false ->
            (** This case is about conflicting reasons why an attribute is placed. **)
            None) (Some false) attps
  else None

(** Same as [compatible_and_progress_player], but also consider global constraints. **)
let compatible_and_progress_player_inst inst cst conss attps contps evs c =
  if respect_constraints_inst inst cst conss evs c then
    match List.fold_left (function
      | None -> fun _ -> None
      | Some b -> fun (a, v) ->
        let v = State.Fixed_value v in (* TODO: Reread *)
        match State.get_attribute_character cst c a with
        | None -> Some b
        | Some v' ->
          match more_precise_attribute_value v v' with
          | None -> None
          | Some true -> Some true
          | Some false ->
            (** This case is about conflicting reasons why an attribute is placed. **)
            None) (Some false) attps with
    | None -> None
    | Some b ->
      (* TODO: contacts [contps] *)
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
      match compatible_and_progress_player_inst inst cst conss attps contps evs c with
      | None -> abort
      | Some b' -> Some (b || b'), 1 + i) (Some false, 0) inst)

let search_instantiation st e =
  let cst = State.get_character_state st in
  let all_players = State.all_players st in
  let possible_players_progress_no_progress =
    Array.map (fun ei ->
      let (conss, attps, contps, evs, rs) = ei in
      let result_list =
        List.map (fun c ->
          (c, compatible_and_progress_player cst conss attps evs c)) all_players in
      let compatible_list =
        List.filter (fun (_, d) -> d <> None) result_list in
      let progress, no_progress =
        List.partition (fun (_, d) -> d = Some true) compatible_list in
      (List.map fst progress, List.map fst no_progress)) e in
  let possible_players =
    (** Possible players for each event.
     * Players that makes the state progress are always put forwards. **)
    Array.map (fun (p, np) ->
        Utils.shuffle p @ Utils.shuffle np) possible_players_progress_no_progress in
  let redirection_array =
    let redirection_array = Utils.seq_array (Array.length e) in
    Array.sort (fun i j ->
      let pi = possible_players.(i) in
      let pj = possible_players.(j) in
      let lpi = List.length pi in
      let lpj = List.length pj in
      compare lpi lpj) redirection_array ;
    redirection_array in
  let rec aux partial_instantiation = function
    | [] ->
      let inst =
        let inst = Array.make (Array.length e) (Utils.Id.from_array (-1)) in
        List.iteri (fun i j ->
          let i = Array.length e - 1 - i in (** The list [partial_instantiation] has been build backward. **)
          let i = redirection_array.(i) in
          inst.(i) <- j) partial_instantiation ;
        inst in
      (match compatible_and_progress st e inst with
       | None -> None
       | Some progress -> Some (inst, progress))
    | i :: redirection_list ->
      let possible = possible_players.(i) in
      let possible =
        List.filter (fun j -> not (List.mem j partial_instantiation)) possible in
      let rec aux' = function
        | [] -> None (** No instantiation led to a compatible state. **)
        | j :: l ->
          match aux (j :: partial_instantiation) redirection_list with
          | None -> aux' l
          | Some r -> Some r
      in aux' possible
    in
  aux [] (Array.to_list redirection_array)

let apply state e inst =
  Utils.array_fold_left2 (fun (state, diff) ei c ->
    let (conss, attps, contps, evs, rs) = ei in
    let (state, diff) =
      List.fold_left (fun (state, diff) -> function
        | Attribute (a, v) ->
          let cstate = State.get_character_state state in
          let write _ =
            State.write_attribute_character cstate c a (State.One_value_of [v]) in
          let diff' =
            match State.get_attribute_character cstate c a with
            | None ->
              write () ;
              0
            | Some v' ->
              match v' with
              | State.Fixed_value v' ->
                assert (v = v') ;
                0
              | State.One_value_of l ->
                assert (List.mem v l) ;
                write () ;
                1 in
          (state, diff + diff')
        | Contact (con, cha, v) ->
          (state, diff) (* TODO *)
        ) (state, diff) conss in
    let (state, diff) =
      List.fold_left (fun (state, diff) (attr, v) ->
        let cstate = State.get_character_state state in
        let write _ =
          State.write_attribute_character cstate c attr (State.Fixed_value v) in
        let diff' =
          match State.get_attribute_character cstate c attr with
          | None ->
            write () ;
            0
          | Some v' ->
            match v' with
            | State.Fixed_value v' ->
              assert (v = v') ;
              0
            | State.One_value_of l ->
              assert (List.mem v l) ;
              write () ;
              1 in
        (state, diff + diff')) (state, diff) attps in
    let (state, diff) =
      List.fold_left (fun (state, diff) _ ->
        (state, diff) (* TODO *)) (state, diff) contps in
    (* TODO: Event [evs] *)
    let _ =
      Array.iter2 (fun c' r ->
        if c <> c' then
          let r' = State.get_relation state c c' in
          State.write_relation state c c' (Relations.compose r' r)) inst rs in
    (state, diff)) (state, 0) e inst

