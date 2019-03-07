
type character = Utils.Id.t

type character_constraint =
  | Attribute of State.PlayerAttribute.attribute
                 * State.PlayerAttribute.constructor State.attribute_value
  | Contact of State.ContactAttribute.attribute
               * int option
               * State.ContactAttribute.constructor State.attribute_value

type cell =
  character_constraint list
  * History.event list
  * Relation.t array

type t = cell array * character_constraint list

(** Returns the list of attributes provided by this cosntraint. **)
let provided_attributes_constraint =
  let aux a = function
    | State.Fixed_value _ -> Utils.PSet.singleton a
    | _ -> Utils.PSet.empty in function
  | Attribute (a, v) -> aux (State.PlayerAttribute a) v
  | Contact (a, _, v) -> aux (State.ContactAttribute a) v

let provided_attributes (e, other) =
  let provided_attributes_constraint_list =
    List.fold_left (fun s c ->
      Utils.PSet.merge s (provided_attributes_constraint c)) in
  Utils.PSet.to_list (Array.fold_left (fun s (l, _, _) ->
      provided_attributes_constraint_list s l)
    (provided_attributes_constraint_list Utils.PSet.empty other) e)

(** States whether [v1] and [v2] are compatible and make some progress.
 * The return value is expressed as for [compatible_and_progress]:
 * [None] means that it is not compatible, [Some true] that it is
 * compatible and progress, and [Some false] that it is compatible
 * but does not progress. **)
let compatible_and_progress_attribute_value v1 v2 =
  Option.map (State.attribute_value_progress v1)
    (State.compose_attribute_value v1 v2)

(** Merges two results of [compatible_and_progress]. **)
let merge_progress b1 b2 =
  match b1, b2 with
  | None, _ | _, None -> None
  | Some b1, Some b2 -> Some (b1 || b2)

(** Checks whether the constraints [conss] are valid for the
 * character [c] in the character state [cst].
 * The contact case is given as argument as the function [f]. **)
let respect_constraints_base f cst conss c =
  List.fold_left (fun b cons ->
    merge_progress b
     (match cons with
     | Attribute (a, v1) ->
       (match State.get_attribute_character cst c a with
        | None -> Some false
        | Some v2 ->
          compatible_and_progress_attribute_value v1 v2)
     | Contact (con, cha, v) ->
       f con cha v)) (Some false) conss

(** Checks whether the constraints [conss] are valid for the
 * character [c] in the character state [cst].
 * In addition to [respect_constraints_base], this
 * function also checks events constraints [evs]. **)
let respect_constraints_events f cst conss evs c =
  merge_progress
    (respect_constraints_base f cst conss c)
    (* TODO: events [evs] *) (Some false)

(** Checks whether the constraints [conss] are locally valid for the
 * character [c] in the character state [cst].
 * Events are also checked to be addable to the characters’s events.
 * Only local constraints are considered: no constraint depending on the
 * instantiation are checked at this point. **)
let respect_constraints =
  respect_constraints_events (fun _ _ _ -> Some false)

(** Returns all the players of the state [st] that are not in the
 * instantiation [inst]. **)
let other_players st inst =
  List.filter (fun i -> Array.exists ((=) i) inst) (State.all_players st)

(** A function to be given to [respect_constraints_events] to deal with the
 * instantiation-dependent contacts. **)
let check_contact inst st c con cha v1 =
  let cst = State.get_character_state st in
  let check cha =
    let cha = inst.(cha) in
    match State.get_contact_character cst c con cha with
    | None -> Some false
    | Some v2 -> compatible_and_progress_attribute_value v1 v2 in
  match cha with
  | Some cha -> check cha
  | None ->
    List.fold_left (fun acc cha ->
      let cha = Utils.Id.to_array cha in
      merge_progress acc (check cha)) (Some false) (other_players st inst)

(** As [respect_constraints], but takes an instanciation and thus also checks
 * global constraints. **)
let respect_constraints_inst inst st conss evs c =
  let cst = State.get_character_state st in
  respect_constraints_events (check_contact inst st c) cst conss evs c

let compatible_and_progress st (e, other) inst =
  let cst = State.get_character_state st in
  let compatible_others =
    List.fold_left (fun acc c ->
        merge_progress acc
          (respect_constraints_base (check_contact inst st c) cst other c))
      (Some false) (other_players st inst) in
  Utils.array_fold_lefti (fun i acc c ->
    let (conss, evs, rs) = e.(i) in
    merge_progress acc
      (respect_constraints_inst inst st conss evs c)) compatible_others inst

let search_instantiation st (e, other) =
  let cst = State.get_character_state st in
  let all_players = State.all_players st in
  (** Players that can be placed as [other]. **)
  let possible_other =
    Utils.PSet.from_list
      (List.filter (fun c ->
        respect_constraints cst other [] c <> None) all_players) in
  let possible_players_progress_no_progress =
    Array.map (fun ei ->
      let (conss, evs, rs) = ei in
      let result_list =
        List.map (fun c ->
          (c, respect_constraints cst conss evs c)) all_players in
      let compatible_list =
        List.filter (fun (_, d) -> d <> None) result_list in
      let progress, no_progress =
        List.partition (fun (_, d) -> d = Some true) compatible_list in
      (List.map fst progress, List.map fst no_progress)) e in
  let possible_players =
    (** Possible players for each variable.
     * Players that make the state progress are always put forwards. **)
    Array.map (fun (p, np) ->
        Utils.shuffle p @ Utils.shuffle np) possible_players_progress_no_progress in
  (** The following array indicates which player variable should be considered
   * first. **)
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
          (** The list [partial_instantiation] has been build backward. **)
          let i = Array.length e - 1 - i in
          let i = redirection_array.(i) in
          inst.(i) <- j) partial_instantiation ;
        inst in
      (match compatible_and_progress st (e, other) inst with
       | None -> None
       | Some progress -> Some (inst, progress))
    | i :: redirection_list ->
      let possible = possible_players.(i) in
      let partial_instantiation_set = Utils.PSet.from_list partial_instantiation in
      (** We remove already chosen players (a player can’t be chosen twice). **)
      let possible =
        List.filter (fun j -> not (Utils.PSet.mem j partial_instantiation_set))
          possible in
      (** We filter out possibilities that would make further instantiations
       * impossible to satisfy the constraints on [other]. **)
      let possible =
        let possible_future =
          Utils.PSet.flatten (Utils.PSet.from_list (List.map (fun i ->
            Utils.PSet.from_list (possible_players.(i))) redirection_list)) in
        List.filter (fun j ->
          List.for_all (fun p ->
              p = j
              || Utils.PSet.mem p partial_instantiation_set
              || Utils.PSet.mem p possible_other
              || Utils.PSet.mem p (Utils.PSet.remove j possible_future))
            all_players) possible in
      let rec aux' = function
        | [] -> None (** No instantiation led to a compatible state. **)
        | j :: l ->
          match aux (j :: partial_instantiation) redirection_list with
          | None -> aux' l
          | Some r -> Some r in
      aux' possible in
  aux [] (Array.to_list redirection_array)

let apply state (e, other) inst =
  let diff = PMap.empty in
  let other_players = other_players state inst in
  let update_diff diff a i =
    let old_value =
      try PMap.find a diff
      with Not_found -> 0 in
    PMap.add a (old_value + i) diff in
  let apply_constraint c (state, diff) =
    let cstate = State.get_character_state state in function
    | Attribute (a, v1) ->
      let diff =
        match State.get_attribute_character cstate c a with
        | None ->
          State.write_attribute_character cstate c a v1 ;
          update_diff diff (State.PlayerAttribute a)
            (if State.attribute_value_can_progress v1 then -1 else 0)
        | Some v2 ->
          let v3 =
            Utils.assert_option __LOC__ (State.compose_attribute_value v1 v2) in
          State.write_attribute_character cstate c a v3 ;
          update_diff diff (State.PlayerAttribute a)
            (if State.attribute_value_progress v2 v3 then 1 else 0) in
      (state, diff)
    | Contact (con, cha, v1) ->
      let apply_contact (state, diff) cha =
        let diff =
          match State.get_contact_character cstate c con cha with
          | None ->
            State.write_contact_character cstate c con cha v1 ;
            update_diff diff (State.ContactAttribute con)
              (if State.attribute_value_can_progress v1 then -1 else 0)
          | Some v2 ->
            let v3 =
              Utils.assert_option __LOC__ (State.compose_attribute_value v1 v2) in
            State.write_contact_character cstate c con cha v3 ;
            update_diff diff (State.ContactAttribute con)
              (if State.attribute_value_progress v2 v3 then 1 else 0) in
        (state, diff) in
      match cha with
      | Some cha ->
        let cha = inst.(cha) in
        apply_contact (state, diff) cha
      | None ->
        List.fold_left apply_contact (state, diff) other_players in
  let apply_constraints state diff c =
    List.fold_left (apply_constraint c) (state, diff) in
  let (state, diff) =
    List.fold_left (fun (state, diff) c -> apply_constraints state diff c other)
      (state, diff) other_players in
  Utils.array_fold_left2 (fun (state, diff) ei c ->
    let (conss, evs, rs) = ei in
    let (state, diff) = apply_constraints state diff c conss in
    (* TODO: Event [evs] *)
    let _ =
      Array.iteri (fun i r ->
        let c' = inst.(i) in
        if c <> c' then
          State.add_relation state c c' r) rs in
    (state, diff)) (state, diff) e inst

let apply_relations state (e, _) inst =
  let result =
    State.create_relation_state (State.number_of_player_relation_state state) in
  Array.iter2 (fun ei c ->
    let (conss, evs, rs) = ei in
    Array.iteri (fun i r ->
      let c' = inst.(i) in
      if c <> c' then
        let r' = State.read_relation_state state c c' in
        State.add_relation_state result c c' (Relation.compose r' r)) rs) e inst ;
  result

