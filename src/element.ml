
type character = Utils.Id.t

type character_constraint =
  | Attribute of State.PlayerAttribute.attribute * State.PlayerAttribute.value State.attribute_value
  | Contact of State.ContactAttribute.attribute * int * State.ContactAttribute.value State.attribute_value

type element =
  (character_constraint list
   * History.event list
   * Relations.t array
  ) array

(** States whether [v1] and [v2] are compatible and make some progress.
 * The return value is expressed as for [compatible_and_progress]:
 * [None] means that it is not compatible, [Some true] that it is
 * compatible and progress, and [Some false] that it is compatible
 * but does not progress. **)
let compatible_and_progress_attribute_value v1 v2 =
  Utils.option_map (State.attribute_value_progress v1)
    (State.compose_attribute_value v1 v2)

(** Merges two results of [compatible_and_progress]. **)
let merge_progress b1 b2 =
  match b1, b2 with
  | None, _ | _, None -> None
  | Some b1, Some b2 -> Some (b1 || b2)

(** Checks whether the constraints [conss] are valid for the
 * character [c] in the character state [cst].
 * The contact case is given as argument as the function [f]. **)
let respect_constraints_base f cst conss evs c =
  merge_progress
    (List.fold_left (fun b cons ->
      merge_progress b
        (match cons with
        | Attribute (a, v1) ->
          (match State.get_attribute_character cst c a with
           | None -> Some false
           | Some v2 ->
             compatible_and_progress_attribute_value v1 v2)
        | Contact (con, cha, v) ->
          f con cha v)) (Some false) conss)
    (* TODO: events [evs] *) (Some false)

(** Checks whether the constraints [conss] are locally valid for the
 * character [c] in the character state [cst].
 * Events are also checked to be addable to the characters’s events.
 * Only local constraints are considered: no constraint depending on the
 * instantiation are checked at this point. **)
let respect_constraints =
  respect_constraints_base (fun _ _ _ -> Some false)

(** As [respect_constraints], but takes an instanciation and thus also checks
 * global constraints. **)
let respect_constraints_inst inst cst conss evs c =
  respect_constraints_base (fun con cha v1 ->
    let cha = inst.(cha) in
    match State.get_contact_character cst c con cha with
    | None -> Some false
    | Some v2 -> compatible_and_progress_attribute_value v1 v2) cst conss evs c

let compatible_and_progress st e inst =
  let cst = State.get_character_state st in
  Utils.array_fold_lefti (fun i acc c ->
    let (conss, evs, rs) = e.(i) in
    merge_progress acc
      (respect_constraints_inst inst cst conss evs c)) (Some false) inst

let search_instantiation st e =
  let cst = State.get_character_state st in
  let all_players = State.all_players st in
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
    let (conss, evs, rs) = ei in
    let update_diff diff a i =
      let old_value =
        try PMap.find a diff
        with Not_found -> 0 in
      PMap.add a (old_value + i) diff in
    let (state, diff) =
      List.fold_left (fun (state, diff) -> function
        | Attribute (a, v1) ->
          let cstate = State.get_character_state state in
          let diff =
            match State.get_attribute_character cstate c a with
            | None ->
              State.write_attribute_character cstate c a v1 ;
              update_diff diff (State.PlayerAttribute a)
                (if State.attribute_value_can_progress v1 then -1 else 0)
            | Some v2 ->
              match State.compose_attribute_value v1 v2 with
              | None -> assert false
              | Some v3 ->
                State.write_attribute_character cstate c a v3 ;
                update_diff diff (State.PlayerAttribute a)
                  (if State.attribute_value_progress v2 v3 then 1 else 0) in
          (state, diff)
        | Contact (con, cha, v) ->
          (state, diff) (* TODO *)
        ) (state, diff) conss in
    (* TODO: Event [evs] *)
    let _ =
      Array.iter2 (fun c' r ->
        if c <> c' then
          let r' = State.get_relation state c c' in
          State.write_relation state c c' (Relations.compose r' r)) inst rs in
    (state, diff)) (state, PMap.empty) e inst

