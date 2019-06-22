
open ExtList


type character = Id.t

type character_constraint =
  | Attribute of Attribute.PlayerAttribute.attribute
                 * Attribute.PlayerAttribute.constructor State.attribute_value
  | Contact of Attribute.ContactAttribute.attribute
               * int option
               * Attribute.ContactAttribute.constructor State.attribute_value

type cell = {
    constraints : character_constraint list ;
    relations : Relation.t array ;
    added_objective : State.objective
  }

type t = {
    status : History.status ;
    players : cell array ;
    others : character_constraint list ;
    events : int Events.t list
  }

(** Returns the list of attributes provided by this constraint. **)
let provided_attributes_constraint =
  let aux a = function
    | State.Fixed_value _ -> PSet.singleton a
    | _ -> PSet.empty in function
  | Attribute (a, v) -> aux (Attribute.PlayerAttribute a) v
  | Contact (a, _, v) -> aux (Attribute.ContactAttribute a) v

let provided_attributes e =
  let provided_attributes_constraint_list =
    List.fold_left (fun s c ->
      PSet.merge s (provided_attributes_constraint c)) in
  PSet.to_list (Array.fold_left (fun s ce ->
      provided_attributes_constraint_list s ce.constraints)
    (provided_attributes_constraint_list PSet.empty e.others) e.players)

(** States whether [v1] and [v2] are compatible and make some progress.
 * The return value is expressed as for [compatible_and_progress]:
 * [None] means that it is not compatible, [Some (lazy true)] that it is
 * compatible and progress, and [Some (lazy false)] that it is compatible
 * but does not progress.
 * As for [State.compose_attribute_value], is takes as an argument a
 * function stating whether two base value are compatible. **)
let compatible_and_progress_attribute_value compatible v1 v2 =
  Utils.apply_option (State.compose_attribute_value compatible v1 v2)
    (fun v' -> lazy (State.attribute_value_progress v1 v'))

(** Merges two results of [compatible_and_progress].
 * The second argument is meant to be lazily evaluated. **)
let merge_progress b1 b2 =
  Utils.if_option b1 (fun b1 ->
    Utils.apply_option (Lazy.force b2) (fun b2 ->
      lazy (Lazy.force b1 || Lazy.force b2)))

(** Checks whether the constraints [conss] are valid for the
 * character [c] in the state [st].
 * The contact case is given as argument as the function [f].
 * The argument [m] is of type [State.constructor_maps] and
 * is an argument of most functions in this file. **)
let respect_constraints_base f m st conss c =
  let cst = State.get_character_state st in
  List.fold_left (fun b cons ->
    merge_progress b
     (lazy (match cons with
      | Attribute (a, v1) ->
        (match State.get_attribute_character cst c a with
         | None -> Some (lazy false)
         | Some v2 ->
           let compatible =
             Attribute.PlayerAttribute.is_compatible m.Attribute.player a in
           compatible_and_progress_attribute_value compatible v1 v2)
      | Contact (con, cha, v) ->
        f con cha v))) (Some (lazy false)) conss

(** Checks whether the constraints [conss] are valid for the
 * character [c] in the state [st].
 * In addition to [respect_constraints_base], this
 * function also checks events constraints [evs]. **)
let respect_constraints_events f m st conss evs c =
  let hst = State.get_history_state st in
  merge_progress
    (respect_constraints_base f m st conss c)
    (lazy (History.lcompatible_and_progress hst evs))

(** Checks whether the constraints [conss] are locally valid for the
 * character [c] in the character state [cst].
 * Events are also checked to be addable to the characters’s events.
 * Only local constraints are considered: no constraint depending on the
 * instantiation are checked at this point. **)
let respect_constraints =
  respect_constraints_events (fun _ _ _ -> Some (lazy false))

(** Returns all the players of the state [st] that are not in the
 * instantiation [inst]. **)
let other_players st inst =
  List.filter (fun i -> not (Array.exists ((=) i) inst)) (State.all_players st)

(** A function to be given to [respect_constraints_events] to deal with the
 * instantiation-dependent contacts. **)
let check_contact m inst st c con cha v1 =
  let cst = State.get_character_state st in
  let check cha =
    match State.get_contact_character cst c con cha with
    | None -> Some (lazy false)
    | Some v2 ->
      let compatible =
        Attribute.ContactAttribute.is_compatible m.Attribute.contact con in
      compatible_and_progress_attribute_value compatible v1 v2 in
  match cha with
  | Some cha ->
    if Utils.assert_defend then assert (cha < Array.length inst) ;
    let cha = inst.(cha) in
    check cha
  | None ->
    List.fold_left (fun acc cha ->
        merge_progress acc (lazy (check cha)))
      (Some (lazy false)) (other_players st inst)

(** As [respect_constraints], but takes an instanciation and thus also checks
 * global constraints. **)
let respect_constraints_inst m inst st conss evs c =
  respect_constraints_events (check_contact m inst st c) m st conss evs c

(** Returns an instantiated list of events with the given instantiation. **)
let instantiate_events e inst =
  Utils.assert_option __LOC__ (Utils.list_map_option
    (Events.instantiate (fun i -> Some (inst.(i)))) e.events)

let compatible_and_progress m st e inst =
  let compatible_others =
    List.fold_left (fun acc c ->
        merge_progress acc
          (lazy (respect_constraints_base
                  (check_contact m inst st c) m st e.others c)))
      (Some (lazy false)) (other_players st inst) in
  Utils.array_fold_lefti (fun i acc c ->
      let conss = e.players.(i).constraints in
      let evs = instantiate_events e inst in
      merge_progress acc
        (lazy (respect_constraints_inst m inst st conss evs c)))
    compatible_others inst

let search_instantiation m st e =
  let all_players = State.all_players st in
  (** Players that can be placed as [e.others]. **)
  (* TODO: This needs to be cached as this set can only shrink. *)
  let possible_other =
    PSet.from_list
      (List.filter (fun c ->
        respect_constraints m st e.others [] c <> None) all_players) in
  let possible_players_progress_no_progress =
    Array.mapi (fun i ei ->
      let conss = ei.constraints in
      let result_list =
        List.map (fun c ->
          let evs =
            Utils.list_map_filter (Events.partially_instantiate i c) e.events in
          (c, respect_constraints m st conss evs c)) all_players in
      let compatible_list =
        Utils.list_map_filter (fun (c, d) ->
          Option.map (fun b -> (c, Lazy.force b)) d) result_list in
      let progress, no_progress =
        List.partition snd compatible_list in
      (List.map fst progress, List.map fst no_progress)) e.players in
  let possible_players =
    (* TODO: This needs to be cached as this set can only shrink. *)
    (** Possible players for each variable.
     * Players that make the state progress are always put forwards. **)
    Array.map (fun (p, np) ->
        Utils.shuffle p @ Utils.shuffle np) possible_players_progress_no_progress in
  (** The following array indicates which player variables should be considered
   * first: the one with the fewest number of possibilities. **)
  let redirection_array =
    let redirection_array = Utils.seq_array (Array.length e.players) in
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
        let inst = Array.make (Array.length e.players) (Id.from_array (-1)) in
        List.iteri (fun i j ->
          let i = redirection_array.(i) in
          inst.(i) <- j) (List.rev partial_instantiation) ;
        inst in
      (match compatible_and_progress m st e inst with
       | None -> None
       | Some progress -> Some (inst, Lazy.force progress))
    | i :: redirection_list ->
      let possible = possible_players.(i) in
      let partial_instantiation_set = PSet.from_list partial_instantiation in
      (** We remove already chosen players (a player can’t be chosen twice). **)
      let possible =
        List.filter (fun j -> not (PSet.mem j partial_instantiation_set))
          possible in
      (** We filter out possibilities that would make further instantiations
       * impossible to satisfy the constraints on [e.others]. **)
      let possible =
        let possible_future =
          PSet.flatten (PSet.from_list (List.map (fun i ->
            PSet.from_list (possible_players.(i))) redirection_list)) in
        List.filter (fun j ->
          List.for_all (fun p ->
              p = j
              || PSet.mem p partial_instantiation_set
              || PSet.mem p possible_other
              || PSet.mem p (PSet.remove j possible_future))
            all_players) possible in
      let rec aux' = function
        | [] -> None (** No instantiation led to a compatible state. **)
        | j :: l ->
          match aux (j :: partial_instantiation) redirection_list with
          | Some r -> Some r
          | None ->
            (* TODO: if [partial_instantiation = []], this means that we are
             * considering the first variable, which can definitely not be [j]
             * as all other cases fail to lead to an instantiation.
             * This means that we can remove [j] from the cache in the possible
             * values for the first variable.
             * We can also do that if all the previous instanciated variables
             * have a [possible] set of size exactly [1]: the best way is thus
             * to simply propagate a boolean. *)
            aux' l in
      aux' possible in
  aux [] (Array.to_list redirection_array)

(** This type represents the difference of attributes that have been fixed with
 * the ones that have been created, as a number for each attribute.
 * For instance, if an instantiation defines an attribute [a] that was to be defined
 * (that is, for which [State.attribute_value_can_progress] returned [true]),
 * it will associate [1] to [a]; if it adds an attribute to be defined, it will
 * associate [-1] instead.
 * The total sum of this map is also stored, as well as the list of elements
 * associated with a negative number. **)
type attribute_differences =
  (Attribute.attribute, int) PMap.t * int * Attribute.attribute list

let empty_difference = (PMap.empty, 0, [])

let difference_weigth (_, w, _) = w

let difference_attribute_in_need (_, _, l) = l

let difference_for_attribute (m, _, _) a =
  try PMap.find a m
  with Not_found -> 0

let update_difference (m, w, l) a d =
  let d_old = difference_for_attribute (m, w, l) a in
  (PMap.add a (d_old + d) m, w + d,
    if d_old < 0 && d_old + d >= 0 then List.remove l a else l)

let merge_attribute_differences (m1, s1, l1) (m2, s2, l2) =
  let (m, li, lo) =
    PMap.foldi (fun a v2 (m, li, lo) ->
      let v1 = difference_for_attribute (m1, s1, l1) a in
      (PMap.add a (v1 + v2) m,
       (if v1 >= 0 && v1 + v2 < 0 then a :: li else li),
       (if v1 < 0 && v1 + v2 >= 0 then a :: lo else lo))) m2 (m1, [], []) in
  (m, s1 + s2, li @ List.filter (fun a -> not (List.mem a lo)) l1)

let apply m state e inst =
  if Utils.assert_defend then
    assert (compatible_and_progress m state e inst <> None) ;
  let evs = instantiate_events e inst in
  let diff = empty_difference in
  let other_players = other_players state inst in
  let apply_constraint c (state, diff) =
    let cst = State.get_character_state state in function
    | Attribute (a, v1) ->
      let compatible =
        Attribute.PlayerAttribute.is_compatible m.Attribute.player a in
      let diff =
        match State.get_attribute_character cst c a with
        | None ->
          State.write_attribute_character cst c a v1 ;
          update_difference diff (Attribute.PlayerAttribute a)
            (if State.attribute_value_can_progress v1 then -1 else 0)
        | Some v2 ->
          let v3 =
            Utils.assert_option __LOC__
              (State.compose_attribute_value compatible v1 v2) in
          State.write_attribute_character cst c a v3 ;
          update_difference diff (Attribute.PlayerAttribute a)
            (if State.attribute_value_progress v2 v3 then 1 else 0) in
      (state, diff)
    | Contact (con, cha, v1) ->
      let compatible =
        Attribute.ContactAttribute.is_compatible m.Attribute.contact con in
      let apply_contact (state, diff) cha =
        let diff =
          match State.get_contact_character cst c con cha with
          | None ->
            State.write_contact_character cst c con cha v1 ;
            update_difference diff (Attribute.ContactAttribute con)
              (if State.attribute_value_can_progress v1 then -1 else 0)
          | Some v2 ->
            let v3 =
              Utils.assert_option __LOC__
                (State.compose_attribute_value compatible v1 v2) in
            State.write_contact_character cst c con cha v3 ;
            update_difference diff (Attribute.ContactAttribute con)
              (if State.attribute_value_progress v2 v3 then 1 else 0) in
        (state, diff) in
      match cha with
      | Some cha ->
        if Utils.assert_defend then assert (cha < Array.length inst) ;
        let cha = inst.(cha) in
        apply_contact (state, diff) cha
      | None ->
        List.fold_left apply_contact (state, diff) other_players in
  let apply_constraints state diff c =
    List.fold_left (apply_constraint c) (state, diff) in
  let (state, diff) =
    List.fold_left (fun (state, diff) c -> apply_constraints state diff c e.others)
      (state, diff) other_players in
  let (state, diff) =
    Utils.array_fold_left2 (fun (state, diff) ei c ->
      let (state, diff) = apply_constraints state diff c ei.constraints in
      Array.iteri (fun i r ->
        let c' = inst.(i) in
        if c <> c' then
          State.add_relation state c c' r) ei.relations ;
      let rst = State.get_relation_state state in
      State.add_difficulty rst c ei.added_objective.State.difficulty ;
      State.add_complexity rst c ei.added_objective.State.complexity ;
      (state, diff)) (state, diff) e.players inst in
  let state = State.apply_events state e.status evs in
  (state, diff)

let safe_apply m state = apply m (State.copy state)

let apply_relations state e inst =
  let result = State.copy_relation_state state in
  Array.iter2 (fun ei c ->
      Array.iteri (fun i r ->
        let c' = inst.(i) in
        if c <> c' then
          State.add_relation_state result c c' r) ei.relations ;
      State.add_difficulty result c ei.added_objective.State.difficulty ;
      State.add_complexity result c ei.added_objective.State.complexity)
    e.players inst ;
  result

