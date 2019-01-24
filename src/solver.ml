
(** Given a state [s] and an element [e], returns [None] if the element
 * can’t be applied whilst increasing the evaluation of the state relations.
 * If it can, it returns a triple with:
 * - the instantiation that makes it so,
 * - whether the element helps progressing with the attribute values,
 * - the new evaluation of the relations.
 * The argument [evs] must be [evaluate (State.get_relation_state s)]
 * (it is passed to avoid recomputing it each time). **)
let grade_evs s evs e =
  Utils.if_option (Element.search_instantiation s e) (fun (inst, progress) ->
    let new_relations = Element.apply_relations s e inst in
    let ev = evaluate new_relations in
    if ev < evs then None
    else Some (inst, progress, ev))

(** As [grade_evs], but withouth the need to provide the [evs] argument. **)
let grade s =
  grade_evs s (evaluate (State.get_relation_state s))

(** Takes a “grade” made of the progress of an element and the gain
 * of evaluation due to an element. **)
let compare_grade (progress1, ev1) (progress2, ev2) =
  (** [test] receives two booleans: if one has it and not the other,
   * then this one is definitely better.
   * Otherwise, we have to continue looking. **)
  let test b1 b2 c =
    if b1 && not b2 then 1
    else if b2 && not b1 then (-1)
    else c () in
  test (ev1 > 0 && progress1) (ev2 > 0 && progress2) (fun _ ->
    test (ev1 >= 0 && progress1) (ev2 >= 0 && progress2) (fun _ ->
      test (ev1 > 0) (ev2 > 0) (fun _ ->
        test (ev1 >= 0) (ev2 >= 0) (fun _ ->
          compare ev1 ev2))))

(** This function iterates of the current state [s] and the pool [p].
 * It is parameterised by two numbers:
 * - [optimistic]: the function first extracts this number of elements
 *   from the pool, hoping that several will apply.
 *   If some apply, the best element is then applied.
 * - [greedy]: In the case where the optimistic approach did not work,
 *   the function extracts up to this number of new elements, halting
 *   on the first one that applies. **)
let step optimistic greedy s p =
  let evs = evaluate (State.get_relation_state s) in
  let rec optimistic_pick p = function
    | 0 -> (p, [])
    | n ->
      let (e, p) = Pool.pick p in
      match e with
      | None -> (p, []) (* Recursive calls won’t magically make elements appear. *)
      | Some e ->
        let (p, l) = optimistic_pick p (n - 1) in
        match grade_evs s evs e with
        | None -> (p, l)
        | Some (inst, progress, ev) -> (p, (e, inst, (progress, ev - evs)) :: l) in
  let (p, l) = optimistic_pick optimistic p in
  match argmax (fun (e1, inst1, g1) (e2, inst2, g2) -> compare_grade g1 g2) l with
  | Some (e, inst, g) -> (p, Some (Element.apply s e inst))
  | None ->
    let rec greedy_pick p = function
      | 0 -> (p, None)
      | n ->
        let (e, p) = Pool.pick p in
        match e with
        | None -> (p, None) (* Recursive calls won’t magically make elements appear. *)
        | Some e ->
          match grade_evs s evs e with
          | None -> greedy_pick p (n - 1)
          | Some (inst, progress, ev) -> (p, Some (Element.apply s e inst)) in
    greedy_pick p greedy


(* TODO: Clean

let sort_characters_by_expectation s =
  TODO (* Return the list of characters, the first one being the one
    the farthest away from its expectations, and the last one the
    closest, in the current state s. *)

let solver_step s u =
  match Utils.match_left u with
  | None -> (s, u)
  | Some ((g, _), u) ->
    match sort_characters_by_expectation s with
    | [] -> (* No character! *) (s, u)
    | c :: cl ->
      let (rl, hl, uf, g) = Generator.apply g s c cl in
      let is_compatible = TODO in
      let s =
        if is_compatible then
          TODO (* apply everything *)
        else s in
      let was_a_success =
        (rl <> [] || hl <> []) && is_compatible in
      let u =
        match uf with
        | Generator.Reuse_as_soon_as_possible ->
          if was_a_success then
            Utils.add_left (g, true) u
          else Utils.add_right (g, false) u
        | Generator.Reuse_later ->
          Utils.add_right (g, was_a_success) u
        | Generator.Do_not_reuse -> u in
      (s, u)

let rec iterate_step s u =
  if Utils.for_all (fun (_, b) -> not b) u then
    s (* All generators have been called and failed to produce anything. *)
  else
    let (s, u) = solver_step s u in
    iterate_step s u

let rec solver s = function
  | [] -> s
  | u :: ul ->
    let u = Utils.two_direction_list_from_list (List.map (fun g -> (g, true)) u) in
    solver (iterate_step s u) ul

*)

