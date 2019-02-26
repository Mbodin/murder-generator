
type global = {
    element_register : Element.t Utils.Id.map
      (** The set of all registered elements. **) ;
    constructor_informations : State.constructor_maps
      (** Informations about constructors. **) ;
    pool_informations : Pool.global (** Informations needed by the pool. **)
  }

(** Reads the element from the register map. **)
let read_element g e =
  Utils.assert_option __LOC__ (Utils.Id.map_inverse g.element_register e)

let empty_global = {
    element_register = Utils.Id.map_create () ;
    constructor_informations = State.empty_constructor_maps ;
    pool_informations = Pool.empty_global
  }

let register_element g e =
  let (eid, m) = Utils.Id.map_insert_t g.element_register e in
  let attrs = Element.provided_attributes e in
  { g with element_register = m ;
           pool_informations = Pool.add_element g.pool_informations eid attrs }

let filter_elements g f =
  { g with pool_informations =
      Pool.filter_global g.pool_informations (fun e ->
        f (read_element g e)) }

type objective = {
    difficulty : int ;
    complexity : int
  }

(** Evaluates a character [c] in a relation state [s] compared to its objective [o].
 * Complexity is difficult to compensate later on, whilst difficulty
 * is easy (it’s just applying an helping element).
 * The evaluation thus punishes more increases of complexity above its target than
 * difficulty. **)
let evaluate_character o s c =
  let collect f =
    Utils.sum (List.map (Utils.compose f (State.read_relation_state s c))
      (State.all_players_relation s)) in
  let d = collect Relation.difficulty - o.difficulty in
  let s = collect Relation.complexity - o.complexity in
  (if s >= 0 then - 2 * s * s * s else s) - d * d

let evaluate o s =
  Utils.array_sum (Array.mapi (fun i o ->
    evaluate_character o s (Utils.Id.from_array i)) o)

(** Given an array of objectives [o], a state [s], and an element [e],
 * returns [None] if the element can’t be applied whilst increasing
 * the evaluation of the state relations.
 * If it can, it returns a triple with:
 * - the instantiation that makes it so,
 * - whether the element helps progressing with the attribute values,
 * - the change in the relation evaluation.
 * The argument [evs] must be [evaluate (State.get_relation_state s)]
 * (it is passed to avoid recomputing it each time). **)
let grade_evs o s evs e =
  Utils.if_option (Element.search_instantiation s e) (fun (inst, progress) ->
    let new_relations =
      Element.apply_relations (State.get_relation_state s) e inst in
    let ev = evaluate o new_relations in
    let dev = ev - evs in
    if dev < 0 then None
    else Some (inst, progress, dev))

(** As [grade_evs], but withouth the need to provide the [evs] argument. **)
let grade o s =
  grade_evs o s (evaluate o (State.get_relation_state s))

(** Takes a “grade” made of the progress of an element and the gain
 * of evaluation due to an element. **)
let compare_grade (progress1, dev1) (progress2, dev2) =
  (** [test] receives two booleans: if one has it and not the other,
   * then this one is definitely better.
   * Otherwise, we have to continue looking. **)
  let test b1 b2 c =
    if b1 && not b2 then 1
    else if b2 && not b1 then (-1)
    else c () in
  test (dev1 > 0 && progress1) (dev2 > 0 && progress2) (fun _ ->
    test (dev1 >= 0 && progress1) (dev2 >= 0 && progress2) (fun _ ->
      compare dev1 dev2))

(** This function iterates on the current state [s] and the pool [p], using the
 * global register [g] and the objective [o].
 * It is parameterised by two numbers:
 * - [optimistic]: the function first extracts this number of elements
 *   from the pool, hoping that several will apply.
 *   If some apply, the best element is then applied.
 * - [greedy]: In the case where the optimistic approach did not work,
 *   the function extracts up to this number of new elements, halting
 *   on the first one that applies. **)
let step g o optimistic greedy s p =
  let evs = evaluate o (State.get_relation_state s) in
  let rec optimistic_pick p = function
    | 0 -> (p, [])
    | n ->
      let (e, p) = Pool.pick p in
      match e with
      | None -> (p, [])
      | Some e ->
        let e = read_element g e in
        let (p, l) = optimistic_pick p (n - 1) in
        match grade_evs o s evs e with
        | None -> (p, l)
        | Some (inst, progress, dev) -> (p, (e, inst, (progress, dev)) :: l) in
  let (p, l) = optimistic_pick optimistic p in
  match Utils.argmax (fun (e1, inst1, g1) (e2, inst2, g2) ->
          compare_grade g1 g2) l with
  | Some (e, inst, g) -> (p, Some (Element.apply s e inst))
  | None ->
    let rec greedy_pick p = function
      | 0 -> (p, None)
      | n ->
        let (e, p) = Pool.pick p in
        match e with
        | None -> (p, None)
        | Some e ->
          let e = read_element g e in
          match grade_evs o s evs e with
          | None -> greedy_pick p (n - 1)
          | Some (inst, progress, dev) -> (p, Some (Element.apply s e inst)) in
    greedy_pick p greedy

let solve g s o =
  Lwt.return s (* TODO *)

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

let rec solve s = function
  | [] -> s
  | u :: ul ->
    let u = Utils.two_direction_list_from_list (List.map (fun g -> (g, true)) u) in
    solver (iterate_step s u) ul

*)

