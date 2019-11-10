
(** Cache is used a lot in this file.
 * We thus introduce the following shortcuts functions to manipulate it. **)
let getv = Utils.get_value
let getc = Utils.get_cache
let updatec s s' =
  Utils.cached s' (getc s)

type global = {
    branch_exploration : float ;
    element_register : Element.t Id.map
      (** The set of all registered elements. **) ;
    constructor_informations : Attribute.constructor_maps
      (** Informations about constructors. **) ;
    pool_informations : Pool.global (** Informations needed by the pool. **) ;
    all_elements : Id.t BidirectionalList.t
      (** The list of all elements. **)
  }

(** Read the element from the register map. **)
let read_element g e =
  Utils.assert_option __LOC__ (Id.map_inverse g.element_register e)

let empty_global p = {
    branch_exploration = p *. p ;
    element_register = Id.map_create () ;
    constructor_informations = Attribute.empty_constructor_maps ;
    all_elements = BidirectionalList.empty ;
    pool_informations = Pool.empty_global
  }

let register_element g e =
  let (eid, m) = Id.map_insert_t g.element_register e in
  let attrs = Element.provided_attributes e in
  { g with element_register = m ;
           all_elements = BidirectionalList.add_left eid g.all_elements ;
           pool_informations = Pool.register_element g.pool_informations eid attrs }

let filter_elements g f =
  let f e = f (read_element g e) in
  { g with all_elements = BidirectionalList.filter f g.all_elements ;
           pool_informations =
             Pool.filter_global g.pool_informations f }

(** Evaluate a character [c] in a relation state [s] compared to its objective [o].
 * Complexity is difficult to compensate later on, whilst difficulty
 * is easy (it’s just applying an helping element).
 * The evaluation thus punishes more increases of complexity above its target than
 * difficulty. **)
let evaluate_character o s c =
  let d = State.character_difficulty s c - o.State.difficulty in
  let s = State.character_complexity s c - o.State.complexity in
  (if s >= 0 then - 2 * s * s * s else s) - d * d

let evaluate o s =
  Utils.array_sum (Array.mapi (fun i o ->
    evaluate_character o s (Id.from_array i)) o)

let evaluate_state o s =
  evaluate o (State.get_relation_state s)

(** Given an array of objectives [o], a state [s], and an element [e],
 * return [None] if the element can’t be applied at all.
 * If it can, it returns a triple with:
 * - the instantiation that makes it so,
 * - whether the element helps progressing with the attribute values,
 * - the new relation evaluation. **)
let grade g o s e =
  Utils.apply_option (Element.search_instantiation g.constructor_informations s e)
    (fun (inst, progress) ->
      let new_relations =
        Element.apply_relations (State.get_relation_state (getv s)) e inst in
      let ev = evaluate o new_relations in
      if Utils.assert_defend then (
        let (s', m) =
          Element.safe_apply g.constructor_informations (getv s) e inst in
        assert (ev = evaluate_state o s')) ;
      (inst, progress, ev))

(** Take a “grade” made of the progress of an element and the evaluation
 * due to an element.
 * It takes the base evaluation [ev0] to be compared with as an argument. **)
let compare_grade ev0 (progress1, ev1) (progress2, ev2) =
  (** [test] receives two booleans: if one has it and not the other,
   * then this one is definitely better.
   * Otherwise, we have to continue looking. **)
  let test b1 b2 c =
    if b1 && not b2 then 1
    else if b2 && not b1 then -1
    else c () in
  let dev1 = ev1 - ev0 in
  let dev2 = ev2 - ev0 in
  test (dev1 > 0 && progress1) (dev2 > 0 && progress2) (fun _ ->
    test (dev1 >= 0 && progress1) (dev2 >= 0 && progress2) (fun _ ->
      if dev1 = dev2 then
        test progress1 progress2 (fun _ -> 0)
      else compare dev1 dev2))

(** Return a number between [mi] and [ma] depending on the parameter
 * [branch_exploration] of the solver. **)
let get_branch g mi ma =
  if mi = ma then mi
  else mi + int_of_float (0.5 +. g.branch_exploration *. float_of_int (ma - mi))

(** Sometimes, it is not worth trying some branches unless we have a large
 * amount of power.
 * This function returns [d] at low power, otherwise returns a number
 * between [mi] and [ma]. **)
let high_get_branch g d mi ma =
  if g.branch_exploration < 0.5 then d
  else get_branch { g with branch_exploration =
                             2. *. (g.branch_exploration -. 0.5) } mi ma

(** This function iterates on the current state [s] and the pool [p], using the
 * global register [g] and the objective [o].
 * Elements that can’t be applied at all are progressively removed from the pool.
 * It is parameterised by two numbers:
 * - [optimistic]: the function first extracts this number of elements
 *   from the pool, hoping that several will apply.
 *   If some apply, the best element is then applied.
 * - [greedy]: in the case where the optimistic approach did not work,
 *   the function extracts up to this number of new elements, halting
 *   on the first one that applies.
 * This function returns a triple composed of the new pool, a list of
 * information about the applied events (event identifier, instantiatiation,
 * and grades), as well as a set of unapplyable events. **)
let step g o optimistic greedy (s, evs) p =
  if Utils.assert_defend then assert (evs = evaluate_state o (getv s)) ;
  (** Extracts [n] elements from the pool, filtering out the ones that don’t
   * apply, and calling [f] on the ones that does whilst increasing the
   * evaluation.
   * The call to [f] is made with a callback to the recursive call of [aux]
   * if needed. **)
  let rec aux default unapplyable f p = function
    | 0 -> (p, default, unapplyable)
    | n ->
      let (ep, p) = Pool.pop p in
      match ep with
      | None -> (p, default, unapplyable)
      | Some ep ->
        let e = read_element g ep in
        match grade g o s e with
        | None -> aux default (PSet.add ep unapplyable) f p (n - 1)
        | Some (inst, progress, ev) ->
          let p = Pool.add p ep in
          if ev < evs then
            aux default unapplyable f p (n - 1)
          else
            f (fun unapplyable -> aux default unapplyable f p (n - 1))
              unapplyable p e inst progress ev in
  (** We first try with the optimistic pick. **)
  let (p, l, unapplyable) =
    aux [] PSet.empty (fun callback unapplyable _ e inst progress ev ->
      let (p, l, unapplyable) = callback unapplyable in
      (p, (e, inst, (progress, ev)) :: l, unapplyable)) p optimistic in
  match Utils.argmax (fun (e1, inst1, g1) (e2, inst2, g2) ->
          compare_grade evs g1 g2) l with
  | Some (e, inst, (progress, ev)) ->
    let (s', m) = Element.safe_apply g.constructor_informations (getv s) e inst in
    (p, Some ((updatec s s', m), ev), unapplyable)
  | None ->
    (** We then move the the greedy pick. **)
    aux None unapplyable (fun _ unapplyable p e inst progress ev ->
        let (s', m) =
          Element.safe_apply g.constructor_informations (getv s) e inst in
        (p, Some ((updatec s s', m), ev),
         unapplyable))
      p greedy

(** Call the function [step] with heuristical arguments [optimistic] and [greedy]
 * depending on a single parameter [parameter] ranging from 0 to 101.
 * This parameter is meant to indicate the need to speed-up the analysis: at low
 * values, it means that adequate elements are being found, at high values, it
 * means that an exhaustive search is needed. **)
let weighted_step g o parameter (s, evs) p =
  let (optimistic, greedy) =
    let parameter = max 0 parameter in
    if parameter > 100 then
      (0, Pool.quick_length p)
    else (6 - get_branch g 6 0 * parameter / 100,
          2 + get_branch g 9 18 * parameter / 100) in
  step g o optimistic greedy (s, evs) p

(** Pick [optimistic] random elements and apply the best.
 * It assumes that [g.all_elements] has been shuffled, so that random elements
 * are just the first ones.
 * Returns the new global [g] with its [all_elements] updated. **)
let add_random g o optimistic (s, evs) =
  if Utils.assert_defend then assert (evs = evaluate_state o (getv s)) ;
  let (g, l) =
    let rec aux g acc = function
      | 0 -> (g, acc)
      | n ->
        match BidirectionalList.match_left g.all_elements with
        | None -> (g, acc)
        | Some (e, all_elements) ->
          let g =
            { g with all_elements =
                       BidirectionalList.add_right all_elements e } in
          let e = read_element g e in
          match grade g o s e with
          | None -> aux g acc (n - 1)
          | Some (inst, progress, ev) ->
            aux g ((e, inst, (progress, ev)) :: acc) (n - 1) in
    aux g [] optimistic in
  match Utils.argmax (fun (e1, inst1, g1) (e2, inst2, g2) ->
          compare_grade evs g1 g2) l with
  | Some (e, inst, (progress, ev)) ->
    let (s', m) = Element.safe_apply g.constructor_informations (getv s) e inst in
    (g, (updatec s s', m), ev)
  | None -> (g, (s, Element.empty_difference), evs)

(** Take a set of elements and consider them in order.
 * Any element that can be applied, that progresses, and that increase the
 * evaluation will be applied.
 * This function returns a couple of:
 * - the new state and its evaluation,
 * - the set of all elements that definitely can’t be applied. **)
let apply_elements_and_return_the_other g o (s, evs) m =
  PSet.fold (fun ep ((s, evs), m, do_not_apply) ->
    let e = read_element g ep in
    match grade g o s e with
    | None -> ((s, evs), m, PSet.add ep do_not_apply)
    | Some (inst, progress, ev) ->
      if progress && ev >= evs then
        let (s', m') =
          Element.safe_apply g.constructor_informations (getv s) e inst in
        let m = Element.merge_attribute_differences m m' in
        ((updatec s s', ev), m, do_not_apply)
      else ((s, evs), m, do_not_apply)) ((s, evs), m, PSet.empty)

(** Call the functions [weighted_step] and [add_random], trying
 * to reach a step [s] whose associated attribute difference [m]
 * has been increased by [temperature].
 * It returns a quadruple:
 * - the new global informations,
 * - a couple of state and the associated evaluation,
 * - the difference of attribute,
 * - a set of elements that might no longer apply. **)
let wide_step pause g o temperature (s, evs) m =
  let initial_weigth = Element.difference_weigth m in
  let objective_weigth =
    initial_weigth + 5 * int_of_float (log (float_of_int temperature)) + 2 in
  let step g (s, evs) m parameter =
    let l = Element.difference_attribute_in_need m in
    if l = [] ||
        (let dw = objective_weigth - Element.difference_weigth m in
         dw > 0 &&
           let p = 10 + objective_weigth - initial_weigth - dw in
           p > 0 && Random.int p = 0) then (
      let optimistic = 7 - 6 * parameter / 100 in
      let (g, (s, m'), evs) = add_random g o optimistic (s, evs) in
      Lwt.return (g, (s, evs), Element.merge_attribute_differences m m', PSet.empty)
    ) else (
      (** We select an attribute and try to increase it.
       * If we fail up or get far from the objective a given number of times,
       * we abort (then choosing another attribute). **)
      let a = Utils.select_any (Element.difference_attribute_in_need m) in
      let p = Pool.empty g.pool_informations in
      let p = Pool.add_attribute p a in
      let rec aux may_be_removed p m (s, evs) = function
        | 0 -> Lwt.return (g, (s, evs), m, may_be_removed)
        | n ->
          pause () ;%lwt
          let (p, r, unapplyable) =
            weighted_step g o (110 * parameter / 100) (s, evs) p in
          let may_be_removed = PSet.merge unapplyable may_be_removed in
          match r with
          | None -> aux may_be_removed p m (s, evs) (n - 1)
          | Some ((s', m'), evs') ->
            let m = Element.merge_attribute_differences m m' in
            let n' =
              if Element.difference_weigth m' > 0
                 || evs' > evs then n else n - 1 in
            aux may_be_removed p m (s', evs') n'
      in aux PSet.empty p m (s, evs) (get_branch g 3 5)) in
  let rec aux g (s, evs) m may_be_removed = function
    | 0 -> Lwt.return (g, (s, evs), m, may_be_removed)
    | n ->
      pause () ;%lwt
      let current_weigth = Element.difference_weigth m in
      if current_weigth >= objective_weigth then
        Lwt.return (g, (s, evs), m, may_be_removed)
      else
        let parameter =
          100 * (current_weigth - initial_weigth)
          / (objective_weigth - initial_weigth) in
        let%lwt (g, (s, evs), m, may_be_removed') = step g (s, evs) m parameter in
        let may_be_removed = PSet.merge may_be_removed' may_be_removed in
        aux g (s, evs) m may_be_removed (n - 1) in
  aux g (s, evs) m PSet.empty (get_branch g 4 10)

(** A last step, where every element is tried to be applied.
 * Only the ones making both progress and moving towards the objective are kept,
 * but steps that don’t change the apparent progress are accepted.
 * To prevent termination, this process eventually ends, though. **)
let final g (s, evs) o m =
  let rec aux g (s, evs) m acc = function
    | [] -> (g, (s, evs), m, acc)
    | e :: l ->
      match grade g o s e with
      | None -> aux g (s, evs) m acc l
      | Some (inst, progress, evs') ->
        if evs' < evs then
          aux g (s, evs) m acc l
        else (
          let (s', m') =
            Element.safe_apply g.constructor_informations (getv s) e inst in
          if Element.difference_weigth m' < 0 then
            aux g (s, evs) m acc l
          else
            let m = Element.merge_attribute_differences m m' in
            aux g ((updatec s s'), evs') m (e :: acc) l
        ) in
  let rec repeat g (s, evs) m l = function
    | 0 -> (g, (s, evs), m)
    | n ->
      let (g, (s, evs), m, l) = aux g (s, evs) m [] l in
      repeat g (s, evs) m l (n - 1) in
  let l =
    List.map (read_element g) (BidirectionalList.to_list g.all_elements) in
  repeat g (s, evs) m l (get_branch g 1 20)

(** An heuristic for the next temperature of the simulated annealing of [wider_step]
 * for when no better position have been found. **)
let next_temperature g temperature =
  get_branch g 6 8 * temperature / 10 - 1

(** An heuristic for the next temperature of the simulated annealing for when
 * a better position has been found. **)
let next_temperature_evs g temperature m evs evs' =
  if Element.difference_weigth m > 0 || evs' > evs then
    if temperature <= 2 then
      temperature
    else get_branch g 97 99 * temperature / 100 - 1
  else get_branch g 8 9 * temperature / 10 - 1

(** The maximum number of iterations that we are willing to take in the simulated
 * annealing. **)
let max_steps g = get_branch g 1_000 1_000_000

(** Estimate how many steps will be needed for the simulated annealing to terminate. **)
let estimate_steps g iteration temperature m evs =
  (** Estimate the number of steps in a given scenario where the next temperature
   * is given by [next]. **)
  let estimate next =
    let rec aux i temperature =
      if temperature <= 0 || i > max_steps g then i
      else aux (1 + i) (next temperature) in
    aux (1 + iteration) temperature in
  Utils.average [
      estimate (next_temperature g) ;
      estimate (fun temp -> next_temperature_evs g temp m evs evs) ;
      estimate (fun temp ->
        match Random.int 2 with
        | 0 -> next_temperature g temp
        | 1 -> next_temperature_evs g temp m evs (evs / 2)
        | 2 -> next_temperature_evs g temp m evs (evs + 100)
        | _ -> assert false)
    ]

(** This function performs a simulated annealing based on [wide_step].
 * This last function is considered costly and we thus starts using the [Lwt.t]
 * type here. **)
let wider_step pause g (s, evs) o m =
  (** Each step of the simulation carries the following information:
   *  - how many steps have already been taken and how many more are estimated to be
   *    needed,
   *  - the current temperature,
   *  - global informations,
   *  - the current state,
   *  - the current attribute difference. **)
  let rec aux iteration estimate temperature g (s, evs) m =
    if temperature <= 0 || iteration > max_steps g then
      Lwt.return (final g (s, evs) o m)
    else (
      let estimate =
        (** Estimating the number of step can be a costly operation.
         * We thus only redo it when we reach specific thresholds. **)
        if iteration >= estimate || iteration <= 3
           || List.mem (estimate - iteration) [
                 10; 100;
                 estimate / 5 ; estimate / 3 ; estimate / 2 ;
                 2 * estimate / 3 ; 9 * estimate / 10 ] then
          estimate_steps g iteration temperature m evs
        else estimate - 1 in
      let progress =
        float_of_int iteration /. float_of_int estimate in
      let pause _ = pause progress in
      pause () ;%lwt
      if Utils.assert_defend then assert (evs = evaluate_state o (getv s)) ;
      let rec compute g (s, evs) = function
        | 0 -> Lwt.return []
        | i ->
          let%lwt (g, (s', evs'), m, may_be_removed) =
            wide_step pause g o temperature (s, evs) m in
          let ((s, evs), m, do_not_apply) =
            apply_elements_and_return_the_other g o (s, evs) m may_be_removed in
          let g =
            { g with pool_informations =
                       Pool.unregister_elements g.pool_informations do_not_apply ;
                     all_elements =
                       BidirectionalList.filter (fun e ->
                         not (PSet.mem e do_not_apply)) g.all_elements } in
          let%lwt l = compute g (s, evs) (i - 1) in
          Lwt.return ((g, (s', evs'), m) :: l) in
      let%lwt l = compute g (s, evs) (get_branch g 3 6) in
      match Utils.argmax (fun (_, (s1, evs1), _) (_, (s2, evs2), _) ->
              compare evs1 evs2) l with
      | Some (g, (s', evs'), m) ->
        let temperature = next_temperature_evs g temperature m evs evs' in
        aux (1 + iteration) estimate temperature g (s', evs') m
      | None ->
        aux (1 + iteration) estimate (next_temperature g temperature) g (s, evs) m) in
  let distance_to_objective =
    max 0 (int_of_float (sqrt (float_of_int (abs evs)))) in
  aux 0 0 (distance_to_objective / 3 + 1) g (s, evs) m

let solve_with_difference pause g s m o =
  let g = { g with all_elements =
    BidirectionalList.from_list (Utils.shuffle
      (BidirectionalList.to_list g.all_elements)) } in
  let s = Utils.cached s Element.empty_cache in
  let%lwt (g, (s, _), m) = wider_step pause g (s, evaluate_state o (getv s)) o m in
  Lwt.return (getv s)

let solve pause g o =
  let state = State.create_state (Array.length o) in
  solve_with_difference pause g state Element.empty_difference o

