
type global = {
    branch_exploration : float ;
    element_register : Element.t Utils.Id.map
      (** The set of all registered elements. **) ;
    constructor_informations : State.constructor_maps
      (** Informations about constructors. **) ;
    pool_informations : Pool.global (** Informations needed by the pool. **) ;
    all_elements : Utils.Id.t Utils.BidirectionalList.t
      (** The list of all elements. **)
  }

(** Reads the element from the register map. **)
let read_element g e =
  Utils.assert_option __LOC__ (Utils.Id.map_inverse g.element_register e)

let empty_global p = {
    branch_exploration = p *. p ;
    element_register = Utils.Id.map_create () ;
    constructor_informations = State.empty_constructor_maps ;
    all_elements = Utils.BidirectionalList.empty ;
    pool_informations = Pool.empty_global
  }

let register_element g e =
  let (eid, m) = Utils.Id.map_insert_t g.element_register e in
  let attrs = Element.provided_attributes e in
  { g with element_register = m ;
           all_elements = Utils.BidirectionalList.add_left eid g.all_elements ;
           pool_informations = Pool.register_element g.pool_informations eid attrs }

let filter_elements g f =
  let f e = f (read_element g e) in
  { g with all_elements = Utils.BidirectionalList.filter f g.all_elements ;
           pool_informations =
             Pool.filter_global g.pool_informations f }

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

let evaluate_state o s =
  evaluate o (State.get_relation_state s)

(** Given an array of objectives [o], a state [s], and an element [e],
 * returns [None] if the element can’t be applied at all.
 * If it can, it returns a triple with:
 * - the instantiation that makes it so,
 * - whether the element helps progressing with the attribute values,
 * - the new relation evaluation. **)
let grade o s e =
  Utils.apply_option (Element.search_instantiation s e) (fun (inst, progress) ->
    let new_relations =
      Element.apply_relations (State.get_relation_state s) e inst in
    let ev = evaluate o new_relations in
    if Utils.assert_defend then (
      let (s', m) = Element.safe_apply s e inst in
      assert (ev = evaluate_state o s')) ;
    (inst, progress, ev))

(** Takes a “grade” made of the progress of an element and the evaluation
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

(** Returns a number between [mi] and [ma] depending on the parameter
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
 *   on the first one that applies. **)
let step g o optimistic greedy (s, evs) p =
  if Utils.assert_defend then assert (evs = evaluate_state o s) ;
  (** Extracts [n] elements from the pool, filtering out the ones that don’t
   * apply, and calling [f] on the ones that does whilst increasing the
   * evaluation.
   * The call to [f] is made with a callback to the recursive call of [aux]
   * if needed. **)
  let rec aux default f p = function
    | 0 -> (p, default)
    | n ->
      let (ep, p) = Pool.pop p in
      match ep with
      | None -> (p, default)
      | Some ep ->
        let e = read_element g ep in
        match grade o s e with
        | None -> aux default f p (n - 1)
        | Some (inst, progress, ev) ->
          let p = Pool.add p ep in
          if ev < evs then
            aux default f p (n - 1)
          else
            f (fun _ -> aux default f p (n - 1)) p e inst progress ev in
  (** We first try with the optimistic pick. **)
  let (p, l) =
    aux [] (fun callback _ e inst progress ev ->
      let (p, l) = callback () in
      (p, (e, inst, (progress, ev)) :: l)) p optimistic in
  match Utils.argmax (fun (e1, inst1, g1) (e2, inst2, g2) ->
          compare_grade evs g1 g2) l with
  | Some (e, inst, (progress, ev)) ->
    (p, Some (Element.safe_apply s e inst, ev))
  | None ->
    (** We then move the the greedy pick. **)
    aux None (fun _ p e inst progress ev ->
      (p, Some (Element.safe_apply s e inst, ev))) p greedy

(** Calls the function [step] with heuristical arguments [optimistic] and [greedy]
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

(** Picks [optimistic] random elements and apply the best.
 * It assumes that [g.all_elements] has been shuffled, so that random elements
 * are just the first ones.
 * Returns the new global [g] with its [all_elements] updated. **)
let add_random g o optimistic (s, evs) =
  if Utils.assert_defend then assert (evs = evaluate_state o s) ;
  let (g, l) =
    let rec aux g acc = function
      | 0 -> (g, acc)
      | n ->
        match Utils.BidirectionalList.match_left g.all_elements with
        | None -> (g, acc)
        | Some (e, all_elements) ->
          let g =
            { g with all_elements =
                       Utils.BidirectionalList.add_right all_elements e } in
          let e = read_element g e in
          match grade o s e with
          | None -> aux g acc (n - 1)
          | Some (inst, progress, ev) ->
            aux g ((e, inst, (progress, ev)) :: acc) (n - 1) in
    aux g [] optimistic in
  match Utils.argmax (fun (e1, inst1, g1) (e2, inst2, g2) ->
          compare_grade evs g1 g2) l with
  | Some (e, inst, (progress, ev)) -> (g, Element.safe_apply s e inst, ev)
  | None -> (g, (s, Element.empty_difference), evs)

(** Calls the functions [weighted_step] and [add_random], trying
 * to reach a step [s] whose associated attribute difference [m]
 * has been increased by [temperature]. **)
let wide_step g o temperature (s, evs) m =
  let initial_weigth = Element.difference_weigth m in
  let objective_weigth =
    initial_weigth + 5 * int_of_float (log (float_of_int temperature)) + 2 in
  let step g (s, evs) m parameter =
    let l = Element.difference_attribute_in_need m in
    if l = [] || Random.int 20 = 0 then (
      let optimistic = 7 - 6 * parameter / 100 in
      let (g, (s, m'), evs) = add_random g o optimistic (s, evs) in
      Lwt.return (g, (s, evs), Element.merge_attribute_differences m m')
    ) else (
      (** We select an attribute and try to increase it.
       * If we fail up or get far from the objective a given number of times,
       * we abort (then choosing another attribute). **)
      let a = Utils.select_any (Element.difference_attribute_in_need m) in
      let p = Pool.empty g.pool_informations in
      let p = Pool.add_attribute p a in
      let rec aux p m (s, evs) = function
        | 0 -> Lwt.return (g, (s, evs), m)
        | n ->
          InOut.pause () ;%lwt
          let (p, r) = weighted_step g o (110 * parameter / 100) (s, evs) p in
          match r with
          | None -> aux p m (s, evs) (n - 1)
          | Some ((s', m'), evs') ->
            let m = Element.merge_attribute_differences m m' in
            let n' =
              if Element.difference_weigth m' > 0
                 || evs' > evs then n else n - 1 in
            aux p m (s', evs') n'
      in aux p m (s, evs) (get_branch g 3 5)) in
  let rec aux g (s, evs) m = function
    | 0 -> Lwt.return (g, (s, evs), m)
    | n ->
      InOut.pause () ;%lwt
      let current_weigth = Element.difference_weigth m in
      if current_weigth >= objective_weigth then
        Lwt.return (g, (s, evs), m)
      else
        let parameter =
          100 * (current_weigth - initial_weigth)
          / (objective_weigth - initial_weigth) in
        let%lwt (g, (s, evs), m) = step g (s, evs) m parameter in
        aux g (s, evs) m (n - 1) in
  aux g (s, evs) m (get_branch g 4 10)

(** This function performs a simulted annealing based on [wide_step].
 * This last function is considered costly and we thus starts using the [Lwt.t]
 * type here. **)
let wider_step g (s, evs) o m =
  let rec aux temperature g (s, evs) m =
    if temperature <= 0 then
      Lwt.return (g, (s, evs), m)
    else (
      InOut.pause () ;%lwt
      if Utils.assert_defend then assert (evs = evaluate_state o s) ;
      let%lwt l =
        Lwt_list.map_s (fun _ -> wide_step g o temperature (s, evs) m)
          (Utils.seq (get_branch g 3 6)) in
      match Utils.argmax (fun (_, (s1, evs1), _) (_, (s2, evs2), _) ->
              compare evs1 evs2) l with
      | Some (g, (s', evs'), m) ->
        let temperature =
          if Element.difference_weigth m > 0 || evs' > evs then
            if temperature <= 2 then
              temperature
            else get_branch g 97 99 * temperature / 100 - 1
          else get_branch g 8 9 * temperature / 10 - 1 in
        aux temperature g (s', evs') m
      | None -> aux (get_branch g 6 8 * temperature / 10 - 1) g (s, evs) m) in
  let distance_to_objective =
    max 0 (int_of_float (sqrt (float_of_int (abs evs)))) in
  aux (distance_to_objective / 3 + 1) g (s, evs) m

let solve g s o =
  let g = { g with all_elements =
    Utils.BidirectionalList.from_list (Utils.shuffle
      (Utils.BidirectionalList.to_list g.all_elements)) } in
  let m = Element.empty_difference in
  (* TODO: Change the value of [m] to consider all the relevant elements given by
   * the constraints provided by the user. *)
  let%lwt (g, (s, _), m) = wider_step g (s, evaluate_state o s) o m in
  Lwt.return s

