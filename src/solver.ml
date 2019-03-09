
type global = {
    element_register : Element.t Utils.Id.map
      (** The set of all registered elements. **) ;
    constructor_informations : State.constructor_maps
      (** Informations about constructors. **) ;
    all_elements : Utils.Id.t Utils.BidirectionalList.t
      (** The list of all elements. **) ;
    pool_informations : Pool.global (** Informations needed by the pool. **)
  }

(** Reads the element from the register map. **)
let read_element g e =
  Utils.assert_option __LOC__ (Utils.Id.map_inverse g.element_register e)

let empty_global = {
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

(** Given an array of objectives [o], a state [s], and an element [e],
 * returns [None] if the element can’t be applied at all, and [Some None]
 * if it can’t be applied whilst increasing the evaluation of the state
 * relations.
 * If it can, it returns a triple with:
 * - the instantiation that makes it so,
 * - whether the element helps progressing with the attribute values,
 * - the change in the relation evaluation.
 * The argument [evs] must be [evaluate (State.get_relation_state s)]
 * (it is passed to avoid recomputing it each time). **)
let grade_evs o s evs e =
  Utils.apply_option (Element.search_instantiation s e) (fun (inst, progress) ->
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
 * Elements that can’t be applied at all are progressively removed from the pool.
 * It is parameterised by two numbers:
 * - [optimistic]: the function first extracts this number of elements
 *   from the pool, hoping that several will apply.
 *   If some apply, the best element is then applied.
 * - [greedy]: in the case where the optimistic approach did not work,
 *   the function extracts up to this number of new elements, halting
 *   on the first one that applies. **)
let step g o optimistic greedy s p =
  let evs = evaluate o (State.get_relation_state s) in
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
        match grade_evs o s evs e with
        | None -> aux default f p (n - 1)
        | Some o ->
          let p = Pool.add p ep in
          match o with
          | None -> aux default f p (n - 1)
          | Some (inst, progress, dev) ->
            f (fun _ -> aux default f p (n - 1)) p e inst progress dev in
  (** We first try with the optimistic pick. **)
  let (p, l) =
    aux [] (fun callback _ e inst progress dev ->
      let (p, l) = callback () in
      (p, (e, inst, (progress, dev)) :: l)) p optimistic in
  match Utils.argmax (fun (e1, inst1, g1) (e2, inst2, g2) ->
          compare_grade g1 g2) l with
  | Some (e, inst, g) -> (p, Some (Element.apply s e inst))
  | None ->
    (** We then move the the greedy pick. **)
    aux None (fun _ p e inst progress dev ->
      (p, Some (Element.apply s e inst))) p greedy

(** Calls the function [step] with heuristical arguments [optimistic] and [greedy]
 * depending on a single parameter [parameter] ranging from 0 to 101.
 * This parameter is meant to indicate the need to speed-up the analysis: at low
 * values, it means that adequate elements are being found, at high values, it
 * means that an exhaustive search is needed. **)
let weighted_step g o parameter s p =
  let (optimistic, greedy) =
    let parameter = max 0 parameter in
    if parameter > 100 then
      (0, Pool.quick_length p)
    else (6 - 3 * parameter / 100, 2 + 18 * parameter / 100) in
  step g o optimistic greedy s p

(** Picks [optimistic] random elements and apply the best.
 * It assumes that [g.all_elements] has been shuffled, so that random elements
 * are just the first ones.
 * Returns the new global [g] with its [all_elements] updated. **)
let add_random g o optimistic s =
  let evs = evaluate o (State.get_relation_state s) in
  let (g, l) =
    let rec aux g acc = function
      | 0 -> (g, acc)
      | n ->
        match Utils.BidirectionalList.match_left g.all_elements with
        | None -> (g, acc)
        | Some (e, all_elements) ->
          let g = { g with all_elements = all_elements } in
          let e = read_element g e in
          match grade_evs o s evs e with
          | None | Some None -> aux g acc (n - 1)
          | Some (Some (inst, progress, dev)) ->
            aux g ((e, inst, (progress, dev)) :: acc) (n - 1) in
    aux g [] optimistic in
  match Utils.argmax (fun (e1, inst1, g1) (e2, inst2, g2) ->
          compare_grade g1 g2) l with
  | Some (e, inst, gr) -> (g, Element.apply s e inst)
  | None -> (g, (s, PMap.empty))

let solve g s o =
  let g = { g with all_elements =
    Utils.BidirectionalList.from_list (Utils.shuffle
      (Utils.BidirectionalList.to_list g.all_elements)) } in
  let p = Pool.empty g.pool_informations in
  (* TODO: Adds in the pool all the relevant elements given the constraints
   * given by the user. *)
  let m = PMap.empty in (* TODO: the associated difference of attributes. *)
  let rec aux g s p m =
    Lwt_js.yield () ;%lwt
    let (p, so) = step g o 5 10 s p in
    match so with
    | None ->
      let (g, (s, m)) = add_random g o 3 s in
      Lwt.return s (* TODO: Call one self recursively… under some guard that
                    * prevents infinite recursion. *)
    | Some (s, m') ->
      let m = Element.merge_attribute_differences m m' in
      aux g s p m in
  aux g s p m

