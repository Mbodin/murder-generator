
type character = Id.t

type event = {
    event_begin : Date.t ;
    event_end : Date.t ;
    event : character Event.t
  }

let generate_event beg e =
  let en =
    match e.Event.event_type with
    | Event.For_life_event -> Date.add_years beg (Utils.rand 20 100)
    | Event.Long_term_event -> Date.add_years beg (Utils.rand 1 10)
    | Event.Medium_term_event -> Date.add_days beg (Utils.rand 14 100)
    | Event.Short_term_event -> Date.add_days beg (Utils.rand 2 10)
    | Event.Very_short_term_event -> Date.add_minutes beg (Utils.rand 2 300)
    | Event.Immediate_event -> beg
  in {
    event_begin = beg ;
    event_end = en ;
    event = e
  }

let compatible_events e1 e2 =
  e1.event.event_type <> e2.event.event_type
  || compare e1.event_begin e2.event_end = 1
  || compare e2.event_begin e1.event_end = 1


type t = {
    events : character Event.t Id.map
      (** Events are not stored directly in the timeline,
       * but through identifiers. **) ;
    kind_map : (character Event.kind * character, Id.t list) PMap.t
      (** For each kind and character, returns a list of event identifier
       * satisfying them. **) ; (* FIXME: Do I really need this? *)
    graph : (Id.t, Id.t list * Id.t list) PMap.t
      (** The graph of event constraints.
       * Each event is associated two sets of events:
       * - the events that have to happen before this particular event,
       * - the events that have to happen after this particular event.
       * This graph is guaranteed to be without cycle.
       * The transitive closure is not stored, though: to get the set of all
       * event after a particular one, one has to recursively explore the
       * graph.
       * This graph is guaranteed to be symmetrical: if [e] must be after [e'],
       * then [e'] must be before [e]. **) ;
    constraint_none :
      (character, (character Event.kind, Id.t PSet.t * Id.t PSet.t) PMap.t) PMap.t
      (** For each event character and kind, provides two sets:
       * - the set for which no event of this kind can be before the given event,
       * - the set for which no event of this kind can be after the given event.
       * These sets are kept to a minimum: if an event is before another,
       * and that both prevents any event of a given kind to be placed after them,
       * then only the latter really have to prevent any such event to
       * happen. **) ;
    constraint_some :
      (character, (character Event.kind, Id.t PSet.t * Id.t PSet.t) PMap.t) PMap.t
      (** Same as [constraint_none], but enforce that there is at least one event
       * of the given kind before/after them.
       * Elements of these sets are naturally removed when their constraints have
       * been met. **)
  }

(** As the type is pure, one can safely returns it. **)
let copy = Utils.id

(** Given a function [dir] being either [fst] or [snd], get the set of all
 * successors/predecessors in this direction. **)
let rec all_successors dir st e =
  let rec aux visited = function
    | [] -> visited
    | e :: l ->
      if PSet.mem e visited then
        aux visited l
      else
        let next =
          try PMap.find e st.graph
          with Not_found -> assert false in
        aux (PSet.add e visited) (dir next @ l) in
  aux PSet.empty [e]

(** Same than [all_successors], but where given a set instead of a single
 * element. **)
let all_successors_set dir st =
  PSet.fold (fun e -> PSet.merge (all_successors dir st e)) PSet.empty

(** Given a graph and an event, returns two sets of events identifiers
 * corresponding to the events that must be before and after this event.
 * This function should only consider the immediate ones: the function
 * [all_successors_set] will be called afterwards. **)
let get_constraints st e =
  (PSet.empty, PSet.empty) (* TODO *)

let lcompatible_and_progress st el =
  (** We first build up constraints, expressed as couples of sets
   * containing identifiers for events that must be before or after
   * the considered event. **)
  let constraints = List.map (get_constraints st) el in
  (** We then check that these constraints are satisfiable. **)
  let rec ok before after = function
    | [] -> true
    | (sb, sa) :: l ->
      PSet.is_empty (PSet.inter sb after)
      && PSet.is_empty (PSet.inter sa before)
      && let sb = all_successors_set fst st sb in
         let sa = all_successors_set snd st sa in
         PSet.is_empty (PSet.inter sb sa)
         && ok (PSet.merge sb before) (PSet.merge sa after) l in
  if not (ok PSet.empty PSet.empty constraints) then
    None
  else (* TODO: Check [st.constraint_none] *)
    Some (
      false (* TODO: Check whether this actually helps any of the
             * [st.constraint_some] constraints. *)
    )

let compatible_and_progress st e = lcompatible_and_progress st [e]

(** Returns a new state [st] where [e1] is assured to be before [e2]. **)
let make_before st e1 e2 =
  let graph = st.graph in
  let graph =
    let (before, after) =
      try PMap.find e1 graph
      with Not_found -> ([], []) in
    let after = if List.mem e1 after then after else e1 :: after in
    PMap.add e1 (before, after) graph in
  let graph =
    let (before, after) =
      try PMap.find e2 graph
      with Not_found -> ([], []) in
    let before = if List.mem e2 before then before else e2 :: before in
    PMap.add e2 (before, after) graph in
  { st with graph = graph }

let lapply st el =
  let (events, idl) =
    List.fold_left (fun (events, idl) e ->
      let (id, events) = Id.map_insert_t events e in
      (events, id :: idl)) (st.events, []) el in
  let idl = List.rev idl in
  let st =
    { st with events = events ;
              graph =
                List.fold_left (fun graph id ->
                  PMap.add id ([], []) graph) st.graph idl } in
  let rec intermediate_constraints st = function
    | [] | _ :: [] -> st
    | e1 :: e2 :: l -> intermediate_constraints (make_before st e1 e2) (e2 :: l) in
  let st = intermediate_constraints st idl in
  let constraints = List.map (get_constraints st) el in
  let st =
    List.fold_left2 (fun st (before, after) e ->
      let st = PSet.fold (fun e' st -> make_before st e' e) st before in
      let st = PSet.fold (fun e' st -> make_before st e e') st before in
      st) st constraints idl in
  st (* TODO: Update [constraint_none] and [constraint_some]. *)

let apply st e = lapply st [e]

let create_state _ = {
    events = Id.map_create () ;
    kind_map = PMap.empty ;
    graph = PMap.empty ;
    constraint_none = PMap.empty ;
    constraint_some = PMap.empty
  }

type final = event list

let finalise st =
  (** We first consider all events with no predecessor. **)
  let start =
    PMap.foldi (fun e (before, after) l ->
      if before = [] then e :: l else l) st.graph [] in
  (** We then perform a topological sort of all events. **)
  let sorted =
    let rec aux acc seen next = function
      | [] ->
        if next = [] then List.rev acc
        else aux acc seen [] next
      | e :: l ->
        if PSet.mem e seen then aux acc seen next l
        else
          let (before, after) =
            try PMap.find e st.graph
            with Not_found -> ([], []) in
          aux (e :: acc) (PSet.add e seen) (after @ next) l in
    aux [] PSet.empty [] start in
  (** We then assign timetables to this list. **)
  (* TODO: we could compress dates. *)
  let (l, _) =
    List.fold_left (fun (acc, t) e ->
      let e = Utils.assert_option __LOC__ (Id.map_inverse st.events e) in
      let t = Date.add_minutes t (Utils.rand 0 5) in
      let e = generate_event t e in
      (e :: acc, e.event_end)) ([], Date.now) sorted in
  List.rev l

