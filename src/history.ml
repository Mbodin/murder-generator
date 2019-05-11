
type character = Id.t

type event = {
    event_begin : Date.t ;
    event_end : Date.t ;
    event : character Events.t
  }

let generate_event beg e =
  let en =
    match e.Events.event_type with
    | Events.For_life_event -> Date.add_years beg (Utils.rand 20 100)
    | Events.Long_term_event -> Date.add_years beg (Utils.rand 1 10)
    | Events.Medium_term_event -> Date.add_days beg (Utils.rand 14 100)
    | Events.Short_term_event -> Date.add_days beg (Utils.rand 2 10)
    | Events.Very_short_term_event -> Date.add_minutes beg (Utils.rand 2 300)
    | Events.Immediate_event -> beg in {
    event_begin = beg ;
    event_end = en ;
    event = e
  }

let generate_event_inv en e =
  let beg =
    match e.Events.event_type with
    | Events.For_life_event -> Date.add_years en (- Utils.rand 20 100)
    | Events.Long_term_event -> Date.add_years en (- Utils.rand 1 10)
    | Events.Medium_term_event -> Date.add_days en (- Utils.rand 14 100)
    | Events.Short_term_event -> Date.add_days en (- Utils.rand 2 10)
    | Events.Very_short_term_event -> Date.add_minutes en (- Utils.rand 2 300)
    | Events.Immediate_event -> en in {
    event_begin = beg ;
    event_end = en ;
    event = e
  }

let get_event_type beg en =
  let check f d r cont =
    let begd = f beg d in
    if Date.compare begd en <= 0 then r
    else cont () in
  check Date.add_years 20 Events.For_life_event (fun _ ->
    check Date.add_years 1 Events.Long_term_event (fun _ ->
      check Date.add_days 14 Events.Medium_term_event (fun _ ->
        check Date.add_days 2 Events.Short_term_event (fun _ ->
          check Date.add_minutes 2 Events.Very_short_term_event (fun _ ->
            Events.Immediate_event)))))

let compatible_events e1 e2 =
  e1.event.event_type <> e2.event.event_type
  || compare e1.event_begin e2.event_end = 1
  || compare e2.event_begin e1.event_end = 1


type t = {
    events : character Events.t Id.map
      (** Events are not stored directly in the timeline,
       * but through identifiers. **) ;
    kind_map : (character Events.kind * character, Id.t PSet.t) PMap.t
      (** For each kind and character, returns a set of event identifiers
       * satisfying them. **) ;
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
    constraints_none :
      (character, (character Events.kind, Id.t PSet.t * Id.t PSet.t) PMap.t) PMap.t
      (** For each event character and kind, provides two sets:
       * - the set for which no event of this kind can be before the given event,
       * - the set for which no event of this kind can be after the given event.
       * These sets are kept to a minimum: if an event is before another,
       * and that both prevents any event of a given kind to be placed after them,
       * then only the latter really have to prevent any such event to happen. **) ;
    constraints_some :
      (character, (character Events.kind, Id.t PSet.t * Id.t PSet.t) PMap.t) PMap.t
      (** Same as [constraints_none], but enforce that there is at least one event
       * of the given kind before/after them.
       * Elements of these sets are naturally removed when their constraints have
       * been met. **) ;
    already_declared_events : (Id.t * character) PSet.t
      (** A given event can’t be reused twice for the same character.
       * As a consequence, we store the event identifiers (from the field
       * [Events.event_id], not from the history identifier) for each character:
       * they are not allowed to appear twice for a given character. **)
  }

(** As the type is pure, one can safely return it. **)
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
  PSet.fold (fun c (before, after) ->
    let none =
      try PMap.find c st.constraints_none
      with Not_found -> PMap.empty in
    let k =
      try PMap.find c e.Events.event_kinds
      with Not_found -> PSet.empty in
    let (before, after) =
      PSet.fold (fun k (before, after) ->
        let (no_before, no_after) =
          try PMap.find k none
          with Not_found -> (PSet.empty, PSet.empty) in
        (PSet.merge no_after before,
         PSet.merge no_before after)) (before, after) k in
    let (before, after) =
      let (no_before, no_after) =
        try PMap.find c e.Events.constraints_none
        with Not_found -> (PSet.empty, PSet.empty) in
      let no_before =
        PSet.flat_map (fun k ->
          try PMap.find (k, c) st.kind_map
          with Not_found -> PSet.empty) no_before in
      let no_after =
        PSet.flat_map (fun k ->
          try PMap.find (k, c) st.kind_map
          with Not_found -> PSet.empty) no_after in
      (PSet.merge no_after before, PSet.merge no_before after) in
    (before, after)) (PSet.empty, PSet.empty) e.Events.event_attendees

(** Generalizes [get_constraints]: given a list of events [el],
 * returns a list a sets [before] and [after] corresponding to
 * the full constraints of the list (the order of the elements
 * of the lists being conserved).
 * The function [all_successors_set] has already been called on
 * these sets.
 * As a third element of each tuple, the corresponding event is
 * returned for convenience. **)
let get_full_constraints st el =
  (** The before list is parsed from left to right, but the after
   * list is parsed from right to left. **)
  let constraints = List.map (get_constraints st) el in
  let rec full f f' sacc acc = function
    | [] -> acc
    | s :: l ->
      let s = f s in
      let s = all_successors_set f' st s in
      let sacc = PSet.merge s sacc in
      full f f' sacc (sacc :: acc) l in
  let constraints_before =
    List.rev (full fst fst PSet.empty [] constraints) in
  let constraints_after =
    full snd snd PSet.empty [] (List.rev constraints) in
  let rec aux lb la el =
    match lb, la, el with
    | [], [], [] -> []
    | sb :: lb, sa :: la, e :: el ->
      (sb, sa, e) :: aux lb la el
    | _, _, _ -> assert false in
  aux constraints_before constraints_after el

let lcompatible_and_progress st el =
  if Utils.assert_defend then
    assert (Utils.is_uniq (List.map (fun e -> e.Events.event_id) el)) ;
  if List.exists (fun e ->
       PSet.fold (fun c b ->
           b || PSet.mem (e.Events.event_id, c) st.already_declared_events)
         false e.Events.event_attendees) el then
    (** An instantiated event can only be declared once. **)
    None
  else
    let rec aux r = function
      | [] -> Some r
      | (sb, sa, e) :: l ->
        if not (PSet.is_empty (PSet.inter sb sa)) then
          (** Adding this event would create a loop in the graph. **)
          None
        else
          (** There is no conflict for this event: we can check whether
           * it would make things progress. **)
          aux (r || PMap.foldi (fun c ks r ->
            r || let m =
                   try PMap.find c st.constraints_some
                   with Not_found -> PMap.empty in
                 PSet.fold (fun k r ->
                     r || let (before, after) =
                            try PMap.find k m
                            with Not_found -> (PSet.empty, PSet.empty) in
                          not (PSet.is_empty (PSet.inter before sb))
                          || not (PSet.is_empty (PSet.inter after sa)))
                   false ks) e.Events.event_kinds false) l in
    aux false (get_full_constraints st el)

let compatible_and_progress st e = lcompatible_and_progress st [e]

(** Returns a new state [st] where [e1] is assured to be before [e2]. **)
let make_before st e1 e2 =
  let graph = st.graph in
  let add l e = if List.mem e l then l else e :: l in
  let graph =
    let (before, after) =
      try PMap.find e1 graph
      with Not_found -> ([], []) in
    let after = add after e2 in
    PMap.add e1 (before, after) graph in
  let graph =
    let (before, after) =
      try PMap.find e2 graph
      with Not_found -> ([], []) in
    let before = add before e1 in
    PMap.add e2 (before, after) graph in
  { st with graph = graph }

let lapply st el =
  (* LATER: This function could be factorised. *)
  (** Registering the new events. **)
  let (events, idl) =
    List.fold_left (fun (events, idl) e ->
      let (id, events) = Id.map_insert_t events e in
      (events, (id, e) :: idl)) (st.events, []) el in
  let idl = List.rev idl in
  let st =
    { st with events = events ;
              graph =
                List.fold_left (fun graph (id, _) ->
                  if Utils.assert_defend then
                    assert (not (PMap.mem id graph)) ;
                  PMap.add id ([], []) graph) st.graph idl ;
              kind_map =
                List.fold_left (fun map (id, e) ->
                  PSet.fold (fun c map ->
                      let k =
                        try PMap.find c e.Events.event_kinds
                        with Not_found -> PSet.empty in
                      PSet.fold (fun k map ->
                        let ids =
                          try PMap.find (k, c) map
                          with Not_found -> PSet.empty in
                        PMap.add (k, c) (PSet.add id ids) map) map k)
                    map e.Events.event_attendees) st.kind_map idl ;
              already_declared_events =
                List.fold_left (fun set (_, e) ->
                    PSet.fold (fun c set -> PSet.add (e.Events.event_id, c) set)
                      set e.Events.event_attendees)
                  st.already_declared_events idl } in
  (** Registering the new constraints. **)
  let rec intermediate_constraints st = function
    | [] | _ :: [] -> st
    | (e1, _) :: (e2, ev2) :: l ->
      intermediate_constraints (make_before st e1 e2) ((e2, ev2) :: l) in
  let st = intermediate_constraints st idl in
  let constraints = List.map (get_constraints st) el in
  let st =
    List.fold_left2 (fun st (before, after) (e, _) ->
      let st = PSet.fold (fun e' st -> make_before st e' e) st before in
      let st = PSet.fold (fun e' st -> make_before st e e') st after in
      st) st constraints idl in
  (** Optimizing [st.constraints_none] by removing useless elements. **)
  let constraints_none =
    List.fold_left (fun constraints_none (id, e) ->
      let (eb, ea) =
        try PMap.find id st.graph
        with Not_found -> ([], []) in
      let (eb, ea) = (PSet.from_list eb, PSet.from_list ea) in
      PSet.fold (fun c constraints_none ->
          let (no_before, no_after) =
            try PMap.find c e.Events.constraints_none
            with Not_found -> (PSet.empty, PSet.empty) in
          let none =
            try PMap.find c constraints_none
            with Not_found -> PMap.empty in
          let none =
            PSet.fold (fun k none ->
              let (k_no_before, k_no_after) =
                try PMap.find k none
                with Not_found -> (PSet.empty, PSet.empty) in
              let k_no_before = PSet.diff k_no_before eb in
              let k_no_before =
                if PSet.is_empty (PSet.inter k_no_before ea) then
                  PSet.add id k_no_before
                else k_no_before in
              PMap.add k (k_no_before, k_no_after) none) none no_before in
          let none =
            PSet.fold (fun k none ->
              let (k_no_before, k_no_after) =
                try PMap.find k none
                with Not_found -> (PSet.empty, PSet.empty) in
              let k_no_after = PSet.diff k_no_after ea in
              let k_no_after =
                if PSet.is_empty (PSet.inter k_no_after eb) then
                  PSet.add id k_no_after
                else k_no_after in
              PMap.add k (k_no_before, k_no_after) none) none no_after in
          PMap.add c none constraints_none)
        st.constraints_none e.Events.event_attendees) st.constraints_none idl in
  let st = { st with constraints_none = constraints_none } in
  (** Trying to match as many requirements of [st.constraints_some] as possible. **)
  let st =
    let rec aux st added_before = function
      | [] -> st
      | (e, ev) :: l ->
        (* FIXME: Some links from an event to itself have been emitted, and I
         * suspect that this part of the code is responsible for this. *)
        let (before, after) = (all_successors fst st e, all_successors snd st e) in
        let (st, added_before) =
          PMap.foldi (fun c ks (st, added_before) ->
              let m =
                try PMap.find c st.constraints_some
                with Not_found -> PMap.empty in
              let (st, added_before, m) =
                PSet.fold (fun k (st, added_before, m) ->
                  let (yes_before, yes_after) =
                    try PMap.find k m
                    with Not_found -> (PSet.empty, PSet.empty) in
                  let do_before = PSet.diff yes_before before in
                  let do_before = PSet.remove e do_before in
                  let st =
                    PSet.fold (fun e' st -> make_before st e e') st do_before in
                  let do_after = PSet.diff yes_after after in
                  let do_after = PSet.remove e do_after in
                  let (st, added_before) =
                    PSet.fold (fun e' (st, added_before) ->
                        (make_before st e' e, all_successors fst st e'))
                      (st, added_before) do_after in
                  let m =
                    PMap.add k (PSet.diff yes_before do_before,
                                PSet.diff yes_after do_after) m in
                  (st, added_before, m)) (st, added_before, m) ks in
              let st =
                { st with constraints_some = PMap.add c m st.constraints_some } in
              (st, added_before)) ev.Events.event_kinds (st, added_before) in
        (* TODO: In addition to the already present constraints, one could
         * try to solve the [constraints_some] constraints of the event [e]. *)
        aux st added_before l in
    aux st PSet.empty idl in
  st

let apply st e = lapply st [e]

let create_state _ = {
    already_declared_events = PSet.empty ;
    events = Id.map_create () ;
    kind_map = PMap.empty ;
    graph = PMap.empty ;
    constraints_none = PMap.empty ;
    constraints_some = PMap.empty
  }

type final = event list

(** Returns the list of all players. **)
let all_players st =
  PMap.foldi (fun c _ l -> c :: l) st []

let finalise st now =
  (** We first consider all events with no successor. **)
  let start =
    PMap.foldi (fun e (before, after) l ->
      if after = [] then e :: l else l) st.graph [] in
  (** We then perform a topological sort of all events. **)
  let sorted =
    let rec aux acc seen next = function
      | [] ->
        if next = [] then List.rev acc
        else (
          let (ready, not_ready) =
            List.partition (fun e ->
              let (before, after) =
                try PMap.find e st.graph
                with Not_found -> ([], []) in
              List.for_all (fun e -> PSet.mem e seen) after) next in
          if ready = [] then
            failwith "Cyclic event dependency." ;
          aux acc seen not_ready ready
        )
      | e :: l ->
        if PSet.mem e seen then aux acc seen next l
        else
          let (before, after) =
            try PMap.find e st.graph
            with Not_found -> ([], []) in
          aux (e :: acc) (PSet.add e seen) (before @ next) l in
    aux [] PSet.empty [] start in
  let sorted = List.rev sorted in
  (** We then assign timetables to this list. **)
  let (l, _) =
    List.fold_left (fun (acc, state) e ->
      let ev = Utils.assert_option __LOC__ (Id.map_inverse st.events e) in
      let (before, after) =
        try PMap.find e st.graph
        with Not_found -> ([], []) in
      let t =
        Date.min
          (List.fold_left (fun t e ->
            let t' =
              try PMap.find e (fst state)
              with Not_found -> now in
            Date.min t t') now after)
          (PSet.fold (fun c ->
            let t =
              try PMap.find (ev.Events.event_type, c) (snd state)
              with Not_found -> now in
            Date.min t) now ev.Events.event_attendees) in
      let t = Date.add_minutes t (- Utils.rand 10 30) in
      let ev = generate_event_inv t ev in
      let state =
        (PMap.add e ev.event_begin (fst state),
         PSet.fold (fun c ->
             PMap.add (ev.event.Events.event_type, c) ev.event_begin)
           (snd state) ev.event.Events.event_attendees) in
      (ev :: acc, state)) ([], (PMap.empty, PMap.empty)) sorted in
  l

let unfinalise l =
  let state =
    let max_character =
      List.fold_left (fun c e ->
        let l =
          List.map Id.to_array (Events.get_attendees_list e.event) in
        List.fold_left max c l) (-1) l in
    create_state (1 + max_character) in
  let lb =
    List.sort (fun e1 e2 -> Date.compare e2.event_begin e1.event_begin) l in
  let le =
    List.sort (fun e1 e2 -> Date.compare e1.event_end e2.event_end) l in
  let (state, l) =
    let (events, graph, l) =
      List.fold_left (fun (m, g, l) ev ->
        if Id.get_id m ev.event <> None then
          failwith "Duplicated events" ;
        let (e, m) = Id.map_insert_t m ev.event in
        let g = PMap.add e ([], []) g in
        (m, g, (e, ev) :: l)) (state.events, state.graph, []) l in
    ({ state with events = events ;
                  graph = graph }, l) in
  let state =
    List.fold_left (fun state (e, ev) ->
      let rec auxb state = function
        | [] -> state
        | ev' :: l ->
          if Date.compare ev.event_end ev'.event_begin < 0 then (
            let e' =
              Utils.assert_option __LOC__ (Id.get_id state.events ev'.event) in
            let state = make_before state e e' in
            auxb state l
          ) else state in
      let state = auxb state lb in
      let rec auxe state = function
        | [] -> state
        | ev' :: l ->
          if Date.compare ev'.event_end ev.event_begin < 0 then (
            let e' =
              Utils.assert_option __LOC__ (Id.get_id state.events ev'.event) in
            let state = make_before state e' e in
            auxe state l
          ) else state in
      let state = auxe state le in
      state) state l in
  state

let fold_graph f acc st =
  PMap.foldi (fun id edges acc ->
    let ev =
      Utils.assert_option __LOC__ (Id.map_inverse st.events id) in
    f acc ev id edges) st.graph acc

