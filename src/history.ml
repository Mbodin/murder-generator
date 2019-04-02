
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
    events : event Id.map
      (** Events are not stored directly in the timeline,
       * but through identifiers. **) ;
    kind_map : (character Event.kind * character, Id.t list) PMap.t
      (** For each kind and character, returns a list of event identifier
       * satisfying them. **) ;
    graph : (Id.t, Id.t list * Id.t list) PMap.t
      (** The graph of event constraints.
       * Each event is associated two sets of events:
       * - the events that have to happen before this particular event,
       * - the events that have to happen after this particular event.
       * This graph is guaranteed to be without cycle.
       * The transitive closure is not stored, though: to get the set of all
       * event after a particular one, one has to recursively explore the
       * graph. **) ;
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

(** Given a function [f] being either [fst] or [snd], get the set of all
 * successors/predecessors in this direction. **)
let rec all_successors f st e =
  let rec aux visited = function
    | [] -> visited
    | e :: l ->
      if PSet.mem e visited then
        aux visited l
      else
        let next =
          try PMap.find e st.graph
          with Not_found -> assert false in
        aux (PSet.add e visited) (f next @ l) in
  aux PSet.empty [e]

let lcompatible_and_progress st el = Some false (* TODO *)

let compatible_and_progress st e = lcompatible_and_progress st [e]

let lapply st el = st (* TODO *)

let apply st e = lapply st [e]

let create_state _ = {
    events = Id.map_create () ;
    kind_map = PMap.empty ;
    graph = PMap.empty ;
    constraint_none = PMap.empty ;
    constraint_some = PMap.empty
  }

type final = event list

let finalise st = [] (* TODO *)

