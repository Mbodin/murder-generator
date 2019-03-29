
type character = Id.t

type event = {
    event_begin : Date.t ;
    event_end : Date.t ;
    event : Event.t
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

type t = unit (* TODO *)

let compatible el e = true (* TODO *)

let copy = Utils.id (* TODO *)

let create_state n = () (* TODO *)

type final = t (* FIXME *)

let finalise = Utils.id (* TODO *)

