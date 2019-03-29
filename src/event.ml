
type character = Id.t

type event_type =
    | For_life_event
    | Long_term_event
    | Medium_term_event
    | Short_term_event
    | Very_short_term_event
    | Immediate_event

type t = {
    event_type : event_type ;
    event_attendees : character list
  }

