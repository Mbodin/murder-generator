(** Module History
 * Stores the timeline of characters. **)

type character = Event.character

(** This type enriches [Event.t] with a date and duration. **)
type event = {
    event_begin : Date.t (** Beginning of the event **) ;
    event_end : Date.t (** End of the event **) ;
    event : Event.t (** The actual event **)
  }

(** A smart constructor for events.
 * It takes its starting date and an event and associates it with
 * an actual duration. **)
val generate_event : Date.t -> Event.t -> event

(** States whether two events are compatible, that is that they do not overlap,
 * or that they are of different types. **)
val compatible_events : event -> event -> bool

(** The global timeline, storing all events that happenned.
 * This typed is used when looking for solution to constraints
 * and only stores the relevant interactions between events. **)
type t

(** The final type, where each event have actually been assigned
 * a particular moment in type. **)
type final

(** States whether an event is compatible with a timeline. **)
val compatible : t -> event -> bool

(** (Deeply) copies the state. **)
val copy : t -> t

(** Creates an empty history state for the given number n of characters,
 * each indexed from 0 to n - 1. **)
val create_state : int -> t

(** Provide an actual instantiation of each event’s date and time. **)
val finalise : t -> final

