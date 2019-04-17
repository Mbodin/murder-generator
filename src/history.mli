(** Module History
 * Stores the timeline of characters. **)

type character = Id.t

(** This type enriches [Events.t] with a date and duration. **)
type event = {
    event_begin : Date.t (** Beginning of the event **) ;
    event_end : Date.t (** End of the event **) ;
    event : character Events.t (** The actual event **)
  }

(** A smart constructor for events.
 * It takes its starting date and an event and associates it with
 * an actual duration. **)
val generate_event : Date.t -> character Events.t -> event

(** States whether two events are compatible, that is that they do not overlap,
 * or that they are of different types. **)
val compatible_events : event -> event -> bool

(** The global timeline, storing all events that happenned.
 * This typed is used when looking for solution to constraints
 * and only stores the relevant interactions between events. **)
type t

(** The final type, where each event have actually been assigned
 * a particular moment in type. **)
type final = event list

(** States whether an event is compatible with a timeline.
 * Its return value is the same as [Element.compatible_and_progress]:
 * [None] if incompatible, [Some false] is compatible but does not help
 * with the already present constraints, and finally [Some true] means
 * that this event not only applies, but it helps the already present
 * events. **)
val compatible_and_progress : t -> character Events.t -> bool option

(** States whether a list of events is compatible with a timeline,
 * assuming that each event has to be in this particular order. **)
val lcompatible_and_progress : t -> character Events.t list -> bool option

(** Apply an event to a timeline.
 * This function should only be called on events for which
 * [compatible_and_progress] returns [Some]. **)
val apply : t -> character Events.t -> t

(** Same as [apply], but for a list of events for which
 * [lcompatible_and_progress] returns [Some].
 * Their relative order will be conserved. **)
val lapply : t -> character Events.t list -> t

(** (Deeply) copies the state. **)
val copy : t -> t

(** Creates an empty history state for the given number n of characters,
 * each indexed from 0 to n - 1. **)
val create_state : int -> t

(** Provide an actual instantiation of each event’s date and time. **)
val finalise : t -> final

