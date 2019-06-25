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

(** Same as [generate_event], but takes as argument the ending
 * date of an event. **)
val generate_event_inv : Date.t -> character Events.t -> event

(** Infer the kind of event from its starting and ending dates. **)
val get_event_type : Date.t -> Date.t -> Events.event_type

(** States whether two events are compatible, that is that they do not overlap,
 * or that they are of different types. **)
val compatible_events : event -> event -> bool

(** The global timeline, storing all events that happenned.
 * This typed is used when looking for solution to constraints
 * and only stores the relevant interactions between events. **)
type t

(** The final type, where each event have actually been assigned
 * a particular moment in type.
 * The events are guaranteed to be ordered along their start time. **)
type final = event list

(** Describes the status of elements with respect to their behaviours
 * with timelines.
 * Most elements are [Normal], but some have special behaviours. **)
type status =
  | Normal (** Any given player can only have this element once. **)
  | Duplicable (** This element can be applied without any restriction. **)
  | Unique (** This element can be applied at most once in the whole scenario. **)

(** States whether an event is compatible with a timeline.
 * Its return value is the same as [Element.compatible_and_progress]:
 * [None] if incompatible, [Some false] is compatible but does not help
 * with the already present constraints, and finally [Some true] means
 * that this event not only applies, but it helps the already present
 * events. **)
val compatible_and_progress : t -> character Events.t -> bool Lazy.t option

(** States whether a list of events is compatible with a timeline,
 * assuming that each event has to be in this particular order. **)
val lcompatible_and_progress : t -> character Events.t list -> bool Lazy.t option

(** Apply an event to a timeline.
 * This function should only be called on events for which
 * [compatible_and_progress] returns [Some]. **)
val apply : t -> status -> character Events.t -> t

(** Same as [apply], but for a list of events for which
 * [lcompatible_and_progress] returns [Some].
 * Their relative order will be conserved. **)
val lapply : t -> status -> character Events.t list -> t

(** (Deeply) copies the state. **)
val copy : t -> t

(** Creates an empty history state for the given number [n] of characters,
 * each indexed from [0] to [n - 1]. **)
val create_state : int -> t

(** Provide an actual instantiation of each event’s date and time.
 * It needs the date of the played scenario. **)
val finalise : t -> Date.t -> final

(** Inverse of [finalise].
 * There may be additional connections in the generated timeline
 * than in the original one.
 * This function only recovers what is possible to recover from
 * the type [final]. **)
val unfinalise : final -> t

(** Fold over the event graph.
 * At each step, we are given an event, its identifier, and two lists
 * of identifiers, corresponding to the events before and after the
 * current event. **)
val fold_graph : ('a -> character Events.t -> Id.t -> Id.t list * Id.t list -> 'a) -> 'a -> t -> 'a

(** Given a number of player, return the associated characters identifiers.
 * The existence of this function implies that character identifiers are
 * fixed accross all executions and really only depends on the total number
 * of players. **)
val all_players_length : int -> character list

