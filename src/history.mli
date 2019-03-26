(** Module History
 * Stores the history of a character. **)

type character = Id.t

(** A moment in time. **)
type date =
  int (** Year, with years before the common era being offset by one. **)
  * int (** Day of the year, counting from [0]. **)
  * int (** Minute of the day, counting from [0]. **)

(** Adds a given number of years to a date. **)
val add_years : date -> int -> date

(** Adds a given number of days to a date. **)
val add_days : date -> int -> date

(** Adds a given number of minutes to a date. **)
val add_minutes : date -> int -> date

(** The current date. **)
val now : date

(** The furthest possible date in the future. **)
val max_date : date

(** Compare dates. **)
val compare_date : date -> date -> int

(** Translate the date into an RFC 2445 date-time string. **)
val rfc2445 : date -> string

(** Translate the date into an org-mode (inactive) date. **)
val orgmode : date -> string

(** Translate the date into an org-mode (inactive) range. **)
val orgmode_range : date -> date -> string

(** Each event is associated an event type, which describes whether
 * two events can be at the same time: two events of the same type
 * can not be at the same time. **)
type event_type =
  | For_life_event (** An event that will continue, reaching the game itself. **)
  | Long_term_event (** Several years **)
  | Medium_term_event (** Several week **)
  | Short_term_event (** Several days **)
  | Very_short_term_event (** Several minutes **)
  | Immediate_event (** Less than a minute **)

(** An important event in the player life. **)
type event = {
    event_begin : date (** Beginning of the event **) ;
    event_end : date (** End of the event **) ;
    event_type : event_type
      (** Two events with the same event type can not happen simultaneously. **) ;
    event_attendees : character list
      (** List of characters fully involved during this event,
       * or that can not be involved during this event takes place.
       * Two events with non-disjunct character lists can not happen
       * simultaneously. **)
    (* TODO: Translations. *)
  }

(** A smart constructor for events. **)
val generate_event : date (** Beginning **) -> event_type (** Duration **) -> character list -> event

(** States whether two events are compatible, that is that they do not overlap,
 * or that they are of different types. **)
val compatible_events : event -> event -> bool

(** The history of a character is a list of events that created
 * the current mind state of the character. **)
type t = event list

(** States whether an event is compatible with an history. **)
val compatible : t -> event -> bool

(** The global state, describing all characters. **)
type state = t array

(** (Deeply) copies the state. **)
val copy : state -> state

(** Creates an empty history state for the given number n of characters,
 * each indexed from 0 to n - 1. **)
val create_state : int -> state

