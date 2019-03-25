(** Module History
 * Stores the history of a character. **)

type character = Id.t

(** The result of an event. **)
type result =
  | Relation_event (** An event that changed the way a character relates
                    * with another one. **)
    of character (** The character in question **)
       * Relation.t (** How the character now perceive this new relation.
                     * Note that in case of a compound relation, this
                     * relation is from the point of view of the player:
                     * asymmetrical relations should probably never
                     * appear here. **)

(** A moment in time. **)
type date =
  int (** Years, from the present (negative for past events) **)
  * int (** Day of the year **)
  * int (** Minute of the day **)

(** Adds a given number of years to a date. **)
val add_years : date -> int -> date

(** Adds a given number of days to a date. **)
val add_days : date -> int -> date

(** Adds a given number of minutes to a date. **)
val add_minutes : date -> int -> date

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
type event =
  date (** Beginning of the event **)
  * date (** End of the event **)
  * result (** Result of the event in the character’s vision of the world. **)
  * event_type (** Two events with the same event type can not happen simultaneously. **)
  * character list (** List of characters fully involved during this event,
                    * or that can not be involved during this event takes place.
                    * Two events with non-disjunct character lists can not happen
                    * simultaneously. **)

(** A smart constructor for events. **)
val generate_event : date (** Beginning **) -> event_type (** Duration **) -> result -> character list -> event

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

