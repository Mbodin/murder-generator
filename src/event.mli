(** Module Event
 * Describes an event as used by the History module. **)

type character = Id.t

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

(** The structure of an event.
 * This structure stores all the needed information to describe the event,
 * but lacks any mention of its date and dependencies.
 * See the History module for a richer type. **)
type t = {
    event_type : event_type
      (** Two events with the same event type can not happen simultaneously. **) ;
    event_attendees : character list
      (** List of characters fully involved during this event,
       * or that can not be involved during this event takes place.
       * Two events with non-disjunct character lists can not happen
       * simultaneously. **)
    (* TODO: Translations. *)
  }

