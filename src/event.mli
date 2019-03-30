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

(** Event kinds **)
type kind

(** Build a kind from a kind name. **)
val kind_of_string : string -> kind

(** Build a kind from an attribute (meaning that this event provides
 * such an attribute). **)
val kind_of_attribute : Attribute.attribute -> kind

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
       * simultaneously. **) ;
    event_kinds : kind PSet.t
      (** The set of event kinds that this event inherit from. **)
    (* TODO: Constraints. *)
    (* TODO: Translations. *)
  }

