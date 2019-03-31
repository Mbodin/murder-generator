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
      (** The set of event kinds that this event inherit from. **) ;
    constraints_none : (kind * character) PSet.t * (kind * character) PSet.t
      (** Provides two sets of kinds and characters: one for before and one
       * for after this event.
       * None of these combinations of kinds and characters should appear
       * after or before this event. **) ;
    constraints_some : (kind * character) PSet.t * (kind * character) PSet.t
      (** Same as [constraints_none], but instead of requiring no such events,
       * it requires that at least one of this kind and character combination
       * appear before or after for each element of these sets. **)
    (* TODO: Translations. *)
  }

(** The type [t] has already been instantiated for its attendees,
 * but events are not parsed that way: one only knows which attendee
 * corresponds to which character once an instantiation has been chosen
 * by [Element.search_instantiation].
 * This type corresponds to an event that has not yet been instantiated. **)
type partial = {
    partial_type : event_type
      (** The type of the partial event. **) ;
    partial_kinds : kind PSet.t
      (** The kinds of the partial event. **) ;
    partial_attendees : int PSet.t
      (** The set of character that attend this event.
       * Characters are here expressed as integers and correspond
       * to the same integers as in [Element.Contact]. **) ;
    constraints_none : (int, kind PSet.t * kind PSet.t) PMap.t
      (** For each character, expressed as an integer, provide a set of
       * kinds that can’t appen before and after, respectively. **) ;
    constraints_some : (int, kind PSet.t * kind PSet.t) PMap.t
      (** Similarly, sets of kinds that must appear at least once before
       * and after for each character. **) ;
    (* TODO: Translations. *)
  }

(** Given an instantiation, translate the partial event to a fully instantiated
 * event. **)
val instantiate : character array -> partial -> t

(** This is kind of a silly function, but we need it in the Element module
 * to quickly evaluate whether a given character can be instantiated as a
 * particular character in an element in general (and in particuar, in a
 * particular event).
 * To do this, we first have to evaluate which character may be instantiated
 * in each slot, only considering its particular constraints before
 * considering the global ones.
 * This enables to filter out in a first pass characters that obviously
 * can’t be instantiated by a particular event before considering each
 * possibilities in an exponential way.
 * This function projects a partial event to an event… pretending that it
 * only features one character.
 * This event is such that if the full event applies, then the projection
 * also applies (but the projection might apply even if the full event does
 * not).
 * The provided integer is the local integer representing the character to
 * be projected, and the provided character is its instantiation.
 * This function returns [None] if this character does not attend this event. **)
val partially_instantiate : int -> character -> partial -> t option

