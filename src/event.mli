(** Module Event
 * Describes an event as used by the History module. **)

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

(** The list of all event type. **)
val all_event_type : event_type list

(** Event kinds.
 * They can store characters and are parameterised that way. **)
type 'character kind

(** Build a kind from a kind identifier. **)
val kind_of_id : Id.t -> 'character kind

(** Build a kind from an attribute (meaning that this event provides
 * such an attribute). **)
val kind_of_attribute : Attribute.PlayerAttribute.attribute -> 'character kind

(** Similar than [kind_of_attribute], but for contacts.
 * The target characters is taken as argument. **)
val kind_of_contact : Attribute.ContactAttribute.attribute -> 'character -> 'character kind

(** Change each representation of characters in a kind. **)
val kind_convert : ('a -> 'b option) -> 'a kind -> 'b kind option

(** The structure of an event.
 * This structure stores all the needed information to describe the event,
 * but lacks any mention of its date and dependencies.
 * See the History module for a richer type.
 * Events are parameterised by the kind of characters. **)
type 'character t = {
    event_type : event_type
      (** Two events with the same event type can not happen simultaneously. **) ;
    event_attendees : 'character PSet.t
      (** List of characters fully involved during this event,
       * or that can not be involved during this event takes place.
       * Two events with non-disjunct character lists can not happen
       * simultaneously. **) ;
    event_kinds : ('character, 'character kind PSet.t) PMap.t
      (** The set of event kinds that this event inherit from, for each player. **) ;
    constraints_none :
      ('character, 'character kind PSet.t * 'character kind PSet.t) PMap.t
      (** For each character, provides two sets of kinds:
       * one for before and one for after this event.
       * None of these combinations of kinds and characters should appear
       * after or before this event. **) ;
    constraints_some :
      ('character, 'character kind PSet.t * 'character kind PSet.t) PMap.t
      (** Same as [constraints_none], but instead of requiring no such events,
       * it requires that at least one of this kind and character combination
       * appear before or after for each element of these sets. **)
    (* TODO: Translations. *)
  }

(** Given an instantiation of characters, instantiate an event. **)
val instantiate : ('a -> 'b option) -> 'a t -> 'b t option

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
 * The first argument is the local name of the character and the second its
 * instantiation. **)
val partially_instantiate : 'a -> 'b -> 'a t -> 'b t option

