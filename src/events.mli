(** Module Events
 * Describes events as used by the History module. **)

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

(** A function converting kind to string, for debugging purposes. **)
val print_kind : ('character -> string) -> 'character kind -> string

(** Change each representation of characters in a kind. **)
val kind_convert : ('a -> 'b option) -> 'a kind -> 'b kind option

(** The translations associated to an event. **)
type 'character translation =
  (int * (** Number of sentence in the event. **)
   (int, 'character) Translation.st
   (** For each sentence number (counting from [0]), how to translate it. **))

(** The structure of an event.
 * This structure stores all the needed information to describe the event,
 * but lacks any mention of its date and dependencies.
 * See the History module for a richer type.
 * Events are parameterised by the kind of characters. **)
type 'character t = {
    event_id : Id.t
      (** A unique identifier for a particular event.
       * Note that this identifier is only used in this structure.
       * In particular, the event identifiers used in the History module
       * are not the same ones.
       * This number is conserved during instantiations: each event
       * instantiation thus shares the same identifier. **) ;
    event_phantom : bool
      (** Whether this element is a phantom event, that is an event with
       * no description, which is not displayed in the event list.
       * Such events are useful to create empty buffer areas
       * in the life of characters. **) ;
    event_blocking : bool
      (** Whether this element is blocking, that is, whether it prevents
       * other events from happening at the same time. **) ;
    event_type : event_type
      (** Two events with the same event type can’t simultaneously
       * involve the same player. **) ;
    event_attendees : 'character PSet.t
      (** Characters involved in this event. **) ;
    all_attendees : 'character list
      (** Only used when importing the event: the list of attendees declared
       * in the full event. **) ;
    event_kinds : ('character, 'character kind PSet.t) PMap.t
      (** The set of event kinds that this event inherits from, for each
       * player. **) ;
    constraints_none :
      ('character, 'character kind PSet.t * 'character kind PSet.t) PMap.t
      (** For each attendee of the event, provide two sets of kinds:
       * one for before and one for after this event.
       * None of these combinations of kinds and characters should appear
       * before (respectively after) this event. **) ;
    constraints_some :
      ('character, 'character kind PSet.t * 'character kind PSet.t) PMap.t
      (** Same as [constraints_none], but instead of requiring no such events,
       * it requires that at least one of this kind and character combination
       * appear before (respectively after) for each element of these sets. **) ;
    translation : 'character translation
      (** How this event can be worded. **)
  }

(** Comparing the whole structure [t] can take some time.
 * However, in practise, only a few fields are really needed to be compared
 * due to a lot of invariants in the program.
 * This function only compares the relevant fields. **)
val compare : 'a t -> 'a t -> int

(** Given an event, return its list of attendees. **)
val get_attendees_list : 'character t -> 'character list

(** Provides a recoverable identifier for an event. **)
val print_event : 'a t -> string

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
 * instantiation.
 * The translation part of this event is not to be used. **)
val partially_instantiate : 'a -> 'b -> 'a t -> 'b t option

