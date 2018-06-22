(** Module State
 * Describes the state of the solver. **)

type character = History.character

(** A mapping from pairs of characters to Relation.t. **)
type relation_state

(** Returns the relation between the two characters.
 * The relations are usually symmetrical, but note how the asymmetrical
 * relation between c1 and c2 is represented by Asymmetrical (r1, r2)
 * where r1 is the relation from the point of view of c1.
 * If a mapping is not present, it returns the default relation
 * Relations.Basic Relations.Neutral. *)
val get_relation_state : relation_state -> character -> character -> Relations.t

(** Non-functionally update the relation state. **)
val write_relation_state : relation_state -> character -> character -> Relations.t -> unit

(** The following exception is returned if one tries to write or read
 * a relation between two identical characters. **)
exception SelfRelation

(** Creates an empty relation state for the given number n of characters,
 * each indexed from 0 to n - 1. **)
val create_relation_state : int -> relation_state

(** The state of attributes and contact of each character. **)
type character_state

(** The type of attributes. **)
type attribute

(** The type of values. **)
type value

(** Values are just constructor identifiers.
 * Each attribute is associated with a given set of possible constructors
 * (which are really just names).
 * The following table keeps track of the constructor names. **)
type constructor_map

(** An empty constructor map. **)
val empty_constructor_map : constructor_map

(** Returns the name of an attribute. **)
val attribute_name : constructor_map -> attribute -> string option

(** Returns the name of a constructor. **)
val value_name : constructor_map -> value -> string option

(** Returns the list of constructors associated with an attribute. **)
val constructors : constructor_map -> attribute -> value list option

(** Declare an attribute, returning its associated normal identifier
 * (if already declared, its normal identifier is still returned). **)
val declare_attribute : constructor_map -> string -> attribute * constructor_map

(** Declare a new constructor for an attribute. **)
val declare_constructor : constructor_map -> attribute -> string -> value * constructor_map

(** Users can remove categories before the story generation.
 * This function removes a constructor, probably because it was associated an unwanted category. **)
val remove_constructor : constructor_map -> attribute -> value -> constructor_map


(** Waiting for a value to be decided for a given attribute,
 * the following type is sed instead. **)
type attribute_value =
  | Fixed_value of value (** The value has already been fixed.  It can not be changed back. **)
  | One_value_of of value list (** The value has not been yet fixed, but it is known to be one of these. **)
(** Note that a [One_value_of] associated with a singleton list is not equivalent
 * to a [Fixed_value]: the latter has been approved by a story element (possibly
 * associating it with an event), whilst the former hasn't. **)
(** Furthermore, note that [One_value_of] associated with an empty list is possible.
 * The underlying meaning is that this attribute must not be used. **)

(** Contacts between characters. **)
type contact

(** values of contact. **)
type contact_value

(** Creates an empty character state for the given number n of characters,
 * each indexed from 0 to n - 1. **)
val create_character_state : int -> character_state

(** Get a character attribute from the character state. **)
val get_attribute_character : character_state -> character -> attribute -> attribute_value option

(** Get a character attribute from the character state.
 * If it is not present, the character set is non-functionally updated
 * to mark the attribute as being of need of a value (returning all the
 * constructors of its type). **)
val force_get_attribute_character : constructor_map -> character_state -> character -> attribute -> attribute_value

(** Non-functionally associates the given attribute of the character to the given value. **)
val write_attribute_character : character_state -> character -> attribute -> attribute_value -> unit

(** Get a contact from the character state. **) (* TODO: We can have more than one contact of the same name. *)
val get_contact_character : character_state -> character -> contact -> (character * contact_value) option

type t

val get_relation : t -> character -> character -> Relations.t

val write_relation : t -> character -> character -> Relations.t -> unit

(** Creates an empty state for the given number n of characters,
 * each indexed from 0 to n - 1. **)
val create_state : int -> t

val get_character_state : t -> character_state

val all_players : t -> character list

