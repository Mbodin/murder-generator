(** Module State
 * Describes the state of the solver. **)

type character = History.character

(** A mapping from pairs of characters to Relation.t. **)
type relation_state

(** Returns the relation between two characters.
 * The relations are usually symmetrical, but note how the asymmetrical
 * relation between [c1] and [c2] is represented by [Asymmetrical (r1, r2)]
 * where [r1] is the relation from the point of view of [c1].
 * If a mapping is not present or if both characters are the same,
 * it returns the default relation [Relation.Basic Relation.Neutral]. **)
val read_relation_state : relation_state -> character -> character -> Relation.t

(** Non-functionally update the relation state. **)
val write_relation_state : relation_state -> character -> character -> Relation.t -> unit

(** The following exception is returned if one tries to write or read
 * a relation between two identical characters. **)
exception SelfRelation

(** Creates an empty relation state for the given number n of characters,
 * each indexed from [0] to [n - 1]. **)
val create_relation_state : int -> relation_state

(** The state of attributes and contact of each character. **)
type character_state

(** A signature for the various value/attribute structures.
 * This signature is used in particular for the player attributes and the contact attributes. **)
module type Attribute = sig

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
     * This function removes a constructor, probably because it was associated
     * an unwanted category. **)
    val remove_constructor : constructor_map -> attribute -> value -> constructor_map

  end

(** A module to express attributes and values for players. **)
module PlayerAttribute : Attribute with type attribute = Utils.Id.t
                                   and type value = Utils.Id.t

(** A module to express attributes and values for contacts between players. **)
module ContactAttribute : Attribute with type attribute = Utils.Id.t
                                    and type value = Utils.Id.t

(** This data structure stores all the informations about constructors. **)
type constructor_maps = {
    player : PlayerAttribute.constructor_map ;
    contact : ContactAttribute.constructor_map
  }

(** An empty structure. **)
val empty_constructor_maps : constructor_maps

(** A generic attribute type for modules who don’t need to precisely
 * understand how attributes work, merging both kinds of attributes. **)
type attribute =
  | PlayerAttribute of PlayerAttribute.attribute
  | ContactAttribute of ContactAttribute.attribute

(** The strictness flag indicates whether a value can be explained by different elements. **)
type strictness =
  | NonStrict (** A non-strict value can be explained by as many elements as needed. For instance, how two persons know each other (there might be more than one reason for that). **)
  | LowStrict (** A low-strict explanation does not conflict with non-strict explanations, but only one low-strict can be set for a given value.
               * This can be used for family relations: there might be non-strict explanations for why people are on the same family (like weddings), but one can’t be both the father and brother of someone. **)
  | Strict (** A strict value can be explained by only one element. **)

(** Waiting for a value to be decided for a given attribute,
 * the following type is used instead. **)
type 'value attribute_value =
  | Fixed_value of 'value * strictness (** The value has already been fixed. TODO LATER: Put a list of values here.
                                        * It can not be changed back.
                                        * The strictness flag indicates how it accepts to be redefined (with the same value). **)
  | One_value_of of 'value list (** The value has not been yet fixed, but it is known to be one of these. **)
(** Note that a [One_value_of] associated with a singleton list is not equivalent
 * to a [Fixed_value]: the latter has been approved by a story element (possibly
 * associating it with an event), whilst the former hasn’t. **)
(** Furthermore, note that [One_value_of] associated with an empty list is possible.
 * The underlying meaning is that this attribute must not be used. **)

(** Compose the strictness flag, returning [None] when they are incompatible. **)
val compose_strictness : strictness -> strictness -> strictness option

(** Compose two attribute values, if they are compatible. **)
val compose_attribute_value : 'a attribute_value -> 'a attribute_value -> 'a attribute_value option

(** Given a transition from an attribute value to another, this function states whether the transition [One_value_of] to [Fixed_value] occured. **)
val attribute_value_progress : 'a attribute_value -> 'a attribute_value -> bool

(** States whether there exists another attribute value such that [attribute_value_progress] may make progress.
 * An attribute value can only make progress once (from [One_value_of] to [Fixed_value]). **)
val attribute_value_can_progress : 'a attribute_value -> bool

(** Creates an empty character state for the given number n of characters,
 * each indexed from 0 to n - 1. **)
val create_character_state : int -> character_state

(** Get a character attribute from the character state. **)
val get_attribute_character : character_state -> character -> PlayerAttribute.attribute -> PlayerAttribute.value attribute_value option

(** Get a character attribute from the character state.
 * If it is not present, the character set is non-functionally updated
 * to mark the attribute as being of need of a value (returning all the
 * constructors of its type). **)
val force_get_attribute_character : PlayerAttribute.constructor_map -> character_state -> character -> PlayerAttribute.attribute -> PlayerAttribute.value attribute_value

(** Non-functionally associates the given attribute of the character to the given value. **)
val write_attribute_character : character_state -> character -> PlayerAttribute.attribute -> PlayerAttribute.value attribute_value -> unit

(** Get a contact from the character state, using the target character. **)
val get_contact_character : character_state -> character -> ContactAttribute.attribute -> character -> (ContactAttribute.value attribute_value) option

(** Get all the contacts of a character from the character state. **)
val get_all_contact_character : character_state -> character -> ContactAttribute.attribute -> (character * ContactAttribute.value attribute_value) list

(** A state is just a combination of each state component. **)
type t =
  character_state * relation_state * History.state

(** Get the [relation_state] component of the state. **)
val get_relation_state : t -> relation_state

(** Read the relation between two different characters in a state. **)
val read_relation : t -> character -> character -> Relation.t

(** Non-functionally updates a relation in a state.
 * The two characters have to be different.
 * This function writes both directions of the relation
 * (that is, there is no need to call both [write_relation s c1 c2 r]
 * and [write_relation s c2 c1 (Relation.reverse r)] at the same time. **)
val write_relation : t -> character -> character -> Relation.t -> unit

(** Creates an empty state for the given number n of characters,
 * each indexed from [0] to [n - 1]. **)
val create_state : int -> t

(** Get the character state component of a state. **)
val get_character_state : t -> character_state

(** Returns the size of the state, that its number of players. **)
val number_of_player : t -> int

(** Similar to [number_of_player], but from a relation state. **)
val number_of_player_relation_state : relation_state -> int

(** Returns the list of all players defined in this state. **)
val all_players : t -> character list

(** Similar to [all_players], but from a relation state. **)
val all_players_relation : relation_state -> character list

