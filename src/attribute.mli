(** Module Attribute
   Describes the player and contact attributes used thorough this development. *)

(** A signature for the various attribute/constructor structures.
   This signature is used in particular for the player attributes and the contact
   attributes. *)
module type Attribute = sig

    (** The type of attributes. *)
    type attribute

    (** The type of constructors. *)
    type constructor

    (** Attribute kinds.
       It describes to which objects this attribute can be applied. *)
    type kind

    (** The maximum kind: it is a supertype of any kind. *)
    val any : kind

    (** Values are just constructor identifiers.
       Each attribute is associated with a given set of possible constructors
       (which are really just names).
       The following table keeps track of the constructor names. *)
    type constructor_map

    (** An empty constructor map. *)
    val empty_constructor_map : constructor_map

    (** Return the name of an attribute. *)
    val attribute_name : constructor_map -> attribute -> string option

    (** Return the kind of an attribute. *)
    val attribute_kind : constructor_map -> attribute -> kind option

    (** Return the name of a constructor. *)
    val constructor_name : constructor_map -> constructor -> string option

    (** Return the associated attribute of a constructor. *)
    val constructor_attribute : constructor_map -> constructor -> attribute option

    (** Return the list of constructors associated to an attribute. *)
    val constructors : constructor_map -> attribute -> constructor list option

    (** Returns all defined constructors. *)
    val all_constructors : constructor_map -> constructor list

    (** Declare an attribute, returning its associated normal identifier.
       The string is the name of the attribute and the boolean states whether
       it is internal.
       If already declared, its previously-set identifier is still returned,
       but its internal status is updated. *)
    val declare_attribute : constructor_map -> string -> bool -> kind -> attribute * constructor_map

    (** Declare a new constructor for an attribute.
       The string is the name of the constructor and the boolean states whether
       it is internal.
       If already declared, its previously-set identifier is still returned,
       but its internal status is updated. *)
    val declare_constructor : constructor_map -> attribute -> string -> bool -> constructor * constructor_map

    (** Get the attribute identifier from its name. *)
    val get_attribute : constructor_map -> string -> attribute option

    (** Get the constructor identifier from its attribute and its name. *)
    val get_constructor : constructor_map -> attribute -> string -> constructor option

    (** Users can remove categories before the story generation.
       This function removes a constructor, probably because it was associated
       to an unwanted category. *)
    val remove_constructor : constructor_map -> constructor -> constructor_map

    (** State that the first constructor is compatible with the second. *)
    val declare_compatibility : constructor_map -> attribute -> constructor -> constructor -> constructor_map

    (** State whether the first constructor is compatible with the second. *)
    val is_compatible : constructor_map -> attribute -> constructor -> constructor -> bool

    (** State whether the given constructor is internal. *)
    val is_internal : constructor_map -> attribute -> constructor -> bool

  end

(** The type of object constructors, and in particular of the constructors
   corresponding to the [object_type] attribute. *)
type object_constructor

(** This type describes to whom an attribute can be attached. *)
type attribute_kind =
  | Player (** Can only be applied to a player. *)
  | AnyObject (** Any object *)
  | Object of object_constructor (** Can only be applied to this kind of object. *)

(** Contacts are attached between a starting object and a destination:
   this type describes which can appear as target and destination. *)
type contact_kind = {
    kind_from : attribute_kind ;
    kind_to : attribute_kind
  }

(** State whether the first kind is a subtype of the second. *)
val attribute_sub : attribute_kind -> attribute_kind -> bool

(** Lifting of the function [attribute_sub] to disjunctive lists. *)
val attributes_sub : attribute_kind list -> attribute_kind list -> bool

(** As for [attribute_sub], but for contact kinds. *)
val contact_sub : contact_kind -> contact_kind -> bool

(** Lifting of the function [contact_sub] to disjunctive lists. *)
val contacts_sub : contact_kind list -> contact_kind list -> bool

(** The attribute kind applying to any player or object:
   it is a supertype of any attribute type. *)
val attribute_any : attribute_kind list

(** The contact kind starting and ending from any player or object:
   it is a supertype of any contact type. *)
val contact_any : contact_kind list

(** A module to express attributes and constructors for players and objects.
   Its kind is given through a disjunctive list of kinds. *)
module PlayerAttribute : Attribute
  with type constructor = object_constructor
   and type kind = attribute_kind list

(** A module to express attributes and constructors for contacts between players.
   As for [PlayerAttribute], the [kind] type is a disjunctive list. *)
module ContactAttribute : Attribute
  with type kind = contact_kind list

(** This data structure stores all the informations about constructors. *)
type constructor_maps = {
    player : PlayerAttribute.constructor_map ;
    contact : ContactAttribute.constructor_map
  }

(** An empty structure. *)
val empty_constructor_maps : constructor_maps

(** A special internal attribute for objects. *)
val object_type : PlayerAttribute.attribute

(** A generic attribute type for modules who don’t need to precisely
   understand how attributes work, merging both kinds of attributes. *)
type attribute =
  | PlayerAttribute of PlayerAttribute.attribute
  | ContactAttribute of ContactAttribute.attribute

(** Similarly, a generic constructor type. *)
type constructor =
  | PlayerConstructor of PlayerAttribute.constructor
  | ContactConstructor of ContactAttribute.constructor

