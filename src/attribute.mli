(** Module Attribute
   Describes the player and contact attributes used thorough this development.
   Attributes and contacts share quite a lot of constructs, which this files
   tries to factorise.  The structure of this file might feel akward because
   of this. *)

(** This data structure stores all the information about the constructors
   of (player or object) attributes. *)
type player_constructor_map

(** This data structure stores all the information about the constructors
   of contacts. *)
type contact_constructor_map

(** This data structure stores all the informations about constructors. *)
type constructor_maps = {
    player : player_constructor_map ;
    contact : contact_constructor_map
  }

(** An empty structure. *)
val empty_constructor_maps : constructor_maps

(** The type of (player or object) attributes. *)
type player_attribute

(** The type of contacts. *)
type contact_attribute

(** A generic attribute type for modules who don’t need to precisely
   understand how attributes work, merging both kinds of attributes. *)
type attributes =
  | PlayerAttribute of player_attribute
  | ContactAttribute of contact_attribute

(** A special internal attribute for objects. *)
val object_type : player_attribute

(** The type of the constructors of (player or object) attributes. *)
type player_constructor

(** The type of the constructors of contacts. *)
type contact_constructor

(** Similarly, a generic constructor type. *)
type constructors =
  | PlayerConstructor of player_constructor
  | ContactConstructor of contact_constructor

(** The type representing objects, that is the constructors
   corresponding to the [object_type] attribute. *)
type object_constructor = player_constructor

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

(** The kind of (player or object) attributes. *)
type player_kinds = attribute_kind list

(** The kind of contacts. *)
type contact_kinds = contact_kind list

(** State whether the first kind is a subtype of the second. *)
val attribute_sub : attribute_kind -> attribute_kind -> bool

(** Lifting of the function [attribute_sub] to disjunctive lists. *)
val attributes_sub : player_kinds -> player_kinds -> bool

(** As for [attribute_sub], but for contact kinds. *)
val contact_sub : contact_kind -> contact_kind -> bool

(** Lifting of the function [contact_sub] to disjunctive lists. *)
val contacts_sub : contact_kinds -> contact_kinds -> bool

(** The attribute kind applying to any player or object:
   it is a supertype of any attribute type. *)
val attribute_any : player_kinds

(** The contact kind starting and ending from any player or object:
   it is a supertype of any contact type. *)
val contact_any : contact_kinds

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

    (* TODO: I might just remove it from the signature, to avoid mixing between
       this and [empty_constructor_maps.player]. *)
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
       but the modifications over the internal status and its kind are unspecified. *)
    val declare_attribute : constructor_map -> string -> bool -> kind -> attribute * constructor_map

    (** Similar than [declare_attribute], but the internal status and its kind are given
       values that will be erased if the attribute is later properly declared.
       If the attribute is already declared, then these values are not changed. *)
    val name_attribute : constructor_map -> string -> attribute * constructor_map

    (** Declare a new constructor for an attribute.
       The string is the name of the constructor and the boolean states whether
       it is internal.
       If already declared, its previously-set identifier is still returned,
       but the action over the internal status is unspecified. *)
    val declare_constructor : constructor_map -> attribute -> string -> bool -> constructor * constructor_map

    (** Same as [declare_constructor], but the internal status is given a value that will
       be erased if the constructor is later properly declared.
       If the constructor is already declared, its internal status is not changed.
       Note how the [attribute] argument is not facultative here: constructors with the same
       name but different attributes are considered different. *)
    val name_constructor : constructor_map -> attribute -> string -> constructor * constructor_map

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

    (** Projection of the [constructor_maps] to either its field [player] or [contact]. *)
    val get_constructor_maps : constructor_maps -> constructor_map

    (** Sets the projection of the [constructor_maps]. *)
    val set_constructor_maps : constructor_maps -> constructor_map -> constructor_maps

    (** Calls the right constructor [PlayerAttribute] or [ContactAttribute]. *)
    val to_attributes : attribute -> attributes

    (** Calls the right constructor [PlayerConstructor] or [ContactConstructor]. *)
    val to_constructors : constructor -> constructors

    (** Either ["attribute"] or ["contact"]. *)
    val name : string

  end

(** A module to express attributes and constructors for players and objects.
   Its kind is given through a disjunctive list of kinds. *)
module PlayerAttribute : Attribute
  with type attribute = player_attribute
   and type constructor = object_constructor
   and type constructor_map = player_constructor_map
   and type kind = player_kinds

(** A module to express attributes and constructors for contacts between players.
   As for [PlayerAttribute], the [kind] type is a disjunctive list. *)
module ContactAttribute : Attribute
  with type attribute = contact_attribute
   and type constructor = contact_constructor
   and type constructor_map = contact_constructor_map
   and type kind = contact_kinds

