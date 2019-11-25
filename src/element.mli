(** Module Element
 * Data structure and functions about story elements. **)

type character = State.character

(** A constraint on a character **)
type character_constraint =
  | Attribute of Attribute.PlayerAttribute.attribute
                 * Attribute.PlayerAttribute.constructor State.attribute_value
    (** The given attribute value is provided by the element.
      * Note that [State.attribute_constructor] can be of the form
      * [State.One_value_of], in which case the attribute is required to be of
      * this value, but also [State.Fixed_value], where the element actually
      * provides an explanation for it. **)
  | Contact of Attribute.ContactAttribute.attribute
               * int option (** If this number is [None], this contact is meant
                              * towards all other players than the ones declared
                              * in the element. **)
               * Attribute.ContactAttribute.constructor State.attribute_value
    (** The given contact (identified in the local array) is provided by the
     * element. **)

(** All the changes applied by an elements to players are summed up in this type. **)
type cell = {
    constraints : character_constraint list
      (** The constraints on this player. **) ;
    relations : Relation.t array
      (** The relations that would be added to this player,  for each characters.
       * This array can be less than the number of players  in this element
       * ([Neutral] is then assumed for all other cells). **) ;
    added_objective : State.objective
      (** Some difficulty or complexity, provided in addition
       * to the ones naturally provided by the relations. **)
  }

(** Each players considered by the element are represented as a cell.
 * A list of constraints given to other players is also given
 * (it corresponds to the [let any other player] declarations.
 * Events are stored as a list, characters being represented by their
 * index in the cell array.
 * Events must not be directly contradictory: if an event has a
 * constraint preventing an event of a given kind to be after this
 * event, it must not be after it in the list.
 * Finally, a unique identifier for each event is provided. **)
type t = {
    status : History.status ;
    players : cell array ;
    others : character_constraint list ;
    events : int Events.t list ;
    id : Id.t
  }

(** Returns the list of attribute that an element may provide. **)
val provided_attributes : t -> Attribute.attribute list

(** Given a state, an element, and an instantiation of the characters, states
 * whether the event can be applied.
 * If not, it returns [None].
 * If the element can be applied, it states whether the element is making
 * progress, that is whether there exists at least one attribute value that
 * has been changed to something recognised by [State.attribute_value_progress].
 * The boolean is lazily evaluated. **)
val compatible_and_progress : Attribute.constructor_maps -> State.t -> t -> character array -> bool Lazy.t option

(** Given an element and a language, states whether it can be safely translated into
 * a given language (that is, whether the language is listed amongst all its translations
 * and that default cases are well-handled in these translations). **)
val is_translatable : t -> Translation.language -> bool

(** To speed-up the search for instantiation, a cache is left in the state.
 * This type describes this cache. **)
type cache

(** An empty cache to be used at initialisation. **)
val empty_cache : cache

(** Look for instantiations.
 * The second return value is the result of [compatible_and_progress] on this
 * instantiation.
 * It tries to return an instantiation that progresses. **)
val search_instantiation : Attribute.constructor_maps -> (State.t, cache) Utils.cached -> t -> (character array * bool) option

(** This type carries information about how the state have been changed
 * by the [apply] function. **)
type attribute_differences

(** Compose two differences of attribute.
 * The function is optimised for when the first argument is larger than the
 * second. **)
val merge_attribute_differences : attribute_differences -> attribute_differences -> attribute_differences

(** Returns some value of the weight of the difference of attributes after
 * the application of an event.
 * A difference of zero means that as many constraints have been solved than
 * constraints that have been added.
 * Negative values means that more constraints have been added than solved,
 * and positive means that more have been solved. **)
val difference_weigth : attribute_differences -> int

(** Returns the difference for a specific attribute. **)
val difference_for_attribute : attribute_differences -> Attribute.attribute -> int

(** Returns all attributes who value associated by [difference_for_attribute]
 * is negative, that is, the attributes with constraints to be solved. **)
val difference_attribute_in_need : attribute_differences -> Attribute.attribute list

(** Return an empty difference, with a weight of zero. **)
val empty_difference : attribute_differences

(** Apply the given element to the state according to this instantiation.
 * This application is not functional (although the new state is returned,
 * invalidating the previous one).
 * This function should only be applied to instantiations for which
 * [compatible_and_progress] returns [Some].
 * Once the total number of attribute to be defined is zero, the state can be
 * published. **)
val apply : Attribute.constructor_maps -> State.t -> t -> character array -> State.t * attribute_differences

(** Same as [apply], but create a copy of the state before the application. **)
val safe_apply : Attribute.constructor_maps -> State.t -> t -> character array -> State.t * attribute_differences

(** Get the resulting relation state from an instantiation.
 * The input state is not modified by this function. **)
val apply_relations : State.relation_state -> t -> character array -> State.relation_state

(** Apply the attributes to a given character.
 * These constructors will be applied with the [State.One_value_of] constructor. **)
val apply_attributes : Attribute.constructor_maps -> State.t -> character -> Attribute.PlayerAttribute.constructor list -> (State.t * attribute_differences) option

(** Apply the contacts to a given source and target characters.
 * These constructors will be applied with the [State.One_value_of] constructor. **)
val apply_contacts : Attribute.constructor_maps -> State.t -> character -> character -> Attribute.ContactAttribute.constructor list -> (State.t * attribute_differences) option

