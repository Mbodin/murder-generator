(** Module Ast.
 * This module defines the types that the Parser module produces
 * and feeds the Driver module.  Note that these types accepts
 * more things than what they should: this is why the driver is
 * being needed, to filter these invalid files and translate
 * the abstract syntax tree into a useful data structure for the
 * solver. **)

type attribute_kind =
  | Attribute
  | Contact

(** Constraints over players (in a let-be declaration). **)
type player_constraint =
  | HasAttribute of string * bool * string list
    (** States that the attribute described by the string should
     * be one of the constructors in the list (if the boolean is [false]).
     * If the boolean if [true], then the relation is opposite: the
     * attribute should not be in this list. **)
  | HasContact of string * string option * bool * string list
    (** States that the contact described by the first string with the
     * player described by the second string should be one of the
     * constructors in the list (if the boolean is [false]).
     * If the target player is [None], it means that it represents any
     * other player.
     * If the boolean if [true], then the relation is opposite: the
     * contact should not be in this list. **)

(** A type to express destinations.
 * The most important constructor is the first one: this type can be
 * tought as just a fancy type for string at first read. **)
type destination =
  | DestinationPlayer of string (** To a player reprensented by this variable. **)
  | AllOtherPlayers
      (** To all other players (than the ones declared in the element). **)
  | AllPlayers (** To all players (including the declared ones). **)

(** Describes a direction of a relation or a contact.
 * The type for player is parameterised to enable the use of the special
 * [destination] type instead. **)
type 'player target_destination =
  | FromTo of 'player * 'player (** From the first player to the second player. **)
  | Between of 'player list (** Between players, in a symmetrical way. **)

(** Describes a translation in a given language and required or given grammatical
 * cases. **)
type translation =
  Translation.language
  * Translation.command list
  * string Translation.sitem list

(** A description in a language. **)
type description = Translation.language * string

(** States that having this constructors set as attribute or contact changes
 * the grammatical cases of the player by implicitely providing the following
 * new tag at each translation. **)
type add = Translation.language * Translation.tag

(** Declare a player with some constraints.
 * If the player is [None], then these constraints apply to any
 * other player than the ones declared in the element. **)
type let_player = string option * player_constraint list

(** Provide a relation between two players. **)
type provide_relation = string target_destination * Relation.t

(** Provide an attribute value to a player. **)
type provide_attribute = {
    attribute_strictness : State.strictness
      (** How compatible this statement is with other [provide] commands. **) ;
    attribute_name : string (** The name of the provided attribute. **) ;
    attribute_player : destination
      (** The player to which this attribute should be attached. **) ;
    attribute_value : string list
      (** The possible constructors provided by this element to the specified player.
       * Each constructor in this list are valid possibilities. **)
  }

(** Provide a contact value to a player. **)
type provide_contact = {
    contact_strictness : State.strictness
      (** How compatible this statement is with other [provide] commands. **) ;
    contact_name : string (** The name of the provided contact. **) ;
    contact_destination : destination target_destination
      (** Defines which players get to be related. **) ;
    contact_value : string list
      (** The possible constructors provided by this element to the specified player.
       * Each constructor in this list are valid possibilities. **)
  }

type event_kind =
  | Kind of string (** User-declared kind. **)
  | KindAttribute of string (** An event providing this attribute. **)
  | KindContact of string * string list
      (** An event providing this contact to these players. **)

(** Declare some constraints over events. **)
type event_constraint = {
    constraint_kind : event_kind
      (** The constraint is about this event kind. **) ;
    constraint_players : string list
      (** The constraint only applies for these players **) ;
    constraint_after : bool
      (** If [true], the constraint is about events following this event,
       * if [false] about prior events. **) ;
    constraint_any : bool
      (** If [true], there should be at least one event of this kind in the
       * considered period.  If [false], there should be none. **)
  }

(** The possible commands present in a block. **)
type command =
  | OfCategory of string
    (** The given category is required to consider this block. **)
  | Translation of translation
  | Description of description
  | Sentence of block
    (** A sentence with multiple translations for an event.
     * Only accepts [Translation] commands. **)
  | Add of add
  | CompatibleWith of string
    (** States that if the current constructor is required in a
     * [provide compatible] command, but the given constructor
     * is present instead, then one can still apply this element. **)
  | LetPlayer of let_player
  | ProvideRelation of provide_relation
  | ProvideAttribute of provide_attribute
  | ProvideContact of provide_contact
  | AddDifficulty of bool * string list
    (** This element is more difficult than it looks like for these players.
     * If the boolean is [false], it is actually less difficult. **)
  | AddComplexity of bool * string list
    (** This element is more complex than it looks like for these players.
     * If the boolean is [false], it is actually less complex. **)
  | EventKind of string (** The event is of this particular kind. **)
  | ProvideEvent of provide_event
  | EventConstraint of event_constraint

(** Provide an event of this kind to these players. **)
and provide_event =
  bool * bool * Events.event_type * string list * block
  (** Provides an event of the following type to a list of player.
   * The first boolean states whether the event is blocking, the second
   * wether it is a phantom event.
   * Accepts the following commands: [Translation], [EventKind],
   * and [EventConstraint]. **)

(** A block that provides commands to a declaration.
 * Note that not all blocks can accept any kinds of commands: a post-parsing
 * treatment is needed.
 * See the Driver module (and its [convert_block] function in particular) for
 * more details. **)
and block = command list

type declaration =
  | DeclareInstance of attribute_kind * string * bool * block
    (** Declare a attribute of this name.
     * The boolean states whether the attribute is internal.
     * Only expects commands of the form [OfCategory] in its block. **)
  | DeclareConstructor of attribute_kind * string * string * bool * block
    (** Declare a attribute’s constructor for this attribute and of this name.
     * The boolean states whether the constructor is internal.
     * Accepts the following commands: [OfCategory], [Translation],
     * [Add], and [CompatibleWith]. **)
  | DeclareCategory of string * block
    (** Declare a category of this name.
     * Only expects commands of the form [OfCategory] and [Translation]. **)
  | DeclareElement of History.status * string * block
    (** Declare an element of this status and name.
     * Accepts the following commands: [LetPlayer], [OfCategory],
     * [ProvideRelation], [ProvideAttribute], [ProvideContact], and
     * [ProvideEvent]. **)
  | DeclareCase of Translation.language * Translation.tag
    (** Declare a grammatical case for a language. **)
  | DeclareEventKind of string * block
    (** Declare an event kind of this name.
     * Accepts the following commands: [OfCategory] and [EventKind]. **)

