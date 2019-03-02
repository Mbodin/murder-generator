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

type language = Translation.language
type language_tag = string

(** A translation is a sequence of translation items.
 * Items’s result are meant to be concatenated (separated by space) to form
 * the overall translation. **)
type translation_item =
  | TranslationString of string
    (** The given string is the translation. **)
  | TranslationVariable of string * language_tag list
    (** The string is a variable name (typically representing a player).
     * The tags are modifiers provided to this variable to fetch a translation
     * (typically, these tags indicate the grammatical case of the translation
     * to be given for the variable). **)

(** Constraints over players (in a let-be declaration). **)
type player_constraint =
  | HasAttribute of string * bool * string list
    (** States that the attribute described by the string should
     * be one of the constructors in the list (if the boolean is [false]).
     * If the boolean if [true], then the relation is opposite: the
     * attribute should not be in this list. **)
  | HasContact of string * string * bool * string list
    (** States that the contact described by the first string with the
     * player described by the second string should be one of the
     * constructors in the list (if the boolean is [false]).
     * If the boolean if [true], then the relation is opposite: the
     * contact should not be in this list. **)

(** Describes a direction of a relation or a contact. **)
type target_destination =
  | FromTo of string * string (** From the first player to the second player. **)
  | Between of string * string (** Between both players, in a symmetrical way. **)

(** Describes a translation in a given language and required grammatical cases.
 * The additional tags state how this translation should be grammatically
 * interpreted in interaction with other translations.
 * For instance in gendered languages, the generic word for a group of person
 * could be gendered: the description of the group has to transmit this information
 * to the translation where it is used.
 * This is how this second list of tags is used. **)
type translation =
  language * language_tag list * translation_item list * language_tag list

(** States that having this constructors set as attribute or contact changes
 * the grammatical cases of the player by implicitely providing the following
 * new tags at each translation. **)
type add = language * language_tag list

(** Declares a player with some constraints. **)
type let_player = string * player_constraint list

(** Provide a relation between two players. **)
type provide_relation = target_destination * Relation.t

(** Provide an attribute value to a player. **)
type provide_attribute = {
    attribute_strictness : State.strictness
      (** How compatible this statement is with other [provide] commands. **) ;
    attribute_name : string (** The name of the provided attribute. **) ;
    attribute_player : string (** The variable name representing the player. **) ;
    attribute_value : string list
      (** The possible constructors provided by this element to the specified player.
       * Each constructor in this list are valid possibilities. **)
  }

(** Provide a contact value to a player. **)
type provide_contact = {
    contact_strictness : State.strictness
      (** How compatible this statement is with other [provide] commands. **) ;
    contact_name : string (** The name of the provided contact. **) ;
    contact_destination : target_destination
      (** Defines which players get to be related. **) ;
    contact_value : string list
      (** The possible constructors provided by this element to the specified player.
       * Each constructor in this list are valid possibilities. **)
  }

(** The possible commands present in a block. **)
type command =
  | OfCategory of string
    (** The given category is required to consider this block. **)
  | Translation of translation
  | Add of add
  | CompatibleWith of string
    (** States that if the current constructor is required in a
     * [provide compatible] command, but the given constructor
     * is present instead, then one can still apply this element. **)
  | LetPlayer of let_player
  | ProvideRelation of provide_relation
  | ProvideAttribute of provide_attribute
  | ProvideContact of provide_contact
  (* TODO: [ProvideEvent] *)

(** A block that provides commands to a declaration.
 * Note that not all blocks can accept any kinds of commands: a post-parsing
 * treatment is needed.
 * See the Driver module (and its [convert_block] function in particular) for
 * more details. **)
type block = command list

type declaration =
  | DeclareInstance of attribute_kind * string * block
    (** Declares a attribute.
     * Only expects commands of the form [OfCategory] in its block. **)
  | DeclareConstructor of attribute_kind * string * string * block
    (** Declares a attribute’s constructor.
     * Accepts the following commands: [OfCategory], [Translation],
     * [Add], and [CompatibleWith]. *)
  | DeclareCategory of string * block
    (** Declares a category.
     * Only expects commands of the form [OfCategory] and [Translation]. **)
  | DeclareElement of string * block
    (** Declares an element.
     * Accepts the following commands: [LetPlayer], [OfCategory],
     * [ProvideRelation], [ProvideAttribute], and [ProvideContact]. **)
  (* TODO: [DeclareEvent] *)

