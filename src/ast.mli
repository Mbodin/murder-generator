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

type translation_item =
  | TranslationString of string * language_tag list
  | TranslationVariable of string * language_tag list

type player_constraint =
  | HasAttribute of string * string
  | HasContact of string * string * string

type target_destination =
  | FromTo of string * string
  | Between of string * string

type translation = language * language_tag list * translation_item list
type add = language * language_tag list
type let_player = string * player_constraint list
type provide_relation = target_destination * Relation.t
type provide_attribute = {
    attribute_strictness : State.strictness ;
    attribute_name : string ;
    attribute_player : string ;
    attribute_value : string list
  }
type provide_contact = {
    contact_strictness : State.strictness ;
    contact_name : string ;
    contact_destination : target_destination ;
    contact_value : string list
  }

type command =
  | OfCategory of string
  | Translation of translation
  | Add of add
  | CompatibleWith of string
  | LetPlayer of let_player
  | ProvideRelation of provide_relation
  | ProvideAttribute of provide_attribute
  | ProvideContact of provide_contact

type block = command list

type declaration =
  | DeclareInstance of attribute_kind * string * block
  | DeclareConstructor of attribute_kind * string * string * block
  | DeclareCategory of string * block
  | DeclareElement of string * block

