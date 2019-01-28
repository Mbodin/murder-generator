
type attribute_kind =
  | Attribute
  | Contact

type language = string
type language_tag = string

type translation_item =
  | TranslationString of string * language_tag list
  | TranslationVariable of string * language_tag list

type player_constraint =
  | HasAttribute of string * string
  | HasContact of string * string * string

type command =
  | OfCategory of string
  | Translation of language * language_tag list * translation_item list
  | Add of language * language_tag list
  | CompatibleWith of string
  | LetPlayer of string * player_constraint list
  | ProvideRelation of string * string * Relation.t
  | ProvideAttribute of State.strictness * string * string * string
  | ProvideContact of State.strictness * string * string * string * string

type declaration =
  | DeclareInstance of attribute_kind * string
  | DeclareConstructor of attribute_kind * string * string * command list
  | DeclareCategory of string * command list
  | DeclareElement of string * command list

