%{ (** Module Parser. **)
   (** This is the main parsing file. **)

type void

type attribute_kind =
  | Attribute
  | Contact

type 'a compound =
  | OfCategory of string
  | CBase of 'a

type language = string
type language_tag = string

type translation_item =
  | TranslationString of string * language_tag list
  | TranslationVariable of string * language_tag list

type 'a translatable =
  | Translation of language * language_tag list * translation_item list
  | LBase of 'a

type element =
  | LetPlayer of string * TODO

type constructor_property =
  | Add of language * language_tag list
  | CompatibleWith of string

type declaration =
  | DeclareInstance of attribute_kind * string
  | DeclareConstructor of attribute_kind * string * string
                          * constructor_property list translatable compound
  | DeclareCategory of void translatable compound
  | DeclareElement of element compound

%}

%nonassoc   CATEGORY ELEMENT
%nonassoc   ALL PLAYER EVENT ATTRIBUTE CONTACT RELATION IDENT STRING
%left       DECLARE PROVIDE LET BE
%nonassoc   STRICT COMPATIBLE
%left       WITH
%left       FROM TO BETWEEN AS
%left       AND OR
%left       TRANSLATION ADD COLON
%left       ASYMMETRICAL EXPLOSIVE STRONG
%nonassoc   BEGIN END

%start<declaration list> main

%%

main:
  | l = list (declaration)  { l }

declaration:
  | DECLARE; k = attribute_kind; id = IDENT
    { DeclareInstance (k, id) }
  | k = attribute_kind; attr = IDENT; constructor = IDENT;
    inner = loption (content (translatable (constructor_property)))
    { DeclareConstructor (k, attr, constructor, inner) }
  | CATEGORY; name = IDENT; c = content (translatable (void))
    { DeclareCategory (name, c) }
  | ELEMENT; name = IDENT; c = content (element)
    { DeclareElement (name, c) }

void:   { }

attribute_kind:
  | ATTRIBUTE   { Attribute }
  | CONTACT     { Contact }

content (X):
    BEGIN;
    l = list ( x = X                { CBase x }
             | CATEGORY; id = IDENT { OfCategory id });
    END  { l }

translatable (X):
  | x = X       { LBase x }
  | TRANSLATION; lang = IDENT;
    tags = language_tags;
    l = list ( str = STRING; tags = language_tags
               { TranslationString (str, tags) }
             | id = IDENT; tags = language_tags
               { TranslationVariable (id, tags) });
    { Translation (lang, tags, l) }

language_tags:
    tags = list (COLON; tag = IDENT { tag })    { tags }

constructor_property:
  | ADD; lang = IDENT; tags = language_tags
    { Add (lang, tags) }
  | COMPATIBLE; WITH; v = IDENT
    { CompatibleWith v }

element:
  | TODO

