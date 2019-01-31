%{ (** Module Parser. **)
   (** This is the main parsing file.
    * This parser however structures relatively few information:
    * see the module Driver for an additional structuration.
    * In particular, each blocks are parsed the same way,
    * even if, for instance, let-be declarations can’t appear
    * in a category declaration.  Yet, this parser accepts them.
    * However, the driver won’t. **)

open Ast
%}

%token          EOF
%token          BEGIN END
%token          CATEGORY
%token          ELEMENT
%token          ATTRIBUTE CONTACT RELATION
%token          PLAYER EVENT
%token          ALL
%token          DECLARE PROVIDE LET BE
%token          WITH AND OR FROM TO BETWEEN AS
%token          STRICT COMPATIBLE
%token          NEUTRAL HATE TRUST CHAOTIC UNDETERMINED AVOIDANCE
%token          ASYMMETRICAL EXPLOSIVE STRONG
%token<string>  LIDENT UIDENT STRING
%token          TRANSLATION ADD COLON

%start<Ast.declaration list> main

%%

main: l = list (declaration); EOF { l }

declaration:
  | DECLARE; k = attribute_kind; id = UIDENT; b = loption (block)
    { DeclareInstance (k, id, b) }
  | k = attribute_kind; attr = UIDENT; constructor = UIDENT; b = loption (block)
    { DeclareConstructor (k, attr, constructor, b) }
  | CATEGORY; name = UIDENT; c = loption (block)
    { DeclareCategory (name, c) }
  | ELEMENT; name = UIDENT; c = block
    { DeclareElement (name, c) }

attribute_kind:
  | ATTRIBUTE   { Attribute }
  | CONTACT     { Contact }

block: BEGIN; l = list (command); END { l }

language_tags: tags = list (COLON; tag = LIDENT { tag }) { tags }

command:
  | CATEGORY; c = UIDENT
    { OfCategory c }
  | TRANSLATION; lang = LIDENT; tags = language_tags;
    l = list ( str = STRING; tags = language_tags
               { TranslationString (str, tags) }
             | id = UIDENT; tags = language_tags
               { TranslationVariable (id, tags) });
    { Translation (lang, tags, l) }
  | ADD; lang = LIDENT; tags = language_tags
    { Add (lang, tags) }
  | COMPATIBLE; WITH; v = UIDENT
    { CompatibleWith v }
  | LET; v = UIDENT; BE; PLAYER;
    l = list (player_constraint)
    { LetPlayer (v, l) }
  | PROVIDE; RELATION;
    d = target_destination;
    AS; r = relation
    { ProvideRelation (d, r) }
  | PROVIDE; s = strictness;
    ATTRIBUTE; a = UIDENT;
    TO; p = UIDENT;
    AS; v = UIDENT
    { ProvideAttribute {
          attribute_strictness = s ;
          attribute_name = a ;
          attribute_player = p ;
          attribute_value = v
        } }
  | PROVIDE; s = strictness;
    CONTACT; c = UIDENT;
    d = target_destination;
    AS; v = UIDENT
    { ProvideContact {
          contact_strictness = s ;
          contact_name = c ;
          contact_destination = d ;
          contact_value = v
        } }

target_destination:
  | FROM; p1 = UIDENT; TO; p2 = UIDENT
    { FromTo (p1, p2) }
  | BETWEEN; p1 = UIDENT; AND; p2 = UIDENT
    { Between (p1, p2) }

player_constraint:
  | WITH; ATTRIBUTE; a = UIDENT; AS; v = UIDENT
    { HasAttribute (a, v) }
  | WITH; CONTACT; c = UIDENT; TO; p = UIDENT; AS; v = UIDENT
    { HasContact (c, p, v) }

relation: b = boption (STRONG { }); r = relation_content { (r, b) }

relation_content:
  | b = basic_relation
    { Relation.Basic b }
  | ASYMMETRICAL; b1 = basic_relation; b2 = basic_relation
    { Relation.Asymmetrical (b1, b2) }
  | EXPLOSIVE; r1 = relation_content; r2 = relation_content
    { Relation.Explosive (r1, r2) }

%inline basic_relation:
  | NEUTRAL         { Relation.Neutral }
  | HATE            { Relation.Hate }
  | TRUST           { Relation.Trust }
  | CHAOTIC         { Relation.Chaotic }
  | UNDETERMINED    { Relation.Undetermined }
  | AVOIDANCE       { Relation.Avoidance }

%inline strictness:
  | COMPATIBLE  { State.NonStrict }
  | empty       { State.LowStrict }
  | STRICT      { State.Strict }

%inline empty: { () }

