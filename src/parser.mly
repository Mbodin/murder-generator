%{ (** Module Parser. **)
   (** This is the main parsing file. **)
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
%token<string>  IDENT STRING
%token          TRANSLATION ADD COLON

%start<Ast.declaration list> main

%%

main: l = list (declaration); EOF { l }

declaration:
  | DECLARE; k = attribute_kind; id = IDENT
    { DeclareInstance (k, id) }
  | k = attribute_kind; attr = IDENT; constructor = IDENT;
    inner = loption (block)
    { DeclareConstructor (k, attr, constructor, inner) }
  | CATEGORY; name = IDENT; c = loption (block)
    { DeclareCategory (name, c) }
  | ELEMENT; name = IDENT; c = block
    { DeclareElement (name, c) }

attribute_kind:
  | ATTRIBUTE   { Attribute }
  | CONTACT     { Contact }

block: BEGIN; l = list (command); END { l }

language_tags: tags = list (COLON; tag = IDENT { tag }) { tags }

command:
  | CATEGORY; c = IDENT
    { OfCategory c }
  | TRANSLATION; lang = IDENT; tags = language_tags;
    l = list ( str = STRING; tags = language_tags
               { TranslationString (str, tags) }
             | id = IDENT; tags = language_tags
               { TranslationVariable (id, tags) });
    { Translation (lang, tags, l) }
  | ADD; lang = IDENT; tags = language_tags
    { Add (lang, tags) }
  | COMPATIBLE; WITH; v = IDENT
    { CompatibleWith v }
  | LET; v = IDENT; BE; PLAYER;
    l = list (player_constraint)
    { LetPlayer (v, l) }
  | PROVIDE; RELATION; BETWEEN;
    p1 = IDENT; AND; p2 = IDENT;
    AS; r = relation
    { ProvideRelation (p1, p2, r) }
  | PROVIDE; s = strictness; ATTRIBUTE;
    a = IDENT; TO; p = IDENT;
    AS; v = IDENT
    { ProvideAttribute (s, a, p, v) }
  | PROVIDE; s = strictness; CONTACT;
    c = IDENT; FROM; p1 = IDENT; TO; p2 = IDENT;
    AS; v = IDENT
    { ProvideContact (s, c, p1, p2, v) }

player_constraint:
  | WITH; ATTRIBUTE; a = IDENT; AS; v = IDENT
    { HasAttribute (a, v) }
  | WITH; CONTACT; c = IDENT; TO; p = IDENT; AS; v = IDENT
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

