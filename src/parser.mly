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
%token          WITH AND OR NOT FROM TO BETWEEN AS
%token          STRICT COMPATIBLE
%token          NEUTRAL HATE TRUST CHAOTIC UNDETERMINED AVOIDANCE
%token          ASYMMETRICAL EXPLOSIVE STRONG
%token<string>  LIDENT UIDENT STRING
%token          TRANSLATION ADD COLON

%start<Ast.declaration list> main

%%

main: l = list (declaration); EOF { l }

declaration:
  | DECLARE; k = attribute_kind; id = UIDENT; b = block
    { DeclareInstance (k, id, b) }
  | k = attribute_kind; attr = UIDENT; constructor = UIDENT; b = block
    { DeclareConstructor (k, attr, constructor, b) }
  | CATEGORY; name = UIDENT; c = block
    { DeclareCategory (name, c) }
  | ELEMENT; name = UIDENT; c = block
    { DeclareElement (name, c) }

attribute_kind:
  | ATTRIBUTE   { Attribute }
  | CONTACT     { Contact }

block: l = loption (BEGIN; l = list (command); END { l })   { l }

language:
  | lang = LIDENT   { lang }
  (* Any two- or three-characters identifier can be a language. *)
  | AND             { "and" }
  | OR              { "or" }
  | NOT             { "not" }
  | ALL             { "all" }
  | END             { "end" }
  | LET             { "let" }
  | BE              { "be" }
  | TO              { "to" }
  | AS              { "as" }
  | ADD             { "add" }

language_tags: tags = list (COLON; tag = LIDENT { tag }) { tags }

command:
  | CATEGORY; c = UIDENT
    { OfCategory c }
  | TRANSLATION; lang = language; tags = language_tags;
    l = list ( str = STRING; tags = language_tags
               { TranslationString (str, tags) }
             | id = UIDENT; tags = language_tags
               { TranslationVariable (id, tags) });
    { Translation (Translation.from_iso639 lang, tags, l) }
  | ADD; lang = language; tags = language_tags
    { Add (Translation.from_iso639 lang, tags) }
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
    AS; v = separated_list (OR, UIDENT)
    { ProvideAttribute {
          attribute_strictness = s ;
          attribute_name = a ;
          attribute_player = p ;
          attribute_value = v
        } }
  | PROVIDE; s = strictness;
    CONTACT; c = UIDENT;
    d = target_destination;
    AS; v = separated_list (OR, UIDENT)
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
  | WITH; ATTRIBUTE; a = UIDENT;
    n = boption (NOT { }); AS; v = separated_list (OR, UIDENT)
    { HasAttribute (a, n, v) }
  | WITH; CONTACT; c = UIDENT;
    TO; p = UIDENT;
    n = boption (NOT { }); AS; v = separated_list (OR, UIDENT)
    { HasContact (c, p, n, v) }

relation: b = boption (STRONG { }); r = relation_content { (r, b) }

relation_content:
  | b = basic_relation
    { Relation.Basic b }
  | ASYMMETRICAL; r1 = relation_content; r2 = relation_content
    { Relation.asymmetrical_relation r1 r2 }
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

