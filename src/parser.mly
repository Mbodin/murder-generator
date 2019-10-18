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
%token          CATEGORY ELEMENT EVENT
%token          ATTRIBUTE CONTACT RELATION
%token          PLAYER
%token          DECLARE PROVIDE LET BE ASSUME ADD REMOVE
%token          INTERNAL
%token          WITH AND OR NOT NO FROM TO BETWEEN AS ANY OTHER
%token          STRICT COMPATIBLE
%token          DUPLICABLE UNIQUE
%token          PHANTOM BLOCKING
%token          DIFFICULTY COMPLEXITY
%token          NEUTRAL HATE TRUST CHAOTIC UNDETERMINED AVOIDANCE
%token          ASYMMETRICAL EXPLOSIVE STRONG
%token<string>  LIDENT UIDENT STRING
%token          SENTENCE TRANSLATION DESCRIPTION
%token          COLON PLUS MINUS
%token          PROVIDING BEFORE AFTER
%token          IMMEDIATE VERY SHORT MEDIUM LONG LIFE
%token          LASTING SECONDS MINUTES DAYS WEEKS YEARS DECADES

%start<Ast.declaration list> main
%start<Relation.t> relation

%%

main: l = list (declaration); EOF { l }

declaration:
  | DECLARE; internal = boption (INTERNAL);
    k = attribute_kind; id = UIDENT; b = block
    { DeclareInstance (k, id, internal, b) }
  | internal = boption (INTERNAL); k = attribute_kind;
    attr = UIDENT; constructor = UIDENT; b = block
    { DeclareConstructor (k, attr, constructor, internal, b) }
  | CATEGORY; name = UIDENT; c = block
    { DeclareCategory (name, c) }
  | s = status; ELEMENT; name = UIDENT; c = block
    { DeclareElement (s, name, c) }
  | DECLARE; TRANSLATION; lang = language; COLON; tag = LIDENT
    { DeclareCase (Translation.from_iso639 lang, Translation.get_tag tag) }
  | DECLARE; EVENT; event = UIDENT; b = block
    { DeclareEventKind (event, b) }

attribute_kind:
  | ATTRIBUTE   { Attribute }
  | CONTACT     { Contact }

block: l = loption (BEGIN; l = list (command); END { l })   { l }

status:
  | empty       { History.Normal }
  | DUPLICABLE  { History.Duplicable }
  | UNIQUE      { History.Unique }

language:
  | lang = LIDENT   { lang }
  (* Any two- or three-characters identifier can be a language. *)
  | AND             { "and" }
  | OR              { "or" }
  | NOT             { "not" }
  | NO              { "no" }
  | ANY             { "any" }
  | END             { "end" }
  | LET             { "let" }
  | BE              { "be" }
  | TO              { "to" }
  | AS              { "as" }
  | ADD             { "add" }

tag_modifier:
    modifier = option ( PLUS   { true }
                      | MINUS  { false })  { modifier }

language_tags:
    tags = list (COLON; modifier = tag_modifier; tag = LIDENT
                 { (modifier, Translation.get_tag tag) })   { tags }

translation_base:
  | empty   { (None, Translation.base) :: [] }
  | ADD     { [] }

command:
  | CATEGORY; c = UIDENT
    { OfCategory c }
  | tb = translation_base;
    TRANSLATION; lang = language; tags = language_tags;
    l = list ( str = STRING
               { Translation.Direct str }
             | id = UIDENT; tags = language_tags
               { Translation.variable id tags })
    { Translation (Translation.from_iso639 lang, tb @ tags, l) }
  | DESCRIPTION; lang = language; str = STRING
    { Description (Translation.from_iso639 lang, str) }
  | SENTENCE; b = block
    { Sentence b }
  | ADD; lang = language; COLON; tag = LIDENT
    { Add (Translation.from_iso639 lang, Translation.get_tag tag) }
  | COMPATIBLE; WITH; v = UIDENT
    { CompatibleWith v }
  | LET; v = UIDENT; BE; PLAYER;
    l = list (player_constraint)
    { LetPlayer (Some v, l) }
  | LET; ANY; OTHER; PLAYER; BE;
    l = nonempty_list (player_constraint)
    { LetPlayer (None, l) }
  | PROVIDE; RELATION;
    d = target_destination (UIDENT);
    AS; r = relation
    { ProvideRelation (d, r) }
  | PROVIDE; s = strictness;
    ATTRIBUTE; a = UIDENT;
    TO; p = destination;
    AS; v = separated_list (OR, UIDENT)
    { ProvideAttribute {
          attribute_strictness = s ;
          attribute_name = a ;
          attribute_player = p ;
          attribute_value = v
        } }
  | PROVIDE; s = strictness;
    CONTACT; c = UIDENT;
    d = target_destination (destination);
    AS; v = separated_list (OR, UIDENT)
    { ProvideContact {
          contact_strictness = s ;
          contact_name = c ;
          contact_destination = d ;
          contact_value = v
        } }
  | d = add_remove; DIFFICULTY; TO; l = separated_list (AND, UIDENT)
    { AddDifficulty (d, l) }
  | d = add_remove; COMPLEXITY; TO; l = separated_list (AND, UIDENT)
    { AddComplexity (d, l) }
  | EVENT; event = UIDENT
    { EventKind event }
  | PROVIDE; t = time;
    bl = boption (BLOCKING { }); ph = boption (PHANTOM { }); EVENT;
    TO; targets = separated_list (AND, UIDENT);
    b = block
    { ProvideEvent (bl, ph, t, targets, b) }
  | PROVIDE; bl = boption (BLOCKING { }); ph = boption (PHANTOM { }); EVENT;
    LASTING; t = duration;
    TO; targets = separated_list (AND, UIDENT);
    b = block
    { ProvideEvent (bl, ph, t, targets, b) }
  | e = event_constraint (empty, AND)
    { e true }
  | e = event_constraint (NO, OR)
    { e false }

add_remove:
  | ADD     { true }
  | REMOVE  { true }

target_destination (player):
  | FROM; p1 = player; TO; p2 = player
    { FromTo (p1, p2) }
  | BETWEEN; p = player; AND; l = separated_nonempty_list (AND, player)
    { Between (p :: l) }

destination:
  | p = UIDENT          { DestinationPlayer p }
  | ANY; OTHER; PLAYER  { AllOtherPlayers }
  | ANY; PLAYER         { AllPlayers }

player_constraint:
  | WITH; ATTRIBUTE; a = UIDENT;
    n = boption (NOT { }); AS; v = separated_list (OR, UIDENT)
    { HasAttribute (a, n, v) }
  | WITH; CONTACT; c = UIDENT;
    TO; p = UIDENT;
    n = boption (NOT { }); AS; v = separated_list (OR, UIDENT)
    { HasContact (c, Some p, n, v) }
  | WITH; CONTACT; c = UIDENT;
    TO; ANY; OTHER; PLAYER;
    n = boption (NOT { }); AS; v = separated_list (OR, UIDENT)
    { HasContact (c, None, n, v) }

relation: b = boption (STRONG { }); r = relation_content { (r, b) }

relation_content:
  | b = basic_relation
    { Relation.Basic b }
  | ASYMMETRICAL; r1 = relation_content; r2 = relation_content
    { Relation.asymmetrical_relation r1 r2 }
  | EXPLOSIVE; r1 = relation_content; r2 = relation_content
    { Relation.Explosive (r1, r2) }

basic_relation:
  | NEUTRAL         { Relation.Neutral }
  | HATE            { Relation.Hate }
  | TRUST           { Relation.Trust }
  | CHAOTIC         { Relation.Chaotic }
  | UNDETERMINED    { Relation.Undetermined }
  | AVOIDANCE       { Relation.Avoidance }

strictness:
  | COMPATIBLE  { State.NonStrict }
  | empty       { State.LowStrict }
  | STRICT      { State.Strict }

event_direction:
  | AFTER   { true }
  | BEFORE  { false }

event_constraint (N, O):
    ASSUME; N; EVENT; kp = kind_players (O);
    d = event_direction
    { fun any ->
        EventConstraint {
          constraint_kind = fst kp ;
          constraint_players = snd kp ;
          constraint_after = d ;
          constraint_any = any
        } }

kind_players (O):
  | event = UIDENT; TO; players = separated_list (O, UIDENT)
    { (Ast.Kind event, players) }
  | PROVIDING; ATTRIBUTE; a = UIDENT; TO; players = separated_list (O, UIDENT)
    { (Ast.KindAttribute a, players) }
  | PROVIDING; CONTACT; c = UIDENT;
    FROM; players = separated_list (O, UIDENT);
    TO; targets = separated_list (O, UIDENT)
    { (Ast.KindContact (c, targets), players) }

time:
  | LIFE        { Events.For_life_event }
  | VERY; LONG  { Events.For_life_event }
  | LONG        { Events.Long_term_event }
  | MEDIUM      { Events.Medium_term_event }
  | SHORT       { Events.Short_term_event }
  | VERY; SHORT { Events.Very_short_term_event }
  | IMMEDIATE   { Events.Immediate_event }

duration:
  | DECADES     { Events.For_life_event }
  | YEARS       { Events.Long_term_event }
  | WEEKS       { Events.Medium_term_event }
  | DAYS        { Events.Short_term_event }
  | MINUTES     { Events.Very_short_term_event }
  | SECONDS     { Events.Immediate_event }

%inline empty: { () }

