%{ (** Module Token **)
   (** Lists all tokens used by the lexer. **) %}

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

