{ (** Module Lexer. **)
  open Parser
  exception SyntaxError of string
}

let space = ' ' | '\t' | '\n'

let letter = ['a'-'z' 'A'-'Z']
let number = ['0'-'9']

let ident = letter (letter | number | '_')*

let strctn = [^ '\n' '"']*

rule lex = parse

  | "begin"                 { BEGIN }
  | "end"                   { END }
  
  | "category"              { CATEGORY }
  | "element"               { ELEMENT }
  | "attribute"             { ATTRIBUTE }
  | "relation"              { RELATION }
  | "contact"               { CONTACT }
  | "player"                { PLAYER }
  | "event"                 { EVENT }

  | "declare"               { DECLARE }
  | "provide"               { PROVIDE }
  | "add"                   { ADD }
  | "let"                   { LET }
  | "be"                    { BE }
  | "with"                  { WITH }
  | "and"                   { AND }
  | "or"                    { OR }
  | "from"                  { FROM }
  | "to"                    { TO }
  | "between"               { BETWEEN }
  | "as"                    { AS }

  | "Neutral"               { NEUTRAL }
  | "Hate"                  { HATE }
  | "Trust"                 { TRUST }
  | "Chaotic"               { CHAOTIC }
  | "Undetermined"          { UNDETERMINED }
  | "Avoidance"             { AVOIDANCE }
  | "Asymmetrical"          { ASYMMETRICAL }
  | "Explosive"             { EXPLOSIVE }
  | "Strong"                { STRONG }

  | ident as id             { IDENT id }

  | ':'                     { COLON }
  | '"' (strctn as str) '"' { STRING str }

  | space+                  { lex lexbuf }
  | "(*"                    { comment lexbuf ; lex lexbuf }

  | _                       { raise (SyntaxError ("Unexpected char: " ^
                                      Lexing.lexeme lexbuf)) }

and comment = parse         

  | "(*"                    { comment lexbuf ; comment lexbuf }
  | "*)"                    { () }

  | eof                     { raise (SyntaxError "Incomplete comment") }
  | _                       { raise (SyntaxError ("Unexpected char: " ^
                                      Lexing.lexeme lexbuf)) }

