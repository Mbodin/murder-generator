{ (** Module Lexer. **)
  open Token
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
  | "contact"               { CONTACT }
  | "relation"              { RELATION }
  | "player"                { PLAYER }
  | "event"                 { EVENT }

  | "all"                   { ALL }

  | "declare"               { DECLARE }
  | "provide"               { PROVIDE }
  | "let"                   { LET }
  | "be"                    { BE }

  | "with"                  { WITH }
  | "and"                   { AND }
  | "or"                    { OR }
  | "from"                  { FROM }
  | "to"                    { TO }
  | "between"               { BETWEEN }
  | "as"                    { AS }

  | "strict"                { STRICT }
  | "compatible"            { COMPATIBLE }

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

  | "translation"           { TRANSLATION }
  | "add"                   { ADD }
  | ':'                     { COLON }
  | '"' (strctn as str) '"' { STRING str }

  | space+                  { lex lexbuf }
  | "(*"                    { comment lexbuf ; lex lexbuf }

  | _                       { raise (SyntaxError ("Unexpected char: " ^
                                      Lexing.lexeme lexbuf)) }
  | eof                     { EOF }

and comment = parse         

  | "(*"                    { comment lexbuf ; comment lexbuf }
  | "*)"                    { () }

  | eof                     { raise (SyntaxError "Incomplete comment") }
  | _                       { raise (SyntaxError ("Unexpected char: " ^
                                      Lexing.lexeme lexbuf)) }

