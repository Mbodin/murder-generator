{ (** Module Lexer. **)

  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos ; pos_lnum = 1 + pos.pos_lnum }

  let current_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    (if pos.pos_fname <> "" then "file `" ^ pos.pos_fname ^ "', " else "")
    ^ "line " ^ string_of_int pos.pos_lnum ^ ", "
    ^ "character " ^ string_of_int (pos.pos_cnum - pos.pos_bol + 1)
}

let space = ' ' | '\t'
let newline = '\n' | '\r' | "\r\n"

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

let ident = (letter | digit | '_')*

let strctn = [^ '\n' '\r' '"']*

rule read = parse

  | "begin"                 { BEGIN }
  | "end"                   { END }
  
  | "category"              { CATEGORY }
  | "element"               { ELEMENT }
  | "attribute"             { ATTRIBUTE }
  | "contact"               { CONTACT }
  | "relation"              { RELATION }
  | "player"                { PLAYER }
  | "event"                 { EVENT }

  | "internal"              { INTERNAL }

  | "any"                   { ANY }
  | "other"                 { OTHER }

  | "declare"               { DECLARE }
  | "provide"               { PROVIDE }
  | "let"                   { LET }
  | "be"                    { BE }
  | "assume"                { ASSUME }
  | "add"                   { ADD }
  | "remove"                { REMOVE }

  | "with"                  { WITH }
  | "and"                   { AND }
  | "or"                    { OR }
  | "not"                   { NOT }
  | "no "                   { NO }
  | "from"                  { FROM }
  | "to"                    { TO }
  | "between"               { BETWEEN }
  | "as"                    { AS }

  | "strict"                { STRICT }
  | "compatible"            { COMPATIBLE }

  | "duplicable"            { DUPLICABLE }
  | "unique"                { UNIQUE }

  | "phantom"               { PHANTOM }
  | "blocking"              { BLOCKING }

  | "difficulty"            { DIFFICULTY }
  | "complexity"            { COMPLEXITY }

  | "neutral"               { NEUTRAL }
  | "hate"                  { HATE }
  | "trust"                 { TRUST }
  | "chaotic"               { CHAOTIC }
  | "undetermined"          { UNDETERMINED }
  | "avoidance"             { AVOIDANCE }
  | "asymmetrical"          { ASYMMETRICAL }
  | "explosive"             { EXPLOSIVE }
  | "strong"                { STRONG }

  | "sentence"              { SENTENCE }
  | "translation"           { TRANSLATION }
  | "description"           { DESCRIPTION }

  | ':'                     { COLON }
  | "+"                     { PLUS }
  | "-"                     { MINUS }
  | '"' (strctn as str) '"' { STRING str }

  | "before"                { BEFORE }
  | "after"                 { AFTER }
  | "providing"             { PROVIDING }

  | "lasting"               { LASTING }
  | "seconds"               { SECONDS }
  | "minutes"               { MINUTES }
  | "days"                  { DAYS }
  | "weeks"                 { WEEKS }
  | "years"                 { YEARS }
  | "decades"               { DECADES }
  | "immediate"             { IMMEDIATE }
  | "very"                  { VERY }
  | "short"                 { SHORT }
  | "medium"                { MEDIUM }
  | "long"                  { LONG }
  | "life"                  { LIFE }

  | (['a'-'z'] ident) as id { LIDENT id }
  | (['A'-'Z'] ident) as id { UIDENT id }

  | space+                  { read lexbuf }
  | "(*"                    { comment lexbuf ; read lexbuf }

  | newline                 { next_line lexbuf ; read lexbuf }
  | eof                     { EOF }
  | _                       { raise (SyntaxError ("Unexpected char: `" ^
                                      lexeme lexbuf ^ "'.")) }

and comment = parse         

  | "(*"                    { comment lexbuf ; comment lexbuf }
  | "*)"                    { () }

  | newline                 { next_line lexbuf ; comment lexbuf }
  | eof                     { raise (SyntaxError "Incomplete comment") }
  | _                       { comment lexbuf }

