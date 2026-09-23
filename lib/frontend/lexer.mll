{
open Parser
open Syntax
let span lexbuf = Text.Span.of_loc (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
  | [' ' '\t']                  { token lexbuf }
  | '\n'                        { Lexing.new_line lexbuf; token lexbuf }
  | "(*"                        { comment (span lexbuf) 0 lexbuf; token lexbuf }
  | "->"                        { ARROW }
  | ":="                        { COLONEQUAL }
  | "+"                         { PLUS }
  | "-"                         { MINUS }
  | ";"                         { SEMICOLON }
  | "if"                        { IF }
  | "then"                      { THEN }
  | "else"                      { ELSE }
  | "while"                     { WHILE }
  | "do"                        { DO }
  | "exit"                      { EXIT }
  | "halt"                      { HALT }
  | "machine"                   { MACHINE }
  | "execute"                   { EXECUTE }
  | "struct"                    { STRUCT }
  | "end"                       { END }
  | "done"                      { DONE }
  | "reg"                       { REG }
  | "clear"                     { CLEAR }
  | "jump"                      { JUMP }
  | (lower (alpha|digit)*) as s { IDENT s }
  | (upper (alpha|digit)*) as s { NAME s }
  | ['0'-'9']+ as i             { INTEGER (int_of_string i)}
  | ":"                         { COLON }
  | ","                         { COMMA }
  | "="                         { EQUAL }
  | "("                         { LPAREN }
  | ")"                         { RPAREN }
  | eof                         { EOF }
  | _ as c                      { raise (Error (Unexpected_character { at = span lexbuf; character = c })) }

and comment opened depth = parse
  | "*)"  { if depth > 0 then comment opened (depth - 1) lexbuf }
  | "(*"  { comment opened (depth + 1) lexbuf }
  | '\n'  { Lexing.new_line lexbuf; comment opened depth lexbuf }
  | eof   { raise (Error (Unterminated_comment { at = opened })) }
  | _     { comment opened depth lexbuf }
