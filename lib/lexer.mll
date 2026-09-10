{
open Parser 

exception Error of Span.t * char
}

rule token = parse
  | [' ' '\t']            { token lexbuf }
  | '\n'                  { Lexing.new_line lexbuf; token lexbuf }
  | 'L'(['0'-'9']+ as i)  { LABEL (int_of_string i) }
  | 'R'(['0'-'9']+ as i)  { REGISTER (int_of_string i)}
  | "->"                  { ARROW }
  | "="                   { EQUALS }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "HALT"                { HALT }
  | ['0'-'9']+ as i       { INTEGER (int_of_string i)}
  | ":"                   { COLON }
  | ","                   { COMMA }
  | eof                   { EOF }
  | _ as c                { raise (Error (Span.of_loc (lexbuf.lex_start_p, lexbuf.lex_curr_p), c)) }
