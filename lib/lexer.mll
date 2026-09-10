{
open Parser
open Error 

exception Fault of Error.error

let span lexbuf = Span.of_loc (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
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
  | ['L' 'R'] as c        { raise (Fault (Missing_index { at = span lexbuf; letter = c })) }
  | _ as c                { raise (Fault (Unexpected_character { at = span lexbuf; character = c })) }
