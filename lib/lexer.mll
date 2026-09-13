{
open Parser
open Error 

exception Fault of Error.error

let span lexbuf = Span.of_loc (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
  | [' ' '\t']                  { token lexbuf }
  | '\n'                        { Lexing.new_line lexbuf; token lexbuf }
  | ('l'|'L')(digit+ as i)      { LABEL (int_of_string i) }
  | "(*"                        { comment (span lexbuf) 0 lexbuf; token lexbuf }
  | "->"                        { ARROW }
  | ":="                        { COLONEQUAL }
  | "+"                         { PLUS }
  | "-"                         { MINUS }
  | "HALT"|"halt"               { HALT }
  | (alpha (alpha|digit)*) as s { REGISTER s }
  | ['0'-'9']+ as i             { INTEGER (int_of_string i)}
  | ":"                         { COLON }
  | ","                         { COMMA }
  | eof                         { EOF }

and comment opened depth = parse
  | "*)"  { if depth > 0 then comment opened (depth - 1) lexbuf }
  | "(*"  { comment opened (depth + 1) lexbuf }
  | '\n'  { Lexing.new_line lexbuf; comment opened depth lexbuf }
  | eof   { raise (Fault (Unterminated_comment { at = opened })) }
  | _     { comment opened depth lexbuf }
