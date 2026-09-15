{
open Parser
open Error

let span lexbuf = Span.of_loc (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
}

let lower = ['a'-'z']
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
  | "halt"                      { HALT }
  | (lower (alpha|digit)*) as s { IDENT s }
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
