open Error

let program filename source = 
  let buffer = Lexing.from_string source in
  try 
    Lexing.set_filename buffer filename;
    Parser.main Lexer.token buffer 
  with  
    Parser.Error ->
      raise (Fault (Syntax_error { at = Span.of_loc (buffer.lex_start_p, buffer.lex_curr_p) }))

