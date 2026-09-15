%token <int> INTEGER 
%token <string> REGISTER 
%token <int> LABEL 
%token ARROW 
%token COLONEQUAL
%token PLUS 
%token MINUS 
%token HALT 
%token COLON
%token COMMA
%token EOF 

%start <Source.config> main 

%%

main:
  | ds = declrs ; is = instrs ; EOF { Source.{ registers = ds; instrs = is } }

declrs:
  | nonempty_list(declr) {$1}

declr:
  | r=REGISTER ; COLONEQUAL ; v= INTEGER
    { Source.{ name = located $loc(r) r; value = v } }

instrs:
  | nonempty_list(instr) {$1}

instr:
  | l=LABEL ; COLON ; r=REGISTER ; PLUS; ARROW; t=LABEL
      { Source.{ label = located $loc(l) l;
                 body = SAdd (located $loc(r) r, located $loc(t) t) } }
  | l=LABEL ; COLON ; r=REGISTER ; MINUS; ARROW; t=LABEL ; COMMA; f=LABEL
      { Source.{ label = located $loc(l) l;
                 body = SSub (located $loc(r) r, located $loc(t) t, located $loc(f) f) } }
  | l=LABEL ; COLON ; HALT
      { Source.{ label = located $loc(l) l; body = SHalt } }
