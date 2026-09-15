%token <int> INTEGER 
%token <string> IDENT
%token ARROW
%token COLONEQUAL
%token PLUS
%token MINUS
%token HALT
%token COLON
%token COMMA
%token EOF

%start <Linear.config> main

%%

main:
  | ds = declrs ; is = instrs ; EOF { Linear.{ registers = List.rev ds; instrs = is } }

declrs:
  | d = declr { [ d ] }
  | ds = declrs ; d = declr { d :: ds }

declr:
  | r=IDENT ; COLONEQUAL ; v= INTEGER
    { Linear.{ name = located $loc(r) r; value = v } }

instrs:
  | nonempty_list(instr) {$1}

instr:
  | l=IDENT ; COLON ; r=IDENT ; PLUS; ARROW; t=IDENT
      { Linear.{ label = located $loc(l) l;
                 body = LAdd (located $loc(r) r, located $loc(t) t) } }
  | l=IDENT ; COLON ; r=IDENT ; MINUS; ARROW; t=IDENT ; COMMA; f=IDENT
      { Linear.{ label = located $loc(l) l;
                 body = LSub (located $loc(r) r, located $loc(t) t, located $loc(f) f) } }
  | l=IDENT ; COLON ; HALT
      { Linear.{ label = located $loc(l) l; body = LHalt } }
