%{
open Located
%}

%token <int> INTEGER 
%token <string> IDENT
%token <string> NAME
%token ARROW
%token COLONEQUAL
%token PLUS
%token MINUS
%token HALT
%token COLON
%token COMMA
%token MACHINE
%token STRUCT
%token END
%token EQUAL
%token LPAREN
%token RPAREN
%token EOF

%start <Source.program> main

%%

main:
  | p = program; EOF { p }

program:
  | ms = list(machine) ; rs = registers ; is = instructions
    { Source.{ machines = ms; registers = List.rev rs; instructions = is } }

registers:
  | { [] }
  | rs = registers ; r = register { r :: rs }

machine:
  | MACHINE ; n = NAME ; LPAREN ; ps = separated_list(COMMA, located(IDENT)) ; RPAREN ;
    EQUAL ; STRUCT ; p = program ; END
    { Source.{ name = located $loc(n) n; parameters = ps; program = p } }

register:
  | r = IDENT ; COLONEQUAL ; v = INTEGER
    { Definitional.{ name = located $loc(r) r; value = v } }

instructions:
  | nonempty_list(instruction) {$1}

instruction:
  | l=IDENT ; COLON ; r=IDENT ; PLUS; ARROW; t=IDENT
      { Source.{ label = located $loc(l) l;
                 body = SAdd (located $loc(r) r, located $loc(t) t) } }
  | l=IDENT ; COLON ; r=IDENT ; MINUS; ARROW; t=IDENT ; COMMA; f=IDENT
      { Source.{ label = located $loc(l) l;
                 body = SSub (located $loc(r) r, located $loc(t) t, located $loc(f) f) } }
  | l=IDENT ; COLON ; HALT
      { Source.{ label = located $loc(l) l; body = SHalt } }

located(X):
  | x = X { located $loc x }
