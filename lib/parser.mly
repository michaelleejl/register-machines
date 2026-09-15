%{
open Located
type item =
  | Definition of Source.defn
  | Register of Linear.register

let split items = 
  List.fold_left
    (fun (ds, rs) -> function
        | Definition d -> (d :: ds, rs)
        | Register r -> (ds, r :: rs))
    ([], []) items
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
  | items = items; instrs = instrs {
    let definitions, registers = split items in 
    Source.{ definitions; registers; instrs }
  }

items:
  | { [] }
  | is = items ; i = item { i :: is }

item:
  | d = defn { Definition d }
  | r = register { Register r }

defn:
  | MACHINE ; n = NAME ; LPAREN ; ps = separated_list(COMMA, located(IDENT)) ; RPAREN ;
    EQUAL ; STRUCT ; p = program ; END
    { Source.{ name = located $loc(n) n; parameters = ps; program = p } }

register:
  | r = IDENT ; COLONEQUAL ; v = INTEGER
    { Linear.{ name = located $loc(r) r; value = v } }

instrs:
  | nonempty_list(instr) {$1}

instr:
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
