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
%token REG
%token EOF

%start <Source.program> main

%%

main:
  | ms = list(machine) ; ds = declarations ; is = instructions ; EOF
    { let registers =
        List.map
          (fun (name, value) ->
             Definitional.{ name; value = match value with None -> 0 | Some v -> v.v })
          ds
      in
      Source.{ machines = ms; registers; instructions = is } }

machine:
  | MACHINE ; n = NAME ; LPAREN ; ps = separated_list(COMMA, located(IDENT)) ; RPAREN ;
    EQUAL ; STRUCT ; ms = list(machine) ; ds = declarations ; is = instructions ; END
    { let registers =
        List.map
          (fun (name, value) ->
             match value with
             | None -> name
             | Some v ->
               raise (Error.Fault (Error.Initialised_in_machine { at = v.at; name = name.v })))
          ds
      in
      Source.{ name = located $loc(n) n;
               parameters = ps;
               body = { machines = ms; registers; instructions = is } } }

declarations:
  | { [] }
  | ds = declarations ; REG ; xs = separated_nonempty_list(COMMA, declaration) { ds @ xs }

declaration:
  | r = located(IDENT) { (r, None) }
  | r = located(IDENT) ; c = COLONEQUAL ; v = INTEGER
    { (r, Some (located ($startpos(c), $endpos(v)) v)) }

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
