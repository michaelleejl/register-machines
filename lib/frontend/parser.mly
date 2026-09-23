%{
open Text.Located
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
%token EXECUTE
%token CLEAR
%token JUMP
%token STRUCT
%token END
%token DONE
%token EQUAL
%token LPAREN
%token RPAREN
%token REG
%token SEMICOLON
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token EOF

%right SEMICOLON

%start <Lang.Source.program> main

%%

main:
  | ms = list(machine) ; ds = declarations ; is = instructions ; EOF
    { let registers =
        List.map
          (fun (name, value) ->
             Lang.Source.{ name; value = match value with None -> 0 | Some v -> v.v })
          ds
      in
      Lang.Source.{ machines = ms; registers; instructions = is } }

machine:
  | MACHINE ; n = NAME ; LPAREN ; ps = separated_list(COMMA, located(IDENT)) ; RPAREN ;
    EQUAL ; d = machine_expr
    { Lang.Source.{ name = located $loc(n) n; parameters = ps; definition = d } }

machine_expr:
  | e = machine_expr ; SEMICOLON ; f = machine_expr { Lang.Source.SSeq (e, f) }
  | STRUCT ; ms = list(machine) ; ds = declarations ; is = instructions ; END
    { let registers =
        List.map
          (fun (name, value) ->
             match value with
             | None -> name
             | Some v ->
               raise (Syntax.Error (Syntax.Initialised_in_machine { at = v.at; name = name.v })))
          ds
      in
      Lang.Source.SStruct { machines = ms; registers; instructions = is } }
  | n = NAME ; LPAREN ; args = separated_list(COMMA, located(IDENT)) ; RPAREN
    { Lang.Source.SApply { name = located $loc(n) n; arguments = args } }
  | IF ; r = located(IDENT) ; THEN ; t = machine_expr ; ELSE ; f = machine_expr ; END
    { Lang.Source.SIf (r, t, f) }
  | WHILE ; r = located(IDENT) ; DO ; e = machine_expr ; DONE
    { Lang.Source.SWhile (r, e) }

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
      { Lang.Source.{ label = located $loc(l) l;
                 body = SAdd (located $loc(r) r, located $loc(t) t) } }
  | l=IDENT ; COLON ; r=IDENT ; MINUS; ARROW; t=IDENT ; COMMA; f=IDENT
      { Lang.Source.{ label = located $loc(l) l;
                 body = SSub (located $loc(r) r, located $loc(t) t, located $loc(f) f) } }
  | l=IDENT ; COLON ; HALT
      { Lang.Source.{ label = located $loc(l) l; body = SHalt } }
  | l=IDENT ; COLON ; EXECUTE ; m=NAME ;
    LPAREN ; args = separated_list(COMMA, located(IDENT)) ; RPAREN ; ARROW ; k=IDENT
      { Lang.Source.{ label = located $loc(l) l;
                 body = SExecute { machine = located $loc(m) m;
                                   arguments = args;
                                   next = located $loc(k) k } } }
  | l=IDENT ; COLON ; CLEAR ; r=IDENT ; ARROW ; k=IDENT
      { Lang.Source.{ label = located $loc(l) l;
                 body = SClear (located $loc(r) r, located $loc(k) k) } }
  | l=IDENT ; COLON ; JUMP ; ARROW ; k=IDENT
      { Lang.Source.{ label = located $loc(l) l; body = SJump (located $loc(k) k) } }
                                   
located(X):
  | x = X { located $loc x }
