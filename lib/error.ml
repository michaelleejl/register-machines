type kind = Register | Label | Machine

type error =
  | Unexpected_character of { at : Span.t; character : char }
  | Syntax_error of { at : Span.t }
  | Missing_index of { at : Span.t; letter : char }
  | Duplicate of { kind : kind; at : Span.t; first : Span.t; name : string }
  | Undeclared of { kind : kind; at : Span.t; name : string; declared : string list }
  | Unterminated_comment of { at : Span.t; }
  | No_registers of { at : Span.t }
  | Wrong_arity of { at : Span.t; machine : string; expected : int; given : int }
  | Repeated_argument of { at : Span.t; first : Span.t; name : string }

exception Fault of error

let at = function
  | Unexpected_character { at; _ } | Syntax_error { at }
  | Missing_index { at; _ } | Duplicate { at; _ }
  | Undeclared { at; _ } | Unterminated_comment {at; _}
  | No_registers { at } | Wrong_arity { at; _ }
  | Repeated_argument { at; _ } -> at
let where (p : Lexing.position) =
  Printf.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol + 1)

let message = function
  | Unexpected_character { character; _ } ->
    Printf.sprintf "unrecognised character %C" character
  | Syntax_error _ -> "this token is out of place"
  | Missing_index { letter; _ } ->
    Printf.sprintf "%c must be followed by a number" letter
  | Duplicate { kind = Register; name; _ } ->
    Printf.sprintf "%s is declared twice" name
  | Duplicate { kind = Label; name; _ } -> Printf.sprintf "%s labels two instructions" name
  | Duplicate { kind = Machine; name; _ } -> Printf.sprintf "%s names two machines" name
  | Undeclared { kind = Register; name; _ } -> Printf.sprintf "%s is not a register" name
  | Undeclared { kind = Label; name; _ } -> Printf.sprintf "%s is not a label" name
  | Undeclared { kind = Machine; name; _ } -> Printf.sprintf "%s is not a machine" name
  | Unterminated_comment _ -> "this comment is never closed"
  | No_registers _ -> "this machine has no registers"
  | Wrong_arity { machine; expected; given; _ } ->
    Printf.sprintf "%s takes %d registers, not %d" machine expected given
  | Repeated_argument { name; _ } -> Printf.sprintf "%s is passed twice" name

let label = function
  | Unexpected_character _ -> "remove it"
  | Syntax_error _ -> "not expected here"
  | Missing_index { letter; _ } -> Printf.sprintf "write %c0, %c1, and so on" letter letter
  | Duplicate { kind = Register; _ } -> "declared again here"
  | Duplicate { kind = Label; _ } -> "used again here"
  | Duplicate { kind = Machine; _ } -> "declared again here"
  | Undeclared { kind = Register; declared; _ } ->
    Printf.sprintf "this machine has registers: %s" (String.concat ", " declared)
  | Undeclared { kind = Label; declared; _ } ->
    Printf.sprintf "this machine has labels: %s" (String.concat ", " declared)
  | Undeclared { kind = Machine; declared; _ } ->
    Printf.sprintf "these machines are in scope: %s" (String.concat ", " declared)
  | Unterminated_comment _ -> "opened here"
  | No_registers _ -> "declare a register above this instruction"
  | Wrong_arity { given; _ } -> Printf.sprintf "%d given here" given
  | Repeated_argument _ -> "passed again here"

let note = function
  | Duplicate { kind = Register; first; _ } -> Some ("first declared here", first)
  | Duplicate { kind = Label; first; _ } -> Some ("first used here", first)
  | Duplicate { kind = Machine; first; _ } -> Some ("first declared here", first)
  | Repeated_argument { first; _ } -> Some ("first passed here", first)
  | _ -> None
