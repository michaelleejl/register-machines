type error =
  | Unexpected_character of { at : Span.t; character : char }
  | Syntax_error of { at : Span.t }
  | Missing_index of { at : Span.t; letter : char }
  | Duplicate_register of { at : Span.t; first : Span.t; name : string }
  | Undeclared_register of { at : Span.t; name : string; declared : string list }
  | Mislabelled of { at : Span.t; written : int; expected : int }
  | Undefined_label of { at : Span.t; target : int; count : int }
  | Unterminated_comment of { at : Span.t; }

exception Fault of error

let at = function
  | Unexpected_character { at; _ } | Syntax_error { at } 
  | Missing_index { at; _ } | Duplicate_register { at; _ }
  | Undeclared_register { at; _ } | Mislabelled { at; _ } 
  | Undefined_label { at; _ } | Unterminated_comment {at; _} -> at
let where (p : Lexing.position) =
  Printf.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol + 1)

let message = function
  | Unexpected_character { character; _ } ->
    Printf.sprintf "unrecognised character %C" character
  | Syntax_error _ -> "this token is out of place"
  | Missing_index { letter; _ } ->
    Printf.sprintf "%c must be followed by a number" letter
  | Duplicate_register { name; _ } ->
    Printf.sprintf "%s is given an initial value twice" name
  | Undeclared_register { name; _ } -> Printf.sprintf "%s is not a register" name
  | Mislabelled { written; expected; _ } ->
    Printf.sprintf "this instruction is L%d, but it is labelled L%d" expected written
  | Undefined_label { target; _ } -> Printf.sprintf "L%d is not an instruction" target
  | Unterminated_comment _ -> "this comment is never closed"

let label = function
  | Unexpected_character _ -> "remove it"
  | Syntax_error _ -> "not expected here"
  | Missing_index { letter; _ } -> Printf.sprintf "write %c0, %c1, and so on" letter letter
  | Duplicate_register _ -> "given again here"
  | Undeclared_register { declared; _ } ->
    Printf.sprintf "this machine has registers: %s" (String.concat ", " declared)
  | Mislabelled { expected; _ } -> Printf.sprintf "expected L%d" expected
  | Undefined_label { count; _ } ->
    Printf.sprintf "this machine has L0 to L%d" (count - 1)
  | Unterminated_comment _ -> "opened here"

let note = function
  | Duplicate_register { first; _ } -> Some ("first given here", first)
  | _ -> None
