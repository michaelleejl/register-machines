let line_at source (p : Lexing.position) =
  let stop =
    Option.value (String.index_from_opt source p.pos_bol '\n')
      ~default:(String.length source)
  in
  String.sub source p.pos_bol (stop - p.pos_bol)

let block ~source ~severity ~summary ~label (span : Span.t) =
  let text = line_at source span.start in
  let number = string_of_int span.start.pos_lnum in
  let gutter = String.make (String.length number) ' ' in
  let column = span.start.pos_cnum - span.start.pos_bol in
  let width =
    max 1 (min (String.length text - column) (span.stop.pos_cnum - span.start.pos_cnum))
  in
  let caret = String.make column ' ' ^ String.make width '^' in
  String.concat "\n"
    [ Printf.sprintf "%s: %s" severity summary;
      Printf.sprintf "%s--> %s" gutter (Error.where span.start);
      Printf.sprintf "%s |" gutter;
      Printf.sprintf "%s | %s" number text;
      Printf.sprintf "%s | %s" gutter (if label = "" then caret else caret ^ " " ^ label) ]

(** One fault as a summary, the line it is on, and a caret under the words. *)
let render ~source error =
  let primary =
    block ~source ~severity:"error" ~summary:(Error.message error)
      ~label:(Error.label error) (Error.at error)
  in
  match Error.note error with
  | None -> primary ^ "\n"
  | Some (summary, span) ->
    primary ^ "\n\n" ^ block ~source ~severity:"note" ~summary ~label:"" span ^ "\n"
