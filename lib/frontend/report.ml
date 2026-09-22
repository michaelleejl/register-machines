type message = {
  at : Text.Span.t;
  text : string;
}

type t = {
  summary : string;
  label : message;
  notes : message list;
}

let handlers : (exn -> t option) list ref = ref []

let register handler = handlers := handler :: !handlers

let try_run e = 
  List.find_map (fun handler -> handler e) !handlers

let where (p : Lexing.position) =
  Printf.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol + 1)

let line_at source (p : Lexing.position) =
  let stop =
    Option.value (String.index_from_opt source p.pos_bol '\n')
      ~default:(String.length source)
  in
  String.sub source p.pos_bol (stop - p.pos_bol)

let block ~source ~severity ~summary ~label (span : Text.Span.t) =
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
      Printf.sprintf "%s--> %s" gutter (where span.start);
      Printf.sprintf "%s |" gutter;
      Printf.sprintf "%s | %s" number text;
      Printf.sprintf "%s | %s" gutter (if label = "" then caret else caret ^ " " ^ label) ]

let render ~source { summary; label; notes } =
  let note { at; text } = block ~source ~severity:"note" ~summary:text ~label:"" at in
  String.concat "\n\n"
    (block ~source ~severity:"error" ~summary ~label:label.text label.at
     :: List.map note notes)
  ^ "\n"
