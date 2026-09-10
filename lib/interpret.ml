open Elaborate
open Source
open Target 

open Seq 
let update r f registers =
    Iarray.mapi (fun i v -> if i = r then f v else v) registers

let step instrs registers j =
  match Iarray.get instrs j with
  | THalt -> None
  | TAdd (r, l) -> Some (update r succ registers, l)
  | TSub (r, l, l') ->
    if Iarray.get registers r = 0
    then Some (registers, l')
    else Some (update r pred registers, l)

let eval initial instrs =
  let rec go registers j () =
    Cons (State.{ label = j; registers },
          match step instrs registers j with
          | None -> (fun () -> Nil)
          | Some (registers', j') -> go registers' j')
  in
  go initial 0

let run prog bound =
  elaborate prog
  |> Result.map (fun (TCfg (registers, instrs)) ->
       match bound with
       | None -> eval registers instrs
       | Some b -> eval registers instrs |> take b)

let interpret bound verbose file =
  let source = In_channel.with_open_text file In_channel.input_all in
  let buffer = Lexing.from_string source in
  Lexing.set_filename buffer file;
  let fail errors =
    List.iter (fun e -> prerr_string (Report.render ~source e); prerr_newline ()) errors;
    exit 1
  in
  match Parser.main Lexer.token buffer with
  | prog -> (
      match run prog bound with
      | Ok traced ->
          let table = if verbose then Table.all traced
          else Table.last traced in
          print_string (Table.to_string table)
      | Error errors -> fail errors)
  | exception Lexer.Fault error -> fail [ error ]
  | exception Parser.Error ->
      fail [ Error.Syntax_error { at = Span.of_loc (buffer.lex_start_p, buffer.lex_curr_p) } ]
