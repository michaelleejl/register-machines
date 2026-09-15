open Elaborate
open Source
open Target 
open Seq

type _ mode = 
  | Trace: (string iarray * State.t Seq.t) mode 
  | Value: int mode 

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

let run: type a. a mode -> Source.config -> int option -> int iarray -> (a, Error.error list) result = 
fun mode prog bound override ->
    match elaborate prog override with 
    | Error errors -> Error errors 
    | Ok(TCfg (values, names, instrs)) -> 
      let states = eval values instrs in
        let states = match bound with None -> states | Some b -> Seq.take (b + 1) states in
        match mode with
        | Trace -> Ok (names, states)
        | Value ->
            Ok (Seq.fold_left (fun _ (s : State.t) -> Iarray.get s.registers 0)
                  (Iarray.get values 0) states)

let interpret bound verbose file override =
  let source = In_channel.with_open_text file In_channel.input_all in
  let buffer = Lexing.from_string source in
  Lexing.set_filename buffer file;
  let fail errors =
    List.iter (fun e -> prerr_string (Report.render ~source e); prerr_newline ()) errors;
    exit 1
  in
  match Parser.main Lexer.token buffer with
  | prog -> (
      if verbose then 
        (match run Trace prog bound override with
        | Ok (names, traced) ->
            if verbose then Table.all names traced |> 
                            Table.to_string |>
                            print_string
            else (
              match Seq.fold_left (fun _ s -> Some s) None traced with
              | None -> ()
              | Some (s : State.t) -> Printf.printf "%d\n" (Iarray.get s.registers 0))
        | Error errors -> fail errors)
      else 
        match run Value prog bound override with 
        | Ok (v) -> Printf.printf "%d\n" v 
        | Error errors -> fail errors
    )
  | exception Lexer.Fault error -> fail [ error ]
  | exception Parser.Error ->
      fail [ Error.Syntax_error { at = Span.of_loc (buffer.lex_start_p, buffer.lex_curr_p) } ]
