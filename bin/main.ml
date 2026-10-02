open Register_machines
open Cmdliner
open Frontend
open Elaborate
open Backend
open Mode

let verbose =
  let doc = "Enable verbose output." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let bound =
  let doc = "Stop after $(docv) steps instead of running to $(b,halt)." in
  Arg.(value & opt (some int) None & info [ "b"; "bound" ] ~docv:"STEPS" ~doc)

let encode =
  let doc = "Encode the program and the register list as a pair of numbers." in
  Arg.(value & flag & info [ "e"; "encode" ] ~doc)

let file = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")

let output bound verbose encode program =
  if encode then
    let registers, instructions = Encode.program program in
    Printf.printf "Registers:%d\nInstructions:%d\n" registers instructions
  else if verbose then
    let machine, traced = Interpret.run Trace program bound in
    Table.all machine traced |> Table.to_string |> print_string
  else Printf.printf "%d\n" (Interpret.run Value program bound)

let run bound verbose encode file =
  let source = In_channel.with_open_text file In_channel.input_all in
  try
    Parse.program file source |> Check.check |> Expand.program
    |> Initialise.program |> Lift.program |> Flatten.program |> Desugar.program
    |> Resolve.program
    |> output bound verbose encode
  with e -> (
    match Report.try_run e with
    | Some report ->
        prerr_string (Report.render ~source report);
        prerr_newline ();
        exit 1
    | None -> raise e)

let cmd =
  let info = Cmd.info "urm" ~version:"0.1.0" in
  Cmd.v info Term.(const run $ bound $ verbose $ encode $ file)

let () = exit (Cmd.eval cmd)
