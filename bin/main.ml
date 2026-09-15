open Register_machines
open Cmdliner 
open Mode 

let verbose = 
  let doc = "Enable verbose output." in 
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let bound =
  let doc = "Stop after $(docv) steps instead of running to $(b,halt)." in
  Arg.(value & opt (some int) None & info ["b"; "bound"] ~docv:"STEPS" ~doc)

let file = 
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")

let override = 
  let doc = "Initial value of the $(i,n)th declared register, overriding the value in $(b,FILE)." in 
  Arg.(value & pos_right 0 int [] & info [] ~docv:"VALUE" ~doc)

let run bound verbose file override =
  let override = Iarray.of_list override in
  let source = In_channel.with_open_text file In_channel.input_all in
  let fail error =
    prerr_string (Report.render ~source error); prerr_newline ();
    exit 1
  in
  try
    let prog = Linearise.program (Parse.program file source) in
      if verbose then
        let (machine, traced) = Interpret.run Trace prog bound override in
          Table.all machine traced |>
          Table.to_string |>
          print_string
      else
        Printf.printf "%d\n" (Interpret.run Value prog bound override)
  with
  | Error.Fault error -> fail error 
  
let cmd = 
  let info = Cmd.info "urm" ~version:"0.1.0" in
  Cmd.v info Term.(const run $ bound $ verbose $ file $ override)

let () = exit (Cmd.eval cmd)
