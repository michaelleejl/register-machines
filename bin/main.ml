open Register_machines
open Cmdliner 

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
  Interpret.interpret bound verbose file (Iarray.of_list override)

let cmd = 
  let info = Cmd.info "urm" ~version:"0.1.0" in
  Cmd.v info Term.(const run $ bound $ verbose $ file $ override)

let () = exit (Cmd.eval cmd)
