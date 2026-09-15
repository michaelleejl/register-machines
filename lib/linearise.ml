open Source
open Error 
let program ({ registers; instrs } : Source.program) : Linear.config =
  match registers, instrs with 
  | [], {label; _}::is -> raise (Fault (No_registers {at = label.at}));
  | _ -> let instr { label; body } =
    Linear.{ label;
             body = match body with
               | SAdd (r, t) -> LAdd (r, t)
               | SSub (r, t, f) -> LSub (r, t, f)
               | SHalt -> LHalt }
  in
  { registers; instrs = List.map instr instrs }
