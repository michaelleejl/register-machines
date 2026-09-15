open Source
open Error 
let program ({ registers; instrs } : Source.program) : Definitional.config =
  match registers, instrs with 
  | [], {label; _}::_ -> raise (Fault (No_registers {at = label.at}));
  | _ -> let instr { label; body } =
    Definitional.{ label;
                   body = match body with
                     | SAdd (r, t) -> DAdd (r, t)
                     | SSub (r, t, f) -> DSub (r, t, f)
                     | SHalt -> DHalt }
  in
  { registers; instrs = List.map instr instrs }
