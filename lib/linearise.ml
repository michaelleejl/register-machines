open Source
open Error 
let program ({ registers; instructions } : Source.program) : Definitional.config =
  match registers, instructions with 
  | [], {label; _}::_ -> raise (Fault (No_registers {at = label.at}));
  | _ -> let instruction { label; body } =
    Definitional.{ label;
                   body = match body with
                     | SAdd (r, t) -> DAdd (r, t)
                     | SSub (r, t, f) -> DSub (r, t, f)
                     | SHalt -> DHalt }
  in
  { registers; instructions = List.map instruction instructions }
