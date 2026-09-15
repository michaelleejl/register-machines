open Source

let program ({ registers; instrs } : Source.program) : Linear.config =
  let instr { label; body } =
    Linear.{ label;
             body = match body with
               | SAdd (r, t) -> LAdd (r, t)
               | SSub (r, t, f) -> LSub (r, t, f)
               | SHalt -> LHalt }
  in
  { registers; instrs = List.map instr instrs }
