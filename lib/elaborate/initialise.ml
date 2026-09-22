open Lang.Checked

let rec machine m =
  let rec clear registers =
    match registers with
    | [] -> m.definition.instructions
    | r :: rs ->
      let rest = clear rs in
      let next =
        match rest with
        | first :: _ -> first.label
        | [] -> failwith "initialise: a machine has no instructions"
      in
      { label = "_init_" ^ r; body = ChClear (r, next) } :: rest
  in
  { m with definition = { m.definition with machines = List.map machine m.definition.machines;
                                instructions = clear m.definition.registers } }

let program p = { p with machines = List.map machine p.machines }
