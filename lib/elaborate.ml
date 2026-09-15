open Source
open Target
open Error


let extract_registers declarations overrides =
  let l = Iarray.length overrides in
  let extract (i, maps, values) {name; value} =
    let v =
      if i = 0 then value else
      if l <= (i-1) then value else Iarray.get overrides (i-1) in
    try (i+1, Var.register maps name, v::values) with
    | Var.Duplicate first ->
      raise (Fault (Duplicate_register { at = name.at; first; name = name.v }))
  in
  let _, maps, values = List.fold_left extract (0, Var.initial, []) declarations in
  (maps, Iarray.of_list (List.rev values))

type elaboration_state = {
  position: int;
  t_instrs: instr list;
}

let undefined_label num_labels (st, instr) =
  let target l =
    if l.v < num_labels then ()
    else raise (Fault (Undefined_label {
      at = l.at;
      target = l.v;
      count = num_labels
    }))
  in
  let () = match instr.body with
    | SAdd (_, t) -> target t
    | SSub (_, t, f) -> target t; target f
    | SHalt -> ()
  in
  st, instr

let mislabelled (({position; _} as st), instr) =
  if instr.label.v = position then st, instr
  else
    raise (Fault (
      Mislabelled {
        at = instr.label.at;
        written = instr.label.v;
        expected = position
      }
    ))

let translate maps (({t_instrs; _} as st), instr) =
  try 
    let t_instr = match instr.body with
      | SAdd (r, t) -> TAdd (Var.encode maps r, t.v)
      | SSub (r, t, f) -> TSub (Var.encode maps r, t.v, f.v)
      | SHalt -> THalt
    in {st with t_instrs = t_instr::t_instrs}, instr
  with Var.Undefined r -> 
    raise (Fault (
    Undeclared_register { 
      at = r.at; 
      name = r.v; 
      declared = Var.names maps
    }))

let next (st, _) = {st with position=st.position+1} 

let extract_instr maps num_labels = 
  fun st -> fun instr ->
    mislabelled (st, instr) 
    |> translate maps 
    |> undefined_label num_labels 
    |> next 

let elaborate ({registers; instrs} : Source.config) override =
  let maps, register_values = extract_registers registers override in
  let register_names = Iarray.init (Var.count maps) (Var.decode maps) in
  let num_instructions = List.length instrs in
  let extractor = extract_instr maps num_instructions in
  let initial = {position=0;t_instrs=[]} in
  let {t_instrs} = List.fold_left extractor initial instrs in
  { register_values; register_names; instrs = Iarray.of_list (List.rev t_instrs) }
