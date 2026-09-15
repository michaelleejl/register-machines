open Source
open Target
open Error


let extract_registers declarations overrides =
  let l = Iarray.length overrides in
  let extract (i, maps, values, errors) {name; value} =
    let v =
      if i = 0 then value else
      if l <= (i-1) then value else Iarray.get overrides (i-1) in
    try (i+1, Var.register maps name, v::values, errors) with
    | Var.Duplicate first ->
      let error = Duplicate_register { at = name.at; first; name = name.v } in
      (i+1, maps, values, error::errors)
  in
  let _, maps, values, errors = List.fold_left extract (0, Var.initial, [], []) declarations in
  (maps, Iarray.of_list (List.rev values), errors)

type elaboration_state = {
  position: int; 
  errors: error list; 
  exprs: expr list; 
}


let undefined_label num_labels (({errors; _} as st), instr) = 
  let target l errors = 
    if l.v < num_labels then errors 
    else Undefined_label { 
      at = l.at; 
      target = l.v; 
      count = num_labels 
    } :: errors
  in
  let errors = match instr.body with 
    | SAdd (_, t) -> target t errors
    | SSub (_, t, f) -> target f (target t errors)
    | SHalt -> errors
  in
  {st with errors}, instr

let mislabelled (({position; errors; exprs} as st), instr) = 
  if instr.label.v = position then st, instr
  else 
    let err = Mislabelled { 
      at = instr.label.at; 
      written = instr.label.v; 
      expected = position 
    } in 
    {st with errors = err::errors}, instr

let translate maps (({errors; exprs; _} as st), instr) = 
  try 
    let expr = match instr.body with
      | SAdd (r, t) -> TAdd (Var.encode maps r, t.v)
      | SSub (r, t, f) -> TSub (Var.encode maps r, t.v, f.v)
      | SHalt -> THalt
    in {st with exprs = expr::exprs}, instr
  with Var.Undefined r -> 
    let err = Undeclared_register { 
      at = r.at; 
      name = r.v; 
      declared = Var.names maps
      }
  in {st with errors = err::errors}, instr

let next (st, _) = {st with position=st.position+1} 

let extract_instr maps num_labels = 
  fun st -> fun instr ->
    mislabelled (st, instr) 
    |> translate maps 
    |> undefined_label num_labels 
    |> next 

let elaborate ({registers; instrs} : Source.config) override =
  let maps, register_values, duplicates = extract_registers registers override in
  let register_names = Iarray.init (Var.count maps) (Var.decode maps) in
  let num_instructions = List.length instrs in  
  let extractor = extract_instr maps num_instructions in 
  let initial = {position=0;exprs=[];errors=duplicates} in 
  let {errors;exprs} = List.fold_left extractor initial instrs in 
  match errors with
  | [] -> Ok { register_values; register_names; instrs = Iarray.of_list (List.rev exprs) }
  | errors -> Error (List.rev errors)
