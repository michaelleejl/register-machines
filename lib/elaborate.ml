open Source
open Target
open Error 


module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)


module Var = struct 

  type maps = {
    encoding: int StringMap.t;
    decoding: string IntMap.t;
    values: int IntMap.t; 
    firsts: Span.t StringMap.t;
    max: int 
  }

  let initial = {
    encoding = StringMap.empty; 
    decoding = IntMap.empty; 
    values = IntMap.empty; 
    firsts = StringMap.empty;
    max = 0;
  }

  exception Duplicate of Span.t
  exception Undefined of string located 
  
  let register {encoding; decoding; firsts; values; max} name value = 
    match StringMap.find_opt name.v encoding with 
    | Some(_) -> 
      let first = StringMap.find name.v firsts in
      raise (Duplicate first) 
    | None -> 
        let id = max in 
        let encoding' = StringMap.add name.v id encoding in 
        let firsts' = StringMap.add name.v name.at firsts in 
        let decoding' = IntMap.add id name.v decoding in 
        let values' = IntMap.add id value values in 
        {
          encoding=encoding'; 
          firsts = firsts';
          decoding = decoding';
          values = values';
          max = id + 1
        }

    let encode {encoding} name = 
      try StringMap.find name.v encoding
      with Not_found -> raise (Undefined name)  

    let decode {decoding} id = 
      IntMap.find id decoding 
    
    let value {values} id = 
      IntMap.find id values 

    let count {max} = max

    let names {decoding} = List.map snd (IntMap.bindings decoding)
end 

let extract_registers declarations =
  let extract (maps, errors) {name; value} =
    try (Var.register maps name value, errors) with 
    | Var.Duplicate first -> 
      let error = Duplicate_register { at = name.at; first; name = name.v } in 
      (maps, error::errors)
  in
  let maps, errors = List.fold_left extract (Var.initial, []) declarations in
  (maps, errors)

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

let elaborate (SCfg (declared, instrs)) =
  let maps, duplicates = extract_registers declared in
  let values = Iarray.init (Var.count maps) (Var.value maps) in 
  let names = Iarray.init (Var.count maps) (Var.decode maps) in 
  let num_instructions = List.length instrs in  
  let extractor = extract_instr maps num_instructions in 
  let initial = {position=0;exprs=[];errors=duplicates} in 
  let {errors;exprs} = List.fold_left extractor initial instrs in 
  match errors with
  | [] -> Ok (TCfg (values, names, Iarray.of_list (List.rev exprs)))
  | errors -> Error (List.rev errors)
