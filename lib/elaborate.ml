open Source
open Target
open Error 

let extract_registers declarations =
  let count = 1 + Iarray.fold_left (fun m r -> max m r.index.v) (-1) declarations in
  let extract (values, errors) { index; value } =
    match List.assoc_opt index.v values with
    | Some (_, first) ->
      (values, Duplicate_register { at = index.at; first; index = index.v } :: errors)
    | None -> ((index.v, (value, index.at)) :: values, errors)
  in
  let values, errors = Iarray.fold_left extract ([], []) declarations in
  let value i = match List.assoc_opt i values with Some (v, _) -> v | None -> 0 in
  (Iarray.init count value, List.rev errors)

let misnamed ~num_registers ~num_labels position instr es =
  let register r es = if r.v < num_registers then es else
    Undeclared_register { at = r.at; index = r.v; count = num_registers }::es in
  let label l es = if l.v < num_labels then es else
    Undefined_label { at = l.at; target = l.v; count = num_labels }::es in
  let mislabelled es =
    if instr.label.v = position then es
    else Mislabelled { at = instr.label.at; written = instr.label.v; expected = position }::es
  in
  mislabelled
  (match instr.body with
    | SAdd (r, t) -> register r (label t es)
    | SSub (r, t, f) -> register r (label t ( label f es))
    | SHalt -> es)

let body i =
  match i.body with
  | SAdd (r, t) -> TAdd (r.v, t.v)
  | SSub (r, t, f) -> TSub (r.v, t.v, f.v)
  | SHalt -> THalt

let elaborate (SCfg (declared, instrs)) =
  let initial, duplicates = extract_registers declared in
  let num_registers = Iarray.length initial and num_labels = Iarray.length instrs in
  let errors =
    duplicates
    @ (
      List.fold_right (@@)
      (List.mapi (misnamed ~num_registers ~num_labels) (Iarray.to_list instrs))
      []
      )
  in
  match errors with
  | [] -> Ok (TCfg (initial, Iarray.map body instrs))
  | errors -> Error errors
