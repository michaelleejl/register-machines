open Definitional
open Target
open Error


let extract_registers declarations overrides =
  let l = Iarray.length overrides in
  let extract (i, maps, values) {name; value} =
    let v =
      if i = 0 then value else
      if l <= (i-1) then value else Iarray.get overrides (i-1) in
    (i+1, Var.add maps name, v::values)
  in
  let _, maps, values = List.fold_left extract (0, Var.initial Register, []) declarations in
  (maps, Iarray.of_list (List.rev values))

let extract_labels instrs =
  let extract maps {label; _} = Var.add maps label in
  List.fold_left extract (Var.initial Label) instrs

let translate registers labels instr =
  let register r = Var.encode registers r in
  let label l = Var.encode labels l in
  match instr.body with
  | DAdd (r, t) ->
    let r = register r in
    let t = label t in
    TAdd (r, t)
  | DSub (r, t, f) ->
    let r = register r in
    let t = label t in
    let f = label f in
    TSub (r, t, f)
  | DHalt -> THalt

let elaborate ({registers; instrs} : Definitional.config) override =
  let register_maps, register_values = extract_registers registers override in
  let label_maps = extract_labels instrs in
  let register_names = Iarray.init (Var.count register_maps) (Var.decode register_maps) in
  let label_names = Iarray.init (Var.count label_maps) (Var.decode label_maps) in
  let t_instrs = List.map (translate register_maps label_maps) instrs in
  { register_values; register_names; label_names; instrs = Iarray.of_list t_instrs }
