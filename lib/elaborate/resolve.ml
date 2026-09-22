open Lang.Definitional
open Lang.Target

let extract_registers declarations =
  let extract (maps, values) { name; value } = (Var.add maps name, value :: values) in
  let maps, values = List.fold_left extract (Var.initial, []) declarations in
  (maps, Iarray.of_list (List.rev values))

let extract_labels instructions =
  let extract maps { label; _ } = Var.add maps label in
  List.fold_left extract Var.initial instructions

let translate registers labels instruction =
  let register r = Var.encode registers r in
  let label l = Var.encode labels l in
  match instruction.body with
  | DAdd (r, t) -> TAdd (register r, label t)
  | DSub (r, t, f) -> TSub (register r, label t, label f)
  | DHalt -> THalt

let rec program { registers; instructions } =
  let register_maps, register_values = extract_registers registers in
  let label_maps = extract_labels instructions in
  let register_names = Iarray.init (Var.count register_maps) (Var.decode register_maps) in
  let label_names = Iarray.init (Var.count label_maps) (Var.decode label_maps) in
  let t_instructions = List.map (translate register_maps label_maps) instructions in
  { register_values; register_names; label_names;
                instructions = Iarray.of_list t_instructions }
