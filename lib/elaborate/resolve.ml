open Lang.Definitional
open Lang.Target

module Var = struct
  module StringMap = Map.Make (String)
  module IntMap = Map.Make (Int)

  type t = { encoding : int StringMap.t; decoding : string IntMap.t; max : int }

  let initial = { encoding = StringMap.empty; decoding = IntMap.empty; max = 0 }

  let add { encoding; decoding; max } name =
    if StringMap.mem name encoding then
      failwith ("Var.add: " ^ name ^ " is declared twice");
    {
      encoding = StringMap.add name max encoding;
      decoding = IntMap.add max name decoding;
      max = max + 1;
    }

  let encode { encoding; _ } name =
    match StringMap.find_opt name encoding with
    | Some id -> id
    | None -> failwith ("Var.encode: " ^ name ^ " is undeclared")

  let decode { decoding; _ } id = IntMap.find id decoding
  let count { max; _ } = max
end

let extract_registers declarations =
  let extract (maps, values) { name; value } =
    (Var.add maps name, value :: values)
  in
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
  let register_names =
    Iarray.init (Var.count register_maps) (Var.decode register_maps)
  in
  let label_names =
    Iarray.init (Var.count label_maps) (Var.decode label_maps)
  in
  let t_instructions =
    List.map (translate register_maps label_maps) instructions
  in
  {
    register_values;
    register_names;
    label_names;
    instructions = Iarray.of_list t_instructions;
  }
