open Definitional
open Target

let linearise ({ registers; instructions } : Source.program) : Control.config =
  let translate ({ label; body } : Source.instruction) : Control.instruction =
    { label;
      body = match body with
        | SAdd (r, t) -> CAdd (r, t)
        | SSub (r, t, f) -> CSub (r, t, f)
        | SHalt -> CHalt }
  in
  { registers; instructions = List.map translate instructions }

let desugar ({ registers; instructions } : Control.config) : Definitional.config =
  let dummy = Span.of_loc (Lexing.dummy_pos, Lexing.dummy_pos) in
  let zero = Located.{ at = dummy; v = "_zero" } in
  let translate ({ label; body } : Control.instruction) : Definitional.instruction =
    { label;
      body = match body with
        | CAdd (r, t) -> DAdd (r, t)
        | CSub (r, t, f) -> DSub (r, t, f)
        | CHalt -> DHalt
        | CJump k -> DSub (zero, k, k) }
  in
  let has_jump =
    List.exists
      (fun ({ body; _ } : Control.instruction) ->
         match body with CJump _ -> true | _ -> false)
      instructions
  in
  { registers = if has_jump then registers @ [ { name = zero; value = 0 } ] else registers;
    instructions = List.map translate instructions }

let rec resolve ({registers; instructions} : Definitional.config) override =
  let register_maps, register_values = extract_registers registers override in
  let label_maps = extract_labels instructions in
  let register_names = Iarray.init (Var.count register_maps) (Var.decode register_maps) in
  let label_names = Iarray.init (Var.count label_maps) (Var.decode label_maps) in
  let t_instructions = List.map (translate register_maps label_maps) instructions in
  { register_values; register_names; label_names; instructions = Iarray.of_list t_instructions }
and extract_registers declarations overrides =
  let l = Iarray.length overrides in
  let extract (i, maps, values) {name; value} =
    let v =
      if i = 0 then value else
      if l <= (i-1) then value else Iarray.get overrides (i-1) in
    (i+1, Var.add maps name, v::values)
  in
  let _, maps, values = List.fold_left extract (0, Var.initial, []) declarations in
  (maps, Iarray.of_list (List.rev values))

and extract_labels instructions =
  let extract maps {label; _} = Var.add maps label in
  List.fold_left extract Var.initial instructions

and translate registers labels instruction =
  let register r = Var.encode registers r in
  let label l = Var.encode labels l in
  match instruction.body with
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
