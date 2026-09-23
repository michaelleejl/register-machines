open Lang
open Named
open Control

let translate { label; body } =
  let body =
    match body with
    | CnAdd (r, t) -> NAdd (r, t)
    | CnSub (r, t, f) -> NSub (r, t, f)
    | CnHalt -> NHalt
    | CnJump k -> NSub ("_zero", k, k)
    | CnClear (r, k) -> NSub (r, label, k)
  in
  Named.{ label; body }

let is_jump { body; _ } = match body with CnJump _ -> true | _ -> false

let program { registers; instructions } =
  let has_jump = List.exists is_jump instructions in
  let registers =
    List.map
      (fun (r : register) -> Named.{ name = r.name; value = r.value })
      registers
  in
  let registers =
    if has_jump then registers @ [ Named.{ name = "_zero"; value = 0 } ]
    else registers
  in
  Named.{ registers; instructions = List.map translate instructions }
