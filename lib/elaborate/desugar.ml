open Lang
open Definitional
open Control

let translate { label; body } =
  let body = match body with
    | CnAdd (r, t) -> DAdd (r, t)
    | CnSub (r, t, f) -> DSub (r, t, f)
    | CnHalt -> DHalt
    | CnJump k -> DSub ("_zero", k, k)
    | CnClear (r, k) -> DSub (r, label, k) in 
    Definitional.{ label; body }

let is_jump {body;_} = match body with 
  | CnJump _ -> true 
  | _ -> false 

let program { registers; instructions } =
  let has_jump =
    List.exists (is_jump) instructions
  in
  let registers =
    List.map (fun (r : register) -> Definitional.{ name = r.name; value = r.value }) registers
  in
  let registers =
    if has_jump then registers @ [ Definitional.{ name = "_zero"; value = 0 } ] else registers
  in
  Definitional.{
    registers;
    instructions = List.map translate instructions }
