open Lang
open Control
open Lifted
module StringMap = Map.Make (String)

let qualify prefix name = if prefix = "" then name else prefix ^ "." ^ name

let rec inline machines rename_label rename_register subst_halt { label; body }
    acc =
  let open Control in
  let label = rename_label label in
  match body with
  | LAdd (r, t) ->
      { label; body = CnAdd (rename_register r, rename_label t) } :: acc
  | LSub (r, t, f) ->
      {
        label;
        body = CnSub (rename_register r, rename_label t, rename_label f);
      }
      :: acc
  | LHalt -> Lang.Control.{ label; body = subst_halt } :: acc
  | LClear (r, k) ->
      Lang.Control.{ label; body = CnClear (rename_register r, rename_label k) }
      :: acc
  | LJump k -> Lang.Control.{ label; body = CnJump (rename_label k) } :: acc
  | LExecute { machine = name; arguments; next } ->
      let machine = StringMap.find name machines in
      let subst =
        StringMap.of_list
          (List.combine machine.parameters (List.map rename_register arguments))
      in
      let rnm_l l = label ^ "." ^ l in
      let rnm_r r =
        match StringMap.find_opt r subst with
        | Some v -> v
        | None -> rename_register r
      in
      let halt = CnJump (rename_label next) in
      let f = inline machines rnm_l rnm_r halt in
      let entry = rnm_l (List.hd machine.instructions).label in
      { label; body = CnJump entry }
      :: List.fold_right f machine.instructions acc

let program ({ machines; registers; instructions } : program) =
  let registers =
    List.map
      (fun (r : register) -> Lang.Control.{ name = r.name; value = r.value })
      registers
    @ List.fold_right
        (fun (m : machine) acc ->
          List.map (fun name -> Lang.Control.{ name; value = 0 }) m.registers
          @ acc)
        machines []
  in
  let machines =
    StringMap.of_list
      (List.map (fun (m : Lang.Lifted.machine) -> (m.name, m)) machines)
  in
  Lang.Control.
    {
      registers;
      instructions =
        List.fold_right (inline machines Fun.id Fun.id CnHalt) instructions [];
    }
