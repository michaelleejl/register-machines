open Lang
open Lifted
open Expanded
module StringMap = Map.Make (String)

let qualify prefix name = if prefix = "" then name else prefix ^ "." ^ name

let rename renamings registers (instruction : instruction) =
  let rename_register r =
    match StringMap.find_opt r registers with
    | Some renamed -> renamed
    | None -> r
  in
  let body =
    match instruction.body with
    | EAdd (r, l) -> LAdd (rename_register r, l)
    | ESub (r, l, l') -> LSub (rename_register r, l, l')
    | EHalt -> LHalt
    | EClear (r, k) -> LClear (rename_register r, k)
    | EJump k -> LJump k
    | EExecute { machine; arguments; next } ->
        let machine =
          match StringMap.find_opt machine renamings with
          | Some path -> path
          | None -> failwith ("lift: " ^ machine ^ " is not in scope")
        in
        LExecute
          { machine; arguments = List.map rename_register arguments; next }
  in
  Lifted.{ label = instruction.label; body }

let rec machines prefix renamings lifted =
  List.fold_left (fun (r, l) m -> machine prefix r l m) (renamings, lifted)

and machine prefix renamings lifted (m : machine) =
  let name = qualify prefix m.name in
  let renamings', lifted =
    machines name renamings lifted m.definition.machines
  in
  let registers = List.map (qualify name) m.definition.registers in
  let register_renamings =
    StringMap.of_list (List.combine m.definition.registers registers)
  in
  let instructions =
    List.map (rename renamings' register_renamings) m.definition.instructions
  in
  let lifted_machine =
    Lifted.{ name; parameters = m.parameters; registers; instructions }
  in
  (StringMap.add m.name name renamings, lifted_machine :: lifted)

let program ({ machines = ms; registers; instructions } : program) =
  let renamings, lifted = machines "" StringMap.empty [] ms in
  let registers =
    List.map
      (fun (r : register) -> Lifted.{ name = r.name; value = r.value })
      registers
  in
  Lifted.
    {
      machines = List.rev lifted;
      registers;
      instructions = List.map (rename renamings StringMap.empty) instructions;
    }
