open Elaborate
open Definitional
open Target 
open Seq
open Mode 

let update r f registers =
    Iarray.mapi (fun i v -> if i = r then f v else v) registers

let step instructions registers j =
  match Iarray.get instructions j with
  | THalt -> None
  | TAdd (r, l) -> Some (update r succ registers, l)
  | TSub (r, l, l') ->
    if Iarray.get registers r = 0
    then Some (registers, l')
    else Some (update r pred registers, l)

let eval initial instructions =
  let rec go registers j () =
    Cons (State.{ label = j; registers },
          match step instructions registers j with
          | None -> (fun () -> Nil)
          | Some (registers', j') -> go registers' j')
  in
  go initial 0

let run: type a. a mode -> Definitional.config -> int option -> int iarray -> a =
fun mode prog bound override ->
    let ({ register_values; instructions; _ } as machine) = resolve prog override in
      let states = eval register_values instructions in
        let states = match bound with None -> states | Some b -> Seq.take (b + 1) states in
        match mode with
        | Trace -> (machine, states)
        | Value ->
            Seq.fold_left (fun _ (s : State.t) -> Iarray.get s.registers 0)
                  (Iarray.get register_values 0) states
