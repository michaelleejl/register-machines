open Located
open Source

type _ kind = 
  | Machine : (string * int) located kind 
  | Register : string located kind
  | Label : string located kind
  | Argument : string located kind

module Scope = struct 
  module StringMap = Map.Make(String)
  
  type signature = { arity : int; at : Span.t }

  type scope = {
    machines : signature StringMap.t;
    registers : Span.t StringMap.t;
    labels : Span.t StringMap.t;
  }

  let empty = {
    machines = StringMap.empty;
    registers = StringMap.empty;
    labels = StringMap.empty;
  }

  let duplicate kind name at first =
    raise (Error.Fault (Error.Duplicate { kind; at; first; name }))

  let undeclared kind name at declared = 
    raise (Error.Fault (Error.Undeclared { kind; at; name; declared}))

  let enter scope = { scope with registers = StringMap.empty; labels = StringMap.empty }

  let add: type a. a kind -> scope -> a -> scope = function
    | Machine -> fun scope { v = (name, arity); at } -> 
      (match StringMap.find_opt name scope.machines with
       | Some { at = first; _ } -> duplicate Error.Machine name at first
       | None -> { scope with machines = StringMap.add name { arity; at } scope.machines })
    | Label -> fun scope { v; at } ->
      (match StringMap.find_opt v scope.labels with
       | Some first -> duplicate Error.Label v at first
       | None -> { scope with labels = StringMap.add v at scope.labels })
    | Register -> fun scope {v; at} -> 
      (match StringMap.find_opt v scope.registers with
       | Some first -> duplicate Error.Register v at first
       | None -> { scope with registers = StringMap.add v at scope.registers }) 
    | Argument -> fun scope {v; at} -> 
      (match StringMap.find_opt v scope.registers with
       | Some first -> duplicate Error.Register v at first
       | None -> { scope with registers = StringMap.add v at scope.registers }) 


  let find kind map { v = name; at } =
    match StringMap.find_opt name map with
    | Some found -> found
    | None -> undeclared kind name at (List.map fst (StringMap.bindings map))

  let machine scope name = find Error.Machine scope.machines name
  let register scope name = ignore (find Error.Register scope.registers name)
  let label scope name = ignore (find Error.Label scope.labels name)
end 

let acc c xs i = List.fold_left c i xs 

let rec check ({ registers; instructions; _ } as p) =
  check_top_level_registers p;
  check_program (fun (r : Definitional.register) -> r.name) Scope.empty p; p
and check_top_level_registers ({ registers; instructions; _ }) =
  match registers, instructions with
   | [], { label; _ } :: _ -> raise (Error.Fault (Error.No_registers { at = label.at }))
   | _ -> ()
and check_program : 'r. ('r -> string located) -> Scope.scope -> 'r block -> unit =
  fun name scope {machines;registers;instructions} ->
 let scope = acc check_machine machines scope
          |> acc (check_register name) registers
          |> acc add_label instructions in
  List.iter (check_instruction scope) instructions

and check_machine scope {name;parameters;body} =
  let scope' = List.fold_left (Scope.add Register) (Scope.enter scope) parameters in
  check_program Fun.id scope' body ;
  Scope.add Machine scope ({v=(name.v, List.length parameters); at=name.at})
and check_register : 'r. ('r -> string located) -> Scope.scope -> 'r -> Scope.scope =
  fun name scope register -> Scope.add Register scope (name register)
and add_label scope {label; body} = 
  Scope.add Label scope label
and check_instruction scope {label; body} = 
  check_body scope body 
and check_body scope body = match body with 
  | SAdd(r, l) ->
    Scope.register scope r;
    Scope.label scope l 
  | SSub(r, l1, l2) -> 
    Scope.register scope r;
    Scope.label scope l1;
    Scope.label scope l2 
  | SHalt -> ()

