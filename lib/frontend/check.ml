open Text.Located
open Lang
open Checked
open Source
module StringMap = Map.Make (String)

type _ kind =
  | Machine : (string * int) located kind
  | Register : string located kind
  | Label : string located kind
  | Argument : string located kind

type subject = Register | Label | Machine

type error =
  | Duplicate of {
      subject : subject;
      at : Text.Span.t;
      first : Text.Span.t;
      name : string;
    }
  | Undeclared of {
      subject : subject;
      at : Text.Span.t;
      name : string;
      declared : string list;
    }
  | No_registers of { at : Text.Span.t }
  | Wrong_arity of {
      at : Text.Span.t;
      machine : string;
      expected : int;
      given : int;
    }
  | Repeated_argument of {
      at : Text.Span.t;
      first : Text.Span.t;
      name : string;
    }

exception Error of error

let names declared = String.concat ", " declared

let report : error -> Report.t = function
  | Duplicate { subject = Register; at; first; name } ->
      {
        summary = Printf.sprintf "%s is declared twice" name;
        label = { at; text = "declared again here" };
        notes = [ { at = first; text = "first declared here" } ];
      }
  | Duplicate { subject = Label; at; first; name } ->
      {
        summary = Printf.sprintf "%s labels two instructions" name;
        label = { at; text = "used again here" };
        notes = [ { at = first; text = "first used here" } ];
      }
  | Duplicate { subject = Machine; at; first; name } ->
      {
        summary = Printf.sprintf "%s names two machines" name;
        label = { at; text = "declared again here" };
        notes = [ { at = first; text = "first declared here" } ];
      }
  | Undeclared { subject = Register; at; name; declared } ->
      {
        summary = Printf.sprintf "%s is not a register" name;
        label =
          {
            at;
            text =
              (match declared with
              | [] -> "this machine has no registers"
              | declared ->
                  Printf.sprintf "this machine has registers: %s"
                    (names declared));
          };
        notes = [];
      }
  | Undeclared { subject = Label; at; name; declared } ->
      {
        summary = Printf.sprintf "%s is not a label" name;
        label =
          {
            at;
            text = Printf.sprintf "this machine has labels: %s" (names declared);
          };
        notes = [];
      }
  | Undeclared { subject = Machine; at; name; declared } ->
      {
        summary = Printf.sprintf "%s is not a machine" name;
        label =
          {
            at;
            text =
              (match declared with
              | [] -> "no machines are in scope here"
              | declared ->
                  Printf.sprintf "these machines are in scope: %s"
                    (names declared));
          };
        notes = [];
      }
  | No_registers { at } ->
      {
        summary = "this machine has no registers";
        label = { at; text = "declare a register above this instruction" };
        notes = [];
      }
  | Wrong_arity { at; machine; expected; given } ->
      {
        summary =
          Printf.sprintf "%s takes %d %s, not %d" machine expected
            (if expected = 1 then "register" else "registers")
            given;
        label = { at; text = Printf.sprintf "%d given here" given };
        notes = [];
      }
  | Repeated_argument { at; first; name } ->
      {
        summary = Printf.sprintf "%s is passed twice" name;
        label = { at; text = "passed again here" };
        notes = [ { at = first; text = "first passed here" } ];
      }

let () = Report.register (function Error e -> Some (report e) | _ -> None)

module Scope = struct
  type signature = { arity : int; at : Text.Span.t }

  type scope = {
    machines : signature StringMap.t;
    registers : Text.Span.t StringMap.t;
    labels : Text.Span.t StringMap.t;
  }

  let empty =
    {
      machines = StringMap.empty;
      registers = StringMap.empty;
      labels = StringMap.empty;
    }

  let duplicate subject name at first =
    raise (Error (Duplicate { subject; at; first; name }))

  let undeclared subject name at declared =
    raise (Error (Undeclared { subject; at; name; declared }))

  let enter scope =
    { scope with registers = StringMap.empty; labels = StringMap.empty }

  let add : type a. a kind -> scope -> a -> scope = function
    | Machine -> (
        fun scope { v = name, arity; at } ->
          match StringMap.find_opt name scope.machines with
          | Some { at = first; _ } -> duplicate Machine name at first
          | None ->
              {
                scope with
                machines = StringMap.add name { arity; at } scope.machines;
              })
    | Label -> (
        fun scope { v; at } ->
          match StringMap.find_opt v scope.labels with
          | Some first -> duplicate Label v at first
          | None -> { scope with labels = StringMap.add v at scope.labels })
    | Register -> (
        fun scope { v; at } ->
          match StringMap.find_opt v scope.registers with
          | Some first -> duplicate Register v at first
          | None ->
              { scope with registers = StringMap.add v at scope.registers })
    | Argument -> (
        fun scope { v; at } ->
          match StringMap.find_opt v scope.registers with
          | Some first -> duplicate Register v at first
          | None ->
              { scope with registers = StringMap.add v at scope.registers })

  let find subject map { v = name; at } =
    match StringMap.find_opt name map with
    | Some found -> found
    | None -> undeclared subject name at (List.map fst (StringMap.bindings map))

  let machine scope name = find Machine scope.machines name
  let register scope name = ignore (find Register scope.registers name)
  let label scope name = ignore (find Label scope.labels name)
end

let erase_string (r : string located) = r.v

let erase_body = function
  | SAdd (r, l) -> ChAdd (r.v, l.v)
  | SSub (r, l, l') -> ChSub (r.v, l.v, l'.v)
  | SHalt -> ChHalt
  | SExit -> ChExit 
  | SExecute { machine; arguments; next } ->
      ChExecute
        {
          machine = machine.v;
          arguments = List.map erase_string arguments;
          next = next.v;
        }
  | SClear (r, k) -> ChClear (r.v, k.v)
  | SJump k -> ChJump k.v

let erase_instruction ({ label; body } : Source.instruction) :
    Checked.instruction =
  { label = label.v; body = erase_body body }

let rec erase_definition = function
  | SStruct b -> ChStruct (erase_block erase_string b)
  | SApply { name; arguments } ->
      ChApply
        {
          name = erase_string name;
          arguments = List.map erase_string arguments;
        }
  | SSeq (d1, d2) -> ChSeq (erase_definition d1, erase_definition d2)
  | SIf (r, d1, d2) ->
      ChIf (erase_string r, erase_definition d1, erase_definition d2)
  | SWhile (r, d) -> ChWhile (erase_string r, erase_definition d)

and erase_block : 'r 's. ('r -> 's) -> 'r Source.block -> 's Checked.block =
 fun register { machines; registers; instructions } ->
  {
    machines = List.map erase_machine machines;
    registers = List.map register registers;
    instructions = List.map erase_instruction instructions;
  }

and erase_machine ({ name; parameters; definition } : Source.machine) :
    Checked.machine =
  {
    name = name.v;
    parameters = List.map erase_string parameters;
    definition = erase_definition definition;
  }

let erase (p : Source.program) : Checked.program =
  erase_block
    (fun ({ name; value } : Source.register) : Checked.register ->
      { name = name.v; value })
    p

let acc c xs i = List.fold_left c i xs

let rec check ({ registers; instructions; _ } as p) =
  check_top_level_registers p;
  check_program (fun (r : Source.register) -> r.name) Scope.empty p;
  erase p

and check_top_level_registers { registers; instructions; _ } =
  match (registers, instructions) with
  | [], { label; _ } :: _ -> raise (Error (No_registers { at = label.at }))
  | _ -> ()

and check_program :
    'r. ('r -> string located) -> Scope.scope -> 'r block -> unit =
 fun name scope { machines; registers; instructions } ->
  let scope =
    acc check_machine machines scope
    |> acc (check_register name) registers
    |> acc add_label instructions
  in
  List.iter (check_instruction scope) instructions

and check_machine scope { name; parameters; definition } =
  let scope' =
    List.fold_left (Scope.add Register) (Scope.enter scope) parameters
  in
  check_definition scope' definition;
  Scope.add Machine scope { v = (name.v, List.length parameters); at = name.at }

and check_definition scope = function
  | SStruct b -> check_program Fun.id scope b
  | SApply { name; arguments } -> check_call scope name arguments
  | SSeq (d1, d2) ->
      check_definition scope d1;
      check_definition scope d2
  | SIf (r, d1, d2) ->
      Scope.register scope r;
      check_definition scope d1;
      check_definition scope d2
  | SWhile (r, d) ->
      Scope.register scope r;
      check_definition scope d

and check_register :
    'r. ('r -> string located) -> Scope.scope -> 'r -> Scope.scope =
 fun name scope register -> Scope.add Register scope (name register)

and add_label scope { label; body } = Scope.add Label scope label
and check_instruction scope { label; body } = check_body scope body

and check_body scope body =
  match body with
  | SAdd (r, l) ->
      Scope.register scope r;
      Scope.label scope l
  | SSub (r, l1, l2) ->
      Scope.register scope r;
      Scope.label scope l1;
      Scope.label scope l2
  | SHalt | SExit -> ()
  | SClear (r, k) ->
      Scope.register scope r;
      Scope.label scope k
  | SJump k -> Scope.label scope k
  | SExecute { machine; arguments; next } ->
      check_call scope machine arguments;
      Scope.label scope next

and check_call scope machine arguments =
  check_arity scope machine arguments;
  ignore (check_arguments scope StringMap.empty arguments)

and check_arity scope machine arguments =
  let Scope.{ arity } = Scope.machine scope machine in
  let given = List.length arguments in
  if List.length arguments <> arity then
    raise
      (Error
         (Wrong_arity
            { at = machine.at; machine = machine.v; expected = arity; given }))

and check_arguments scope seen = function
  | [] -> scope
  | ({ at; v = name } as arg) :: args -> (
      Scope.register scope arg;
      match StringMap.find_opt arg.v seen with
      | None -> check_arguments scope (StringMap.add name at seen) args
      | Some first -> raise (Error (Repeated_argument { at; first; name })))
