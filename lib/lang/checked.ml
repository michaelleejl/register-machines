type body =
  | ChAdd of string * string
  | ChSub of string * string * string
  | ChHalt
  | ChExecute of { machine : string; arguments : string list; next : string }
  | ChClear of string * string
  | ChJump of string

type instruction = { label : string; body : body }

type 'r block = {
  machines : machine list;
  registers : 'r list;
  instructions : instruction list;
}

and machine = {
  name : string;
  parameters : string list;
  definition : definition;
}

and definition =
  | ChStruct of string block
  | ChApply of { name : string; arguments : string list }
  | ChSeq of definition * definition
  | ChIf of string * definition * definition
  | ChWhile of string * definition

type register = { name : string; value : int }
type program = register block
