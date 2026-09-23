type register = { name : string; value : int }

type body =
  | EAdd of string * string
  | ESub of string * string * string
  | EHalt
  | EExit
  | EExecute of { machine : string; arguments : string list; next : string }
  | EClear of string * string
  | EJump of string

type instruction = { label : string; body : body }

type 'r block = {
  machines : machine list;
  registers : 'r list;
  instructions : instruction list;
}

and machine = {
  name : string;
  parameters : string list;
  definition : string block;
}

type program = register block
