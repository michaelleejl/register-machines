
type register = {
  name : string;
  value : int;
}

type body =
 | LAdd of string * string
 | LSub of string * string * string
 | LHalt
 | LExecute of {
    machine : string;
    arguments : string list;
    next : string
  }
 | LClear of string * string
 | LJump of string

type instruction = {
  label : string ;
  body : body ;
}

type machine = {
  name : string;
  parameters : string list;
  registers : string list;
  instructions : instruction list;
}

type program = {
  machines : machine list;
  registers : register list;
  instructions : instruction list;
}
