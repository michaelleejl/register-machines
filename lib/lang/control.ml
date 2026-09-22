type body =
 | CnAdd of string * string
 | CnSub of string * string * string
 | CnHalt
 | CnJump of string
 | CnClear of string * string

type instruction = {
  label : string ;
  body : body ;
}

type register = {
  name : string;
  value : int;
}

type program = {
  registers: register list;
  instructions: instruction list
}
