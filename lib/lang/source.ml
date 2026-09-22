open Text.Located

type body =
 | SAdd of string located * string located
 | SSub of string located * string located * string located
 | SHalt
 | SExecute of {
    machine : string located;
    arguments : string located list;
    next : string located
  }
 | SClear of string located * string located
 | SJump of string located

type instruction = {
  label : string located ;
  body : body ;
}

type register = {
  name : string located;
  value : int;
}

type 'r block = {
  machines: machine list;
  registers: 'r list;
  instructions: instruction list
}
and machine = {
  name : string located;
  parameters: string located list;
  definition : string located block;
}

type program = register block
