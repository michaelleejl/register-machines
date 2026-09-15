open Located

type body =
 | LAdd of string located * string located
 | LSub of string located * string located * string located
 | LHalt

type instr = {
  label : string located ;
  body : body ;
}

type register = {
  name : string located;
  value : int;
}

type config = {
  registers: register list;
  instrs: instr list
}
