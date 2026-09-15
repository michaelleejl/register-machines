open Located

type body =
 | SAdd of string located * string located
 | SSub of string located * string located * string located
 | SHalt

type instr = {
  label : string located ;
  body : body ;
}

type program = {
  registers: Linear.register list;
  instrs: instr list
}
