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
  definitions: defn list;
  registers: Linear.register list;
  instrs: instr list
}
and defn = {
  name : string located;
  parameters: string located list; 
  program : program;
}
