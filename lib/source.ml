open Located

type body =
 | SAdd of string located * string located
 | SSub of string located * string located * string located
 | SHalt

type instruction = {
  label : string located ;
  body : body ;
}

type program = {
  machines: machine list;
  registers: Definitional.register list;
  instructions: instruction list
}
and machine = {
  name : string located;
  parameters: string located list; 
  program : program;
}
