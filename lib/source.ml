open Located

type body =
 | SAdd of string located * string located
 | SSub of string located * string located * string located
 | SHalt

type instruction = {
  label : string located ;
  body : body ;
}

type 'r block = {
  machines: machine list;
  registers: 'r list;
  instructions: instruction list
}
and machine = {
  name : string located;
  parameters: string located list;
  body : string located block;
}

type program = Definitional.register block
