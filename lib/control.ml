open Located

type body =
 | CAdd of string located * string located
 | CSub of string located * string located * string located
 | CHalt
 | CJump of string located

type instruction = {
  label : string located ;
  body : body ;
}

type config = {
  registers: Definitional.register list;
  instructions: instruction list
}
