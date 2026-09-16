open Located

type body =
 | DAdd of string located * string located
 | DSub of string located * string located * string located
 | DHalt

type instruction = {
  label : string located ;
  body : body ;
}

type register = {
  name : string located;
  value : int;
}

type config = {
  registers: register list;
  instructions: instruction list
}
