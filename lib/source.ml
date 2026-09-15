type 'a located = {
  at : Span.t;
  v : 'a
}

let located loc v = { at = Span.of_loc loc; v }

type body =
 | SAdd of string located * string located
 | SSub of string located * string located * string located
 | SHalt

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
