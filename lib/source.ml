type 'a located = {
  at : Span.t;
  v : 'a
}

let located loc v = { at = Span.of_loc loc; v }

type body =
 | SAdd of string located * int located
 | SSub of string located * int located * int located
 | SHalt

type instr = {
  label : int located ;
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
