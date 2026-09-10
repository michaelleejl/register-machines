type 'a located = {
  at : Span.t;
  v : 'a
}

let located loc v = { at = Span.of_loc loc; v }

type body =
 | SAdd of int located * int located
 | SSub of int located * int located * int located
 | SHalt

type instr = {
  label : int located ;
  body : body ;
}

type register = {
  index : int located;
  value : int
}

type config = SCfg of register iarray * instr iarray
