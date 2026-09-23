type body = NAdd of string * string | NSub of string * string * string | NHalt
type instruction = { label : string; body : body }
type register = { name : string; value : int }
type program = { registers : register list; instructions : instruction list }
