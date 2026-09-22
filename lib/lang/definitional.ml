type body = DAdd of string * string | DSub of string * string * string | DHalt
type instruction = { label : string; body : body }
type register = { name : string; value : int }
type program = { registers : register list; instructions : instruction list }
