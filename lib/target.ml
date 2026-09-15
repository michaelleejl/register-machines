type instr = 
 | TAdd of int * int 
 | TSub of int * int * int
 | THalt 

type config = {
  register_values: int iarray;
  register_names: string iarray;
  instrs: instr iarray
}
