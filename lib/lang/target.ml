type instruction = 
 | TAdd of int * int 
 | TSub of int * int * int
 | THalt 

type program = {
  register_values: int iarray;
  register_names: string iarray;
  label_names: string iarray;
  instructions: instruction iarray
}
