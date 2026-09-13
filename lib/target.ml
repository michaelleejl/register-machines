type expr = 
 | TAdd of int * int 
 | TSub of int * int * int
 | THalt 

type config = TCfg of (int*string) iarray * expr iarray
