type expr = 
 | TAdd of int * int 
 | TSub of int * int * int
 | THalt 

type config = TCfg of int iarray * expr iarray
