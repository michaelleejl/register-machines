type _ mode = 
  | Trace: (string iarray * State.t Seq.t) mode 
  | Value: int mode 
