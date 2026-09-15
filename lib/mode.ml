type _ mode = 
  | Trace: (Target.config * State.t Seq.t) mode
  | Value: int mode 
