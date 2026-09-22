type _ mode =
  | Trace : (Lang.Target.program * Config.t Seq.t) mode
  | Value : int mode
