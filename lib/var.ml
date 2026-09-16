open Located

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

type t = {
  encoding: int StringMap.t;
  decoding: string IntMap.t;
  max: int;
}

let initial = {
  encoding = StringMap.empty;
  decoding = IntMap.empty;
  max = 0;
}

let add {encoding; decoding; max} name =
  if StringMap.mem name.v encoding then failwith ("Var.add: " ^ name.v ^ " is declared twice");
  {
    encoding = StringMap.add name.v max encoding;
    decoding = IntMap.add max name.v decoding;
    max = max + 1;
  }

let encode {encoding; _} name =
  match StringMap.find_opt name.v encoding with
  | Some id -> id
  | None -> failwith ("Var.encode: " ^ name.v ^ " is undeclared")

let decode {decoding; _} id =
  IntMap.find id decoding

let count {max; _} = max
