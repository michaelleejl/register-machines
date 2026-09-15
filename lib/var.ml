open Source

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

type maps = {
  encoding: int StringMap.t;
  decoding: string IntMap.t;
  firsts: Span.t StringMap.t;
  max: int
}

type t = maps

let initial = {
  encoding = StringMap.empty;
  decoding = IntMap.empty;
  firsts = StringMap.empty;
  max = 0;
}

exception Duplicate of Span.t
exception Undefined of string located

let register {encoding; decoding; firsts; max} name =
  match StringMap.find_opt name.v encoding with
  | Some(_) ->
    let first = StringMap.find name.v firsts in
    raise (Duplicate first)
  | None ->
      let id = max in
      let encoding' = StringMap.add name.v id encoding in
      let firsts' = StringMap.add name.v name.at firsts in
      let decoding' = IntMap.add id name.v decoding in
      {
        encoding=encoding';
        firsts = firsts';
        decoding = decoding';
        max = id + 1
      }

let encode {encoding} name =
  try StringMap.find name.v encoding
  with Not_found -> raise (Undefined name)

let decode {decoding} id =
  IntMap.find id decoding

let count {max} = max

let names {decoding} = List.map snd (IntMap.bindings decoding)
