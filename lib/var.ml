open Linear
open Error

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

type maps = {
  encoding: int StringMap.t;
  decoding: string IntMap.t;
  firsts: Span.t StringMap.t;
  max: int;
  kind : kind;
}

type t = maps

let initial kind = {
  encoding = StringMap.empty;
  decoding = IntMap.empty;
  firsts = StringMap.empty;
  max = 0;
  kind = kind;
}

let add {encoding; decoding; firsts; max ; kind} name =
  match StringMap.find_opt name.v encoding with
  | Some(_) ->
    let first = StringMap.find name.v firsts in
    raise (Fault (Duplicate { kind; at = name.at; first; name = name.v }))
  | None ->
      let id = max in
      let encoding' = StringMap.add name.v id encoding in
      let firsts' = StringMap.add name.v name.at firsts in
      let decoding' = IntMap.add id name.v decoding in
      {
        encoding=encoding';
        firsts = firsts';
        decoding = decoding';
        max = id + 1; 
        kind
      }

let names {decoding} = List.map snd (IntMap.bindings decoding)

let encode maps name =
  try StringMap.find name.v maps.encoding
  with Not_found -> raise (Fault (Undeclared { kind=maps.kind; at = name.at; name = name.v; declared = names maps }))

let decode {decoding} id =
  IntMap.find id decoding

let count {max} = max
