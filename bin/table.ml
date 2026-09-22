open Register_machines

type t = { headings : string list; rows : string list list }

let row (machine : Lang.Target.program) (s : Backend.Config.t) =
  Iarray.get machine.label_names s.label
  :: List.map string_of_int (Iarray.to_list s.registers)

let all (machine : Lang.Target.program) states =
  let rows = List.of_seq (Seq.map (row machine) states) in
  let headings =
    match rows with
    | [] -> []
    | _ -> "Label" :: Iarray.to_list machine.register_names
  in
  { headings; rows }

let last machine states =
  match Seq.fold_left (fun _ s -> Some s) None states with
  | None -> { headings = []; rows = [] }
  | Some s -> { headings = []; rows = [ row machine s ] }

let column_widths { headings; rows } =
  match if headings = [] then rows else headings :: rows with
  | [] -> []
  | first :: rest ->
      List.fold_left
        (List.map2 (fun w cell -> max w (String.length cell)))
        (List.map String.length first)
        rest

let line widths cells =
  let pad w cell = String.make (w - String.length cell) ' ' ^ cell in
  String.concat "  " (List.map2 pad widths cells)

let to_string table =
  let widths = column_widths table in
  let body = List.map (line widths) table.rows in
  let lines =
    match table.headings with
    | [] -> body
    | headings ->
        let heading = line widths headings in
        let rule =
          String.concat ""
            (List.init (String.length heading) (fun _ -> "\u{2500}"))
        in
        (rule :: heading :: rule :: body) @ [ rule ]
  in
  String.concat "" (List.map (fun l -> l ^ "\n") lines)
