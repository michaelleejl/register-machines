type error =
  | Unterminated_comment of { at : Text.Span.t }
  | Unexpected_character of { at : Text.Span.t; character : char }
  | Unexpected_token of { at : Text.Span.t }
  | Initialised_in_machine of { at : Text.Span.t; name : string }

exception Error of error

let report : error -> Report.t = function
  | Unterminated_comment { at } ->
      {
        summary = "this comment is never closed";
        label = { at; text = "opened here" };
        notes = [];
      }
  | Unexpected_character { at; character } ->
      {
        summary = Printf.sprintf "unrecognised character %C" character;
        label = { at; text = "remove it" };
        notes = [];
      }
  | Unexpected_token { at } ->
      {
        summary = "this token is out of place";
        label = { at; text = "not expected here" };
        notes = [];
      }
  | Initialised_in_machine { at; name } ->
      {
        summary =
          Printf.sprintf
            "%s is declared inside a machine, where registers start at 0" name;
        label = { at; text = "remove the value" };
        notes = [];
      }

let () = Report.register (function Error e -> Some (report e) | _ -> None)
