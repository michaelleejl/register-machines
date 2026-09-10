type t = { start : Lexing.position; stop : Lexing.position }

let of_loc (start, stop) = { start; stop }
