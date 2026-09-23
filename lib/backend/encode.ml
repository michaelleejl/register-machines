open Lang
open Target

let rec pow b n = if n = 0 then 1 else b * pow b (n - 1)
let pair_to_pos x y = pow 2 x * ((2 * y) + 1)
let pair_to_nat x y = pair_to_pos x y - 1

let rec list_to_nat = function
  | [] -> 0
  | x :: xs -> pow 2 x * ((2 * list_to_nat xs) + 1)

let instruction_to_nat = function
  | THalt -> 0
  | TAdd (r, l) -> pair_to_pos (2 * r) l
  | TSub (r, l1, l2) -> pair_to_pos ((2 * r) + 1) (pair_to_nat l1 l2)

let instructions_to_nat p =
  Iarray.to_list p |> List.map instruction_to_nat |> list_to_nat

let registers_to_nat rs = Iarray.to_list rs |> list_to_nat

let program { instructions; register_values } =
  (registers_to_nat register_values, instructions_to_nat instructions)
