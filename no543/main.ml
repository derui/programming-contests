type problem = T | F

let problem_of_string = function
  | "T" -> T
  | "F" -> F
  | _ -> failwith "unknown token"

let string_of_problem = function
  | T -> "T"
  | F -> "F"


let solve_back p p_inv = p_inv
let solve_pair p p_inv = p

let parse_line line =
  let regexp = Str.regexp " " in
  let splitted = Str.split regexp line in
  match splitted with
  | p :: p_inv :: _ -> (problem_of_string p, problem_of_string p_inv)
  | _ -> failwith "invalid line"

let () =
  let p, p_inv = read_line () |> parse_line in  

  let p_back = solve_back p p_inv
  and p_pair = solve_pair p p_inv in
  Printf.printf "%s %s\n" (string_of_problem p_back) (string_of_problem p_pair)
