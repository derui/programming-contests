let take_list ~num list =
  let rec take_list' count rest accum =
    if count = 0 then List.rev accum
    else
      match rest with
      | [] ->
          List.rev accum
      | v :: rest ->
          take_list' (pred count) rest (v :: accum)
  in
  take_list' num list []

let split_on_char ~char ~str =
  let rec split str accum =
    if String.length str = 0 then List.rev accum
    else
      try
        let index = String.index str char in
        let splitted = String.sub str 0 index
        and rest =
          String.sub str (index + 1) (String.length str - index - 1)
        in
        split rest (splitted :: accum)
      with Not_found -> List.rev (str :: accum)
  in
  split str []

(* calculate factorial *)
let factorial n =
  let rec laddar' accum n =
    if n = 0 then accum
    else laddar' Big_int.(mult_int_big_int n accum) (pred n)
  in
  laddar' (Big_int.big_int_of_int 1) n

(* calculate combination as nCm *)
let combination n m =
  let n' = factorial n and m' = factorial m and nm' = factorial (n - m) in
  Big_int.(div_big_int n' (mult_big_int m' nm'))

let read_lines count ~f =
  let rec read_lines' current accum =
    if current >= count then List.rev accum
    else
      let line = read_line () |> f in
      read_lines' (succ current) (line :: accum)
  in
  read_lines' 0 []

let solve coins =
  let correct_divisors v = List.filter (fun v' -> Int64.rem v v' = 0L) coins in
  let calculate_expected v =
    let divisors = (correct_divisors v |> List.length) - 1 in
    let divisors' = float_of_int divisors in
    if divisors mod 2 = 1 then 0.5
    else (divisors' +. 2.) /. ((2. *. divisors') +. 2.)
  in
  List.fold_left (fun accum v -> accum +. calculate_expected v) 0. coins

let () =
  let count = read_line () |> int_of_string in
  let coins = read_lines count ~f:Int64.of_string in
  let result = solve coins in
  Printf.printf "%.6f\n" result
