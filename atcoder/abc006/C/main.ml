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

let baby_legs = 4

let old_legs = 3

let adult_legs = 2

let solve numbers total_legs =
  let calc_legs total_numbers total_legs numbers legs_of_type =
    (total_numbers - numbers, total_legs - (numbers * legs_of_type))
  in
  let rec solve_adult total_numbers total_legs prev_numbers =
    if prev_numbers = 0 then
      if total_numbers <> 0 || total_legs <> 0 then -1 else prev_numbers
    else
      let last_numbers, rest_legs =
        calc_legs total_numbers total_legs prev_numbers adult_legs
      in
      if last_numbers = 0 && rest_legs = 0 then prev_numbers
      else if last_numbers = 0 || rest_legs = 0 then -1
      else solve_adult last_numbers rest_legs (pred prev_numbers)
  in
  let rec solve_old total_numbers total_legs prev_numbers =
    if prev_numbers = 0 then
      (solve_adult total_numbers total_legs total_numbers, 0)
    else
      let rest_numbers, rest_legs =
        calc_legs total_numbers total_legs prev_numbers old_legs
      in
      if rest_numbers = 0 && rest_legs = 0 then (prev_numbers, 0)
      else if rest_numbers <= 0 || rest_legs <= 0 then
        solve_old total_numbers total_legs (pred prev_numbers)
      else
        let adults = solve_adult rest_numbers rest_legs rest_numbers in
        if adults = -1 then
          solve_old total_numbers total_legs (pred prev_numbers)
        else (adults, prev_numbers)
  in
  let rec solve_baby total_numbers total_legs prev_numbers =
    let rest_numbers, rest_legs =
      calc_legs total_numbers total_legs prev_numbers baby_legs
    in
    if rest_numbers = 0 && rest_legs = 0 then (prev_numbers, 0, 0)
    else if rest_numbers <= 0 || rest_legs <= 0 then
      solve_baby total_numbers total_legs (pred prev_numbers)
    else
      let adults, olds = solve_old rest_numbers rest_legs rest_numbers in
      if adults = -1 || olds = -1 then
        solve_baby total_numbers total_legs (pred prev_numbers)
      else (adults, olds, prev_numbers)
  in
  solve_baby numbers total_legs (total_legs / baby_legs)

let () =
  let line = read_line () in
  let numbers, total_legs = Scanf.sscanf line "%d %d" (fun a b -> (a, b)) in
  let adults, olds, babies = solve numbers total_legs in
  Printf.printf "%d %d %d\n" adults olds babies
