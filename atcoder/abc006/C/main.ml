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
  let rec solve' total_legs baby_number adult_number =
    if baby_number < 0 then (-1, -1, -1)
    else
      let legs = (baby_number * baby_legs) + (adult_number * adult_legs) in
      if legs = total_legs then (adult_number, 0, baby_number)
      else
        let legs, baby_number', adult_number' =
          if baby_number > 0 then
            (legs - baby_legs + old_legs, pred baby_number, adult_number)
          else (legs - adult_legs + old_legs, baby_number, pred adult_number)
        in
        if legs = total_legs then (adult_number', 1, baby_number')
        else solve' total_legs (pred baby_number) (succ adult_number)
  in
  solve' total_legs numbers 0

let () =
  let line = read_line () in
  let numbers, total_legs = Scanf.sscanf line "%d %d" (fun a b -> (a, b)) in
  let adults, olds, babies = solve numbers total_legs in
  Printf.printf "%d %d %d\n" adults olds babies
