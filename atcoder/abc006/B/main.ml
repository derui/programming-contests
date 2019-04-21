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

let tribonatch count =
  let rec tribonatch' count' pre_3 pre_2 pre_1 current =
    if count' >= count then current
    else
      match count' with
      | 0 ->
          tribonatch' (succ count') pre_3 pre_2 pre_1 pre_3
      | 1 ->
          tribonatch' (succ count') pre_3 pre_2 pre_1 pre_2
      | 2 ->
          tribonatch' (succ count') pre_3 pre_2 pre_1 pre_1
      | _ ->
          let ret = (pre_3 + pre_2 + pre_1) mod 10007 in
          tribonatch' (succ count') pre_2 pre_1 ret ret
  in
  tribonatch' 0 0 0 1 0

let () =
  let count = read_line () |> int_of_string in
  let ret = match count with 1 | 2 -> 0 | 3 -> 1 | _ -> tribonatch count in
  Printf.printf "%d\n" ret
