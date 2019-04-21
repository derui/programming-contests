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

let solve allow_second takoball_count takoball_seconds guest_count
    guest_seconds =
  if takoball_count < guest_count then "no"
  else
    let tako_max = List.length takoball_seconds
    and guest_max = List.length guest_seconds in
    let rec solve' tako_index guest_index bought_guests =
      if tako_index >= tako_max || guest_index >= guest_max then bought_guests
      else
        let tako_second = List.nth takoball_seconds tako_index
        and guest_second = List.nth guest_seconds guest_index in
        if
          tako_second + allow_second >= guest_second
          && tako_second <= guest_second
        then solve' (succ tako_index) (succ guest_index) (succ bought_guests)
        else if tako_second < guest_second then
          solve' (succ tako_index) guest_index bought_guests
        else solve' tako_index (succ guest_index) bought_guests
    in
    let rest_guest = solve' 0 0 0 in
    if rest_guest - guest_max = 0 then "yes" else "no"

let () =
  let allow_second = read_line () |> int_of_string in
  let takoball_count = read_line () |> int_of_string in
  let takoball_seconds = read_line () in
  let guest_count = read_line () |> int_of_string in
  let guest_seconds = read_line () in
  let takoball_seconds =
    split_on_char ~char:' ' ~str:takoball_seconds |> List.map int_of_string
  and guest_seconds =
    split_on_char ~char:' ' ~str:guest_seconds |> List.map int_of_string
  in
  let result =
    solve allow_second takoball_count takoball_seconds guest_count
      guest_seconds
  in
  Printf.printf "%s\n" result
