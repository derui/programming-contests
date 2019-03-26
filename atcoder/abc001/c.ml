let () =
  let rotate_deg = 225 in
  let dirs =
    [ "NNE"
    ; "NE"
    ; "ENE"
    ; "E"
    ; "ESE"
    ; "SE"
    ; "SSE"
    ; "S"
    ; "SSW"
    ; "SW"
    ; "WSW"
    ; "W"
    ; "WNW"
    ; "NW"
    ; "NNW"
    ; "N" ]
  in
  let rec solve_dir given current index =
    if index >= List.length dirs - 1 then List.nth dirs index
    else if current <= given && given < current + rotate_deg then
      List.nth dirs index
    else solve_dir given (current + rotate_deg) (succ index)
  in
  let solve_w = function
    | _ as v when v <= 2 -> 0
    | _ as v when 3 <= v && v <= 15 -> 1
    | _ as v when 16 <= v && v <= 33 -> 2
    | _ as v when 34 <= v && v <= 54 -> 3
    | _ as v when 55 <= v && v <= 79 -> 4
    | _ as v when 80 <= v && v <= 107 -> 5
    | _ as v when 108 <= v && v <= 138 -> 6
    | _ as v when 139 <= v && v <= 171 -> 7
    | _ as v when 172 <= v && v <= 207 -> 8
    | _ as v when 208 <= v && v <= 244 -> 9
    | _ as v when 245 <= v && v <= 284 -> 10
    | _ as v when 285 <= v && v <= 326 -> 11
    | _ -> 12
  in
  let l = read_line () in
  let i = String.index l ' ' in
  let d = String.sub l 0 i |> int_of_string
  and w = String.sub l (i + 1) (String.length l - i - 1) |> int_of_string in
  let w = w * 10 / 6 in
  let w =
    match w mod 10 with _ as v when v < 5 -> w / 10 | _ -> (w / 10) + 1
  in
  let w = solve_w w in
  let d = if w = 0 then "C" else solve_dir d 113 0 in
  Printf.printf "%s %d\n" d w
