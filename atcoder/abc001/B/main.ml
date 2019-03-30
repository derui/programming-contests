let () =
  let meter = read_line () |> int_of_string in
  let ret =
    match meter with
    | _ as v when v < 100 -> 0
    | _ as v when 100 <= v && v <= 5000 -> v / 100
    | _ as v when 6000 <= v && v <= 30000 -> (v / 1000) + 50
    | _ as v when 35000 <= v && v <= 70000 -> (((v / 1000) - 30) / 5) + 80
    | _ -> 89
  in
  Printf.printf "%02d\n" ret
