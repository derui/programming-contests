let () =
  let after = read_line () |> int_of_string in
  let before = read_line () |> int_of_string in
  Printf.printf "%d\n" (after - before)
