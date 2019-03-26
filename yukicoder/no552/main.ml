
let () =
  let number = read_line () |> Big_int.big_int_of_string in
  let number = Big_int.mult_int_big_int 10 number in
  Big_int.string_of_big_int number |> print_endline
