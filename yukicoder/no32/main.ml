
let solve sc mc lc =
  let sc_rest = sc mod 25 in
  let mc' = sc / 25 in
  let mc_rest = (mc + mc') mod 4 in
  let lc' = (mc + mc') / 4 in
  let lc_rest = (lc + lc') mod 10 in

  sc_rest + mc_rest + lc_rest

let () =
  let large_coins = read_line () |> int_of_string in
  let middle_coins = read_line () |> int_of_string in
  let small_coins = read_line () |> int_of_string in
  Printf.printf "%d\n" (solve small_coins middle_coins large_coins)
