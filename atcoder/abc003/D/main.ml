let laddar n =
  let rec laddar' accum n =
    if n = 0 then accum
    else laddar' Big_int.(mult_int_big_int n accum) (pred n)
  in
  laddar' (Big_int.big_int_of_int 1) n

(* calculate combination as nCm *)
let combination n m =
  let n' = laddar n and m' = laddar m and nm' = laddar (n - m) in
  Big_int.(div_big_int n' (mult_big_int m' nm'))

let () =
  let line = read_line () in
  let rows, columns =
    Scanf.sscanf line "%d %d" (fun rows columns -> (rows, columns))
  in
  let line = read_line () in
  let wrapped_rows, wrapped_columns =
    Scanf.sscanf line "%d %d" (fun rows columns -> (rows, columns))
  in
  let line = read_line () in
  let desks, racks =
    Scanf.sscanf line "%d %d" (fun rows columns -> (rows, columns))
  in
  if wrapped_columns * wrapped_rows = desks + racks then
    let diviser = Big_int.big_int_of_int64 1000000007L in
    let ret =
      if desks = 0 || racks = 0 then Big_int.big_int_of_int 1
      else
        let ret = laddar (desks + racks) in
        Big_int.(div_big_int ret (big_int_of_int (desks + racks)))
    in
    let patterns =
      (rows - wrapped_rows + 1) * (columns - wrapped_columns + 1)
    in
    let ret = Big_int.mult_int_big_int patterns ret in
    Printf.printf "%s\n" Big_int.(mod_big_int ret diviser |> string_of_big_int)
  else Printf.printf "unknown\n"
