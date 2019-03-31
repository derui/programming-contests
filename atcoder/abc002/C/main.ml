let () =
  let line = read_line () in
  Scanf.sscanf line "%d %d %d %d %d %d" (fun x1 y1 x2 y2 x3 y3 ->
      let x2, y2 = (float_of_int (x2 - x1), float_of_int (y2 - y1))
      and x3, y3 = (float_of_int (x3 - x1), float_of_int (y3 - y1)) in
      let size =
        abs_float ((x2 *. y3) -. (y2 *. x3)) /. 2.0 *. 100.0 |> int_of_float
      in
      let rec prec v = if v = 0 || v mod 10 <> 0 then v else prec (v / 10) in
      Printf.printf "%d.%d\n" (size / 100) (prec (size mod 100)) )
