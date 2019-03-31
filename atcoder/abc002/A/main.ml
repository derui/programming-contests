let () =
  let line = read_line () in
  Scanf.sscanf line "%Ld %Ld" (fun x y -> Printf.printf "%Ld\n" (max x y))
