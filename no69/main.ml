let to_code_array str =
  let ret = Array.make (String.length str) 0 in
  String.iteri (fun index char -> ret.(index) <- Char.code char) str;
  Array.sort compare ret;
  ret

let () =
  let base = read_line () |> to_code_array |> Array.to_list in
  let target = read_line () |> to_code_array |> Array.to_list in
  
  match (List.for_all2 (=) base target) with
  | true -> print_endline "YES"
  | false -> print_endline "NO"

