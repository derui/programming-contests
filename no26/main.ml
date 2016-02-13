type mark = C | X

let cups = [|C ; C ; C|]

let swap (f, t) =
  let v = cups.(f - 1) in
  let v' = cups.(t - 1) in
  cups.(f - 1) <- v';
  cups.(t - 1) <- v

let solve () =
  let cups = Array.to_list cups in
  let index = ref 0 in
  List.iteri (fun ind v -> if v = X then index := ind + 1 else ()) cups;
  !index

let () =
  let marking = read_line () |> int_of_string in
  let count_of_swap = read_line () |> int_of_string in
  let rec loop counter list =
    if counter = 0 then ()
    else
      let target = Scanf.sscanf (read_line ()) "%d %d" (fun f t -> (f, t)) in
      swap target;
      loop (pred counter) (target :: list)
  in
  cups.(marking - 1) <- X;
  loop count_of_swap [];
  Printf.printf "%d\n" (solve ())
