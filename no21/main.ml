
let solve num_group numbers =
  let sorted = List.sort compare numbers in
  let rev_sorted = List.rev sorted in
  let min' = List.hd sorted
  and max' = List.hd rev_sorted in
  max' - min'

let () =
  let count = read_line () |> int_of_string in
  let count_of_groups = read_line () |> int_of_string in
  let rec loop counter list =
    if counter = 0 then list
    else let number = read_line() |> int_of_string in
         loop (pred counter) (number :: list)
  in
  let numbers = loop count [] in
  Printf.printf "%d\n" (solve count_of_groups numbers)
