let take_list ~num list =
  let rec take_list' count rest accum =
    if count = 0 then List.rev accum
    else
      match rest with
      | [] -> List.rev accum
      | v :: rest -> take_list' (pred count) rest (v :: accum)
  in
  take_list' num list []

let split_on_char ~char ~str =
  let rec split str accum =
    if String.length str = 0 then List.rev accum
    else
      try
        let index = String.index str char in
        let splitted = String.sub str 0 index
        and rest =
          String.sub str (index + 1) (String.length str - index - 1)
        in
        split rest (splitted :: accum)
      with Not_found -> List.rev (str :: accum)
  in
  split str []

let solve rates =
  let rates = List.sort compare rates in
  List.fold_left (fun accum v -> (accum +. v) /. 2.) 0. rates

let () =
  let line = read_line () in
  let _, viewable =
    Scanf.sscanf line "%d %d" (fun total viewable -> (total, viewable))
  in
  let line = read_line () in
  let rates =
    split_on_char ~char:' ' ~str:line
    |> List.map float_of_string |> List.sort compare |> List.rev
    |> take_list ~num:viewable
  in
  let my_rate = solve rates in
  Printf.printf "%f\n" my_rate
