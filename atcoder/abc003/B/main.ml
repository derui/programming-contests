let solve first second =
  let size = String.length first in
  let match_char target c =
    match c with
    | '@' -> (
        match target with
        | 'a' | 't' | 'c' | 'o' | 'd' | 'e' | 'r' | '@' -> true
        | _ -> false )
    | _ when target = c -> true
    | _ -> false
  in
  let rec match_string index =
    if index >= size then true
    else
      let fc = first.[index] and sc = second.[index] in
      if match_char fc sc || match_char sc fc then match_string (succ index)
      else false
  in
  match_string 0

let () =
  let first_line = read_line () in
  let second_line = read_line () in
  match solve first_line second_line with
  | true -> Printf.printf "You can win\n"
  | false -> Printf.printf "You will lose\n"
