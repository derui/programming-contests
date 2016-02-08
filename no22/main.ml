type paren = Open | Close

let solve position parens =
  let base_paren = String.get parens (position - 1) in
  let start, manipulator, initial = match base_paren with
    | '(' -> (position, succ, 1)
    | ')' -> (position - 2, pred, -1)
    | _ -> failwith ""
  in
  let rec loop next correspond pos =
    if correspond = 0 then pos + 1
    else let paren = String.get parens next in
         match paren with
         | '(' -> loop (manipulator next) (succ correspond) next
         | ')' -> loop (manipulator next) (pred correspond) next
         | _ -> failwith ""
  in
  loop start initial position

let () =
  let info = read_line () in
  let _, target = Scanf.sscanf info "%d %d" (fun len target -> (len, target)) in
  let parens = read_line () in
  Printf.printf "%d\n" (solve target parens)
