
type dir = N | E | W | S

let move_with_dir (x, y) pitch = function
  | N -> (x, y + pitch)
  | W -> (x - pitch, y)
  | E -> (x + pitch, y)
  | S -> (x, y - pitch)

let turn_for_x v current =
  let to_west = v < 0
  and to_east = v > 0 in
  match current with
  | N when to_east -> Some (E, 1)
  | N when to_west -> Some (W, 1)
  | S when to_east -> Some (E, 1)
  | S when to_west -> Some (W, 1)
  | E when to_west -> Some (W, 2)
  | W when to_east -> Some (E, 2)
  | _ -> None

let turn_for_y v  current =
  let to_south = v < 0
  and to_north = v > 0 in
  match current with
  | E when to_south -> Some (S, 1)
  | E when to_north -> Some (N, 1)
  | W when to_south -> Some (S, 1)
  | W when to_north -> Some (N, 1)
  | N when to_south -> Some (S, 2)
  | S when to_north -> Some (N, 2)
  | _ -> None

let solve tag_x tag_y max_pitch =
  let move_to_x x dir =
      let dir, act_count =
        match turn_for_x tag_x dir with
        | None -> dir, 0
        | Some v -> v in
      let quo = (abs tag_x) / max_pitch
      and rem = (abs tag_x) mod max_pitch in
      let rem = if rem > 0 then 1 else 0 in
      dir, (act_count + quo + rem)
  and move_to_y y dir =
      let dir, act_count =
        match turn_for_y tag_y dir with
        | None -> dir, 0
        | Some v -> v in
      let quo = (abs tag_y) / max_pitch
      and rem = (abs tag_y) mod max_pitch in
      let rem = if rem > 0 then 1 else 0 in
      dir, (act_count + quo + rem)
  in
  let rec solve' (x, y) dir count =
    if (x, y) = (tag_x, tag_y) then count
    else if x = tag_x then
      let dir, act_count = move_to_y y dir in
      solve' (x, tag_y) dir (count + act_count)
    else if y = tag_y then
      let dir, act_count = move_to_x x dir in
      solve' (tag_x, y) dir (count + act_count)
    else
      let dir_x, act_count_x = move_to_x x dir
      and dir_y, act_count_y = move_to_y y dir in
      if dir_y = dir then
        solve' (x, tag_y) dir_y (count + act_count_y)
      else
        solve' (tag_x, y) dir_x (count + act_count_x)
  in
  solve' (0, 0) N 0

let () =
  let x_pos = read_line () |> int_of_string in
  let y_pos = read_line () |> int_of_string in
  let max_movable = read_line () |> int_of_string in
  Printf.printf "%d\n" (solve x_pos y_pos max_movable)
