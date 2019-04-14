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

(* calculate factorial *)
let factorial n =
  let rec laddar' accum n =
    if n = 0 then accum
    else laddar' Big_int.(mult_int_big_int n accum) (pred n)
  in
  laddar' (Big_int.big_int_of_int 1) n

(* calculate combination as nCm *)
let combination n m =
  let n' = factorial n and m' = factorial m and nm' = factorial (n - m) in
  Big_int.(div_big_int n' (mult_big_int m' nm'))

let () =
  let board_size = 4 in
  let line_to_array l =
    Scanf.sscanf l "%c %c %c %c" (fun a b c d -> [|a; b; c; d|])
  in
  let print_board board =
    Array.iter
      (fun row ->
        Printf.printf "%s\n"
          (Array.map Char.escaped row |> Array.to_list |> String.concat " ") )
      board
  in
  let row1 = read_line () |> line_to_array in
  let row2 = read_line () |> line_to_array in
  let row3 = read_line () |> line_to_array in
  let row4 = read_line () |> line_to_array in
  let board = [|row1; row2; row3; row4|] in
  let new_board = Array.init board_size (fun _ -> Array.make board_size 'x') in
  Array.iteri
    (fun y row ->
      Array.iteri
        (fun x v ->
          let y' = abs (y - (board_size - 1))
          and x' = abs (x - (board_size - 1)) in
          new_board.(y').(x') <- v )
        row )
    board ;
  print_board new_board
