let take_list ~num list =
  let rec take_list' count rest accum =
    if count = 0 then List.rev accum
    else
      match rest with
      | [] ->
          List.rev accum
      | v :: rest ->
          take_list' (pred count) rest (v :: accum)
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

type block = Free | Wall

let read_maze rows =
  let rec read' current accm =
    if current >= rows then List.rev accm
    else
      let row = ref [] in
      let line = read_line () in
      String.iter
        (function
          | '.' ->
              row := Free :: !row
          | '#' ->
              row := Wall :: !row
          | _ ->
              failwith "unknown character" )
        line ;
      read' (succ current) (List.rev !row :: accm)
  in
  read' 0 []

let around_frees maze ~x ~y =
  let arounds = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)] in
  List.map (fun (x, y) -> (x, y, List.nth (List.nth maze y) x)) arounds
  |> List.filter (function _, _, Free -> true | _, _, _ -> false)

module Hand_map = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let solve ~maze ~rows ~columns ~start:(sx, sy) ~goal:(gx, gy) =
  let queue : (int * int) Queue.t = Queue.create () in
  let hand_map : int Hand_map.t = Hand_map.empty in
  let rec solve' hand_map =
    if Queue.is_empty queue then hand_map
    else
      let x, y = Queue.take queue in
      let arounds = around_frees maze ~x ~y in
      let cell_hand = Hand_map.find (x, y) hand_map in
      let hand_map =
        List.fold_left
          (fun map (x, y, _) ->
            if not (Hand_map.mem (x, y) map) then (
              Queue.push (x, y) queue ;
              Hand_map.add (x, y) (succ cell_hand) map )
            else map )
          hand_map arounds
      in
      solve' hand_map
  in
  let hand_map = Hand_map.add (sx, sy) 0 hand_map in
  Queue.add (sx, sy) queue ;
  solve' hand_map |> Hand_map.find (gx, gy)

let () =
  let rows, columns =
    Scanf.sscanf (read_line ()) "%d %d" (fun v1 v2 -> (v1, v2))
  in
  let starty, startx =
    Scanf.sscanf (read_line ()) "%d %d" (fun v1 v2 -> (pred v1, pred v2))
  in
  let goaly, goalx =
    Scanf.sscanf (read_line ()) "%d %d" (fun v1 v2 -> (pred v1, pred v2))
  in
  let maze = read_maze rows in
  let result =
    solve ~maze ~rows ~columns ~start:(startx, starty) ~goal:(goalx, goaly)
  in
  Printf.printf "%d\n" result
