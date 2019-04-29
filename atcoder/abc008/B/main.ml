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

let () =
  let rows = read_line () |> int_of_string in
  let rec read' current accum =
    if current >= rows then accum
    else
      let line = read_line () in
      read' (succ current) (line :: accum)
  in
  let mans = read' 0 [] in
  let hash : (string, int) Hashtbl.t = Hashtbl.create 0 in
  List.iter
    (fun v ->
      let count = if Hashtbl.mem hash v then Hashtbl.find hash v else 0 in
      Hashtbl.add hash v (succ count) )
    mans ;
  let name, _ =
    Hashtbl.fold
      (fun key value (name, max_count) ->
        if value > max_count then (key, value) else (name, max_count) )
      hash ("", 0)
  in
  Printf.printf "%s\n" name
