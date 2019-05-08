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

module Int_set = Set.Make (struct
  type t = int

  let compare = compare
end)

let read_lines count ~f =
  let rec read_lines' current accum =
    if current >= count then List.rev accum
    else
      let line = read_line () |> f in
      read_lines' (succ current) (line :: accum)
  in
  read_lines' 0 []

let () =
  let count = read_line () |> int_of_string in
  let prices = read_lines count ~f:int_of_string in
  let set =
    List.fold_left (fun accum v -> Int_set.add v accum) Int_set.empty prices
  in
  let prices = Int_set.fold (fun v accum -> v :: accum) set [] in
  let price = List.sort compare prices |> List.rev |> List.tl |> List.hd in
  Printf.printf "%d\n" price
