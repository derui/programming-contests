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
  let slide_right origin =
    let head = origin.(0) in
    Array.init (Array.length origin) (fun index ->
        if index = Array.length origin - 1 then head else origin.(index + 1) )
  in
  let rec slide_by_count array count =
    if count = 0 then array
    else slide_by_count (slide_right array) (pred count)
  in
  let swap_by_count array count =
    let rec swap' array current =
      if current >= count then array
      else
        let base_index = current mod 5 in
        let from_i = base_index and to_i = base_index + 1 in
        let a = array.(from_i) and b = array.(to_i) in
        array.(from_i) <- b ;
        array.(to_i) <- a ;
        swap' array (succ current)
    in
    swap' array 0
  in
  let line = read_line () in
  Scanf.sscanf line "%Ld" (fun count ->
      let slide_count = Int64.rem count (Int64.of_int 30)
      and swap_count = Int64.rem count (Int64.of_int 5) |> Int64.to_int in
      let slide_count = Int64.div slide_count (Int64.of_int 5) in
      let slided_array =
        slide_by_count
          [|"1"; "2"; "3"; "4"; "5"; "6"|]
          (Int64.to_int slide_count)
      in
      let swapped_array = swap_by_count slided_array swap_count in
      Printf.printf "%s\n" (Array.to_list swapped_array |> String.concat "") )
