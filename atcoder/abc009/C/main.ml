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

let list_find_opt ~f l = try Some (List.find f l) with Not_found -> None

let string_of_list l =
  let str = Bytes.make (List.length l) ' ' in
  List.iteri (fun i c -> Bytes.set str i c) l ;
  Bytes.to_string str

let list_of_string s =
  Array.init (String.length s) (fun i -> s.[i]) |> Array.to_list

let string_of_list l = String.init (List.length l) (List.nth l)

module Char_set = Set.Make (struct
  type t = char

  let compare = compare
end)

let diff_count s1 s2 =
  let rec loop s1 s2 s1_diff s2_diff =
    match (s1, s2) with
    | [], [] ->
        List.length s1_diff
    | [], c2 :: s2_rest ->
        loop [] s2_rest s1_diff (c2 :: s2_diff)
    | c1 :: s1_rest, [] ->
        loop s1_rest [] (c1 :: s1_diff) s2_diff
    | c1 :: s1, c2 :: s2 ->
        if c1 < c2 then loop s1 (c2 :: s2) (c1 :: s1_diff) s2_diff
        else if c1 > c2 then loop (c1 :: s1) s2 s1_diff (c2 :: s2_diff)
        else loop s1 s2 s1_diff s2_diff
  in
  let l1 = list_of_string s1 |> List.sort compare
  and l2 = list_of_string s2 |> List.sort compare in
  loop l1 l2 [] []

let rec get_close_string target accum rest_chars allow_diff char_index =
  let rest_chars' = List.map snd rest_chars in
  (* Printf.printf
   *   "allow diff: %d, char index: %d, target: %s, accum: %s, rest chars: %s, \
   *    diff: %d\n"
   *   allow_diff char_index target
   *   (List.rev accum |> string_of_list)
   *   (List.map snd rest_chars |> string_of_list)
   *   (string_of_list rest_chars' |> diff_count target) ; *)
  match rest_chars with
  | [] ->
      Some (List.rev accum |> string_of_list)
  | _ -> (
      if char_index >= List.length rest_chars then None
      else if string_of_list rest_chars' |> diff_count target > allow_diff then
        None
      else
        let use_char =
          List.find (fun (i, _) -> i = char_index) rest_chars |> snd
        and rest_chars' =
          List.filter (fun (i, _) -> i <> char_index) rest_chars
          |> List.mapi (fun i v -> (i, snd v))
        in
        let v =
          if use_char <> target.[0] then
            get_close_string
              (String.sub target 1 (String.length target - 1))
              (use_char :: accum) rest_chars'
              (max 0 (pred allow_diff))
              0
          else
            get_close_string
              (String.sub target 1 (String.length target - 1))
              (use_char :: accum) rest_chars' allow_diff 0
        in
        match v with
        | None ->
            get_close_string target accum rest_chars allow_diff
              (succ char_index)
        | Some v ->
            Some v )

let solve original replaceable_count =
  let rest_chars =
    list_of_string original |> List.sort compare
    |> List.mapi (fun i v -> (i, v))
  in
  match get_close_string original [] rest_chars replaceable_count 0 with
  | None ->
      failwith ""
  | Some v ->
      v

let () =
  let _, replaceable_count =
    Scanf.sscanf (read_line ()) "%d %d" (fun v1 v2 -> (v1, v2))
  in
  let original = read_line () in
  let ret = solve original replaceable_count in
  Printf.printf "%s\n" ret
