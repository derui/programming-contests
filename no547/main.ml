let split_word str =
  let regexp = Str.regexp " " in
  Str.split regexp str

let compare_words s t =
  let ret = ref (0, "", "") in 
  List.combine s t
  |> List.iteri (fun index (s, t) ->
         if s <> t then ret := (index, s, t) else ()
       );
  !ret


let () =
  let _ = read_line () |> int_of_string in
  let s_words = read_line () |> split_word in
  let t_words = read_line () |> split_word in

  let index, s_diff, t_diff = compare_words s_words t_words in
  string_of_int (succ index) |> print_endline;
  print_endline s_diff;
  print_endline t_diff;
