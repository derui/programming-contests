let str_to_list str =
  let ret = ref [] in
  String.iter (fun v -> ret := Char.escaped v :: !ret) str ;
  List.rev !ret

let () =
  let line = read_line () in
  let list = str_to_list line in
  let list =
    List.filter
      (function "a" | "i" | "u" | "e" | "o" -> false | _ -> true)
      list
  in
  Printf.printf "%s\n" @@ String.concat "" list
