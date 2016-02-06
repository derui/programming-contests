
let length_of_alphabet = 26
let first_of_alphabet = int_of_char 'A'

let decrypt ind c =
  let amount = (ind + 1) mod length_of_alphabet in
  let c = int_of_char c in
  let slided = c - amount in
  let slided = if slided < first_of_alphabet then (int_of_char 'Z') - (first_of_alphabet - slided - 1) else slided in
  char_of_int slided

let () =
  let crypted = read_line () in
  let result = String.make (String.length crypted) ' ' in
  String.iteri (fun ind c ->
                   let c = decrypt ind c in
                   result.[ind] <- c
                 ) crypted;
  print_string result
