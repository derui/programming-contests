let make_matrix n = Array.init n (fun _ -> Array.make n false)

let loop f accum count =
  let rec inner_loop accum current =
    if current >= count then accum
    else
      let accum = f accum current in
      inner_loop accum (succ current)
  in
  inner_loop accum 0

let count_max_relations matrix =
  let size = Array.length matrix in
  let has_bit pat v = (pat lsr v) land 1 = 1 in
  let bit_count v =
    let rec bit_count v accum =
      if v = 0 then accum else bit_count (v lsr 1) (succ accum)
    in
    bit_count v 0
  in
  loop
    (fun accum current_pattern ->
       Printf.printf "pat: %d, accum %d\n" current_pattern accum ;
       let count = bit_count current_pattern in
       if accum >= count then accum
       else
         let ret =
           loop
             (fun _ i ->
                loop
                  (fun accum j ->
                     Printf.printf
                       "pat: %d: i: %d, j: %d, bit(i): %b, bit(j): %b, matrix: %b\n"
                       current_pattern i j
                       (has_bit current_pattern i)
                       (has_bit current_pattern j)
                       matrix.(i).(j) ;
                     if
                       has_bit current_pattern i && has_bit current_pattern j
                       && not matrix.(i).(j)
                     then false
                     else accum )
                  true i )
             false size
         in
         if ret then count else accum )
    0 (1 lsl size)

let () =
  let line = read_line () in
  let matrix, relations =
    Scanf.sscanf line "%d %d" (fun humans relations ->
        (make_matrix humans, relations) )
  in
  let rec read_data count =
    if count <= 0 then ()
    else
      let line = read_line () in
      Scanf.sscanf line "%d %d" (fun x y ->
          matrix.(x - 1).(y - 1) <- true ;
          matrix.(y - 1).(x - 1) <- true ) ;
      read_data (pred count)
  in
  read_data relations ;
  Printf.printf "%d\n" @@ count_max_relations matrix
