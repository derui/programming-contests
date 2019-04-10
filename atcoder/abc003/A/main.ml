let () =
  let line = read_line () in
  Scanf.sscanf line "%d" (fun task_count ->
      let task_count_f = float_of_int task_count in
      let mean =
        (1.0 +. task_count_f) *. (task_count_f /. 2.0) /. task_count_f
      in
      let mean = string_of_float (mean *. 10000.0) in
      let mean =
        if mean.[String.length mean - 1] = '.' then
          String.sub mean 0 (String.length mean - 1)
        else mean
      in
      Printf.printf "%s\n" mean )
