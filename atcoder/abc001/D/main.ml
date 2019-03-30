type mark = N | M

type rainy_time =
  {start_hour: int; start_minute: int; end_hour: int; end_minute: int}

let minute_unit = 5

let to_rainy_range time =
  let time_to_index hour minute = ((hour * 60) + minute) / minute_unit in
  ( time_to_index time.start_hour time.start_minute
  , time_to_index time.end_hour time.end_minute )

let correct_time v =
  let {start_minute; end_minute} = v in
  let rec correct_start_minute current v =
    if current > v then current - minute_unit
    else correct_start_minute (current + minute_unit) v
  in
  let rec correct_end_minute current v =
    if current < v then current + minute_unit
    else correct_end_minute (current - minute_unit) v
  in
  { v with
    start_minute= correct_start_minute 0 start_minute
  ; end_minute= correct_end_minute 60 end_minute }

let rec read_lines line data =
  if line <= 0 then data
  else
    let line_str = read_line () in
    let time =
      Scanf.sscanf line_str "%02d%02d-%02d%02d"
        (fun start_hour start_minute end_hour end_minute ->
           correct_time {start_hour; start_minute; end_minute; end_hour} )
    in
    read_lines (pred line) (time :: data)

let () =
  let marks = Array.make (24 * 60 / 5) N in
  let len = read_line () |> int_of_string in
  let lines = read_lines len [] in
  List.iter
    (fun line ->
       let si, ei = to_rainy_range line in
       Array.fill marks si (ei - si) M )
    lines ;
  let rec marks_to_times current start ranges =
    if current >= Array.length marks then
      if start = -1 then List.rev ranges
      else List.rev ((start, Array.length marks) :: ranges)
    else
      let mark = marks.(current) in
      match mark with
      | N ->
        if start = -1 then marks_to_times (succ current) start ranges
        else marks_to_times (succ current) (-1) ((start, current) :: ranges)
      | M ->
        marks_to_times (succ current)
          (if start = -1 then current else start)
          ranges
  in
  let times = marks_to_times 0 (-1) [] in
  List.iter
    (fun (s, e) ->
       let start_hour = s * 5 / 60
       and start_minute = s * 5 mod 60
       and end_hour = e * 5 / 60
       and end_minute = e * 5 mod 60 in
       Printf.printf "%02d%02d-%02d%02d\n" start_hour start_minute end_hour
         end_minute )
    times
