open Core.Std

type point_f = (float * float)
type point_i = (float * float)

let point_f_of_i (x,y) = ((float_of_int x),(float_of_int y))
let point_i_of_f (x,y) = (int_of_float (Float.round x),
                          int_of_float (Float.round y))

let get_line_points (x1,y1) (x2,y2) =
  let point_count = (Int.max (Int.abs (y2 - y1)) (Int.abs (x2 - x1)))  in
  print_endline ("# points: " ^ (string_of_int point_count));
  let delta = 1.0 /. (float_of_int point_count) in
  let points = List.init point_count
      ~f:(fun i ->
          let t = (float_of_int i) *. delta in
          let (x1,y1) = point_f_of_i (x1,y1) in
          let (x2,y2) = point_f_of_i (x2,y2) in
          let x = x1 *. (1.0 -. t) +. x2 *. t in
          let y = y1 *. (1.0 -. t) +. y2 *. t in
          point_i_of_f (x,y))
  in
  points

(*
   TODO: write code to simplify transition between floats and ints;
   use floats everywhere except on the canvas side, i.e., convert from
   floats to int in set_canvas_pixel, maybe? floats will likely
   simplify the math, so they're nice to have.
*)

let () =
  let points = get_line_points (100,100) (400,400) in
  let points' = get_line_points (130,100) (333, 200) in
  let c = Canvas.create_canvas 500 500 in

  Canvas.draw_list c points (255,255,255);
  Canvas.draw_list c points' (255,0,0);
  Canvas.render_canvas c "test.bmp"

