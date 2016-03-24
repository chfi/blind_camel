open Core.Std

type world_point = Wp of float * float * float

type screen_point = Sp of int * int

(* TODO: make the distinction between world and screen points clearer;
   this needs more thought *)

(* used when transforming from world to screen coordinates;
   it's more handy to have the types be floats, then *)
type screen = { width : float;
                height : float;
                x_mult : float;
                y_mult : float; }

let map_pair f (x, y) = (f x, f y)

let world_point_of_vertex (x,y,z,w) = Wp (x,y,z)

(* this results in orthographic projection *)
let screen_of_world s wp =
  let Wp (x,y,_) = wp in
  let sx = Float.round ((s.width /. 2.) +. (s.x_mult *. x)) in
  let sy = Float.round ((s.height /. 2.) +. (s.y_mult *. y)) in
  let (i_sx,i_sy) = map_pair int_of_float (sx,sy) in
  Sp (i_sx,i_sy)

(* val get_line_points : screen_point -> screen_point -> world_point list *)
(** Returns a list of points constituting a line between the two
    points on the screen **)
let get_line_screen_points p1 p2 =
  let Sp (x1, y1) = p1 in
  let Sp (x2, y2) = p2 in
  let point_count = (Int.max (Int.abs (y2 - y1)) (Int.abs (x2 - x1))) in
  let delta = 1.0 /. (float_of_int point_count) in
  List.init point_count
      ~f:(fun i ->
          let t = (float_of_int i) *. delta in
          let (x1,y1) = map_pair float_of_int (x1,y1) in
          let (x2,y2) = map_pair float_of_int (x2,y2) in
          let x = x1 *. (1.0 -. t) +. x2 *. t in
          let y = y1 *. (1.0 -. t) +. y2 *. t in
          map_pair int_of_float (x,y))

(** Returns a list of list of points constituting the lines between all
    vertices given. The vertices represent world coordinates. *)

(* TODO: should this function return world coordinates as well? *)
let get_lines_from_world_vertices s vs =
  Array.mapi vs ~f:(fun i v1 ->
      let v2 =
        if i > 0 then
          vs.(i-1)
        else
          Array.nget vs (-1)
      in
      let wp1 = world_point_of_vertex v1 in
      let wp2 = world_point_of_vertex v2 in

      let sp1 = screen_of_world s wp1 in
      let sp2 = screen_of_world s wp2 in
      get_line_screen_points sp1 sp2)

