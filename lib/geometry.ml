open Core.Std

type screen = { width : float;
                height : float;
                x_mult : float;
                y_mult : float; }

(* this results in orthographic projection such that the point is placed
   on the screen given *)

let orthogonal_projection s u =
  let (x,y,_) = Vec3.to_tuple u in
  let x = Float.round ((s.width /. 2.) +. (s.x_mult *. x)) in
  let y = Float.round ((s.width /. 2.) +. (s.y_mult *. y)) in
  Float.( (to_int x, to_int y) )
  |> Vec2.of_tuple

(* TODO: make the distinction between world and screen points clearer;
   this needs more thought *)

(* used when transforming from world to screen coordinates;
   it's more handy to have the types be floats, then *)

let map_pair f (x, y) = (f x, f y)

let vec3_of_vertex vx =
  let open Model.Vertex in
  let { x; y; z; w } = vx in
  Vec3.of_tuple (x, y, z)

(* val get_line_points : screen_point -> screen_point -> world_point list *)
(** Returns a list of points constituting a line between the two
    points on the screen **)
let get_line_screen_points sp1 sp2 =
  (* maybe rewrite using only integer points? *)
  let point_count =
    Vec2.(Int.max (Int.abs (sp2.y - sp1.y)) (Int.abs (sp2.x - sp1.x))) in
  let delta = 1.0 /. (float_of_int point_count) in
  List.init point_count
      ~f:(fun i ->
          let t = (float_of_int i) *. delta in

          let (x1,y1) = Vec2.to_tuple (Vec2.vec2f_of_vec2i sp1) in
          let (x2,y2) = Vec2.to_tuple (Vec2.vec2f_of_vec2i sp2) in

          let x = int_of_float (x1 *. (1.0 -. t) +. x2 *. t) in
          let y = int_of_float (y1 *. (1.0 -. t) +. y2 *. t) in
          Vec2.of_tuple (x,y))

let get_empty_triangle_screen_points sp1 sp2 sp3 =
  List.concat
    [get_line_screen_points sp1 sp2;
     get_line_screen_points sp2 sp3;
     get_line_screen_points sp3 sp1]

let get_filled_triangle_screen_points sp1 sp2 sp3 =
  let open Vec2 in
  let order_by_y p1 p2 p3 =
    (* let open Vec2i in *)

    let cmp u v =
      let (_,y1) = Vec2.to_tuple u in
      let (_,y2) = Vec2.to_tuple v in
      compare y2 y1
    in

    let sorted = List.sort [p1;p2;p3] ~cmp:cmp in
    match sorted with
    | [p1;p2;p3] -> (p1,p2,p3)
    | _ -> assert false
  in
  let (btm_s,mid_s,top_s) = order_by_y sp1 sp2 sp3 in
  let origin_s = btm_s in

  (* shift the coordinates so one of the points are the origin *)

  let btm_s = { x = (btm_s.x - btm_s.x);
                     y = (btm_s.y - btm_s.y); } in
  let mid_s = { x = (mid_s.x - btm_s.x);
                     y = (mid_s.y - btm_s.y); } in
  let top_s = { x = (top_s.x - btm_s.x);
                     y = (top_s.y - btm_s.y); } in


  let y_length = Int.abs (top_s.y - btm_s.y) in

  let ps_a = Int.abs (top_s.y - btm_s.y) in
  let ps_b1 = Int.abs (mid_s.y - btm_s.y) in
  let ps_b2 = Int.abs (top_s.y - mid_s.y) in

  (* should write this using only ints *)
  let d_a = 1. /. (float_of_int ps_a) in
  let d_b1 = 1. /. (float_of_int ps_b1) in
  let d_b2 = 1. /. (float_of_int ps_b2) in

  let rows = List.init y_length ~f:(fun i -> i) in
  List.concat (List.map rows
                 ~f:(fun y ->
                     let yf = float_of_int y in
                     let t_a = yf *. d_a in
                     let xa1 = float_of_int btm_s.x in
                     let xa2 = float_of_int top_s.x in
                     let x_a = xa1 *. (1.0 -. t_a) +. xa2 *. t_a in
                     let x_b = if (y < mid_s.y)
                         then
                           let t_b = yf *. d_b1 in
                           let xb1 = xa1 in
                           let xb2 = float_of_int mid_s.x in
                           xb1 *. (1.0 -. t_b) +. xb2 *. t_b
                         else
                           let yf' = yf -. (float_of_int mid_s.y) in
                           let t_b = yf' *. d_b2 in
                           let xb1 = float_of_int mid_s.x in
                           let xb2 = xa2 in
                           xb1 *. (1.0 -. t_b) +. xb2 *. t_b
                     in
                     let sx_1 = (int_of_float x_a) + origin_s.x in
                     let sy_1 = y + origin_s.y in
                     let sx_2 = (int_of_float x_b) + origin_s.x in
                     let sy_2 = y + origin_s.y in
                     let p1 = { x = sx_1; y = sy_1 } in
                     let p2 = { x = sx_2; y = sy_2 } in

                     get_line_screen_points p1 p2
                   ))


(** Returns a list of list of points constituting the lines between all
    vertices given. The vertices represent world coordinates. *)

(* TODO: should this function return world coordinates as well? *)
let get_screen_lines_from_world_vertices s vs =
  List.mapi vs ~f:(fun i v1 ->
      let v2 = if i > 0 then List.nth_exn vs (i-1) else List.last_exn vs in
      let wp1 = vec3_of_vertex v1 in
      let wp2 = vec3_of_vertex v2 in

      let sp1 = orthogonal_projection s wp1 in
      let sp2 = orthogonal_projection s wp2 in
      get_line_screen_points sp1 sp2)

let get_wireframe_screen_lines s f =
  let open Model.Face_vertices in
  List.map f ~f:(fun face ->
      get_screen_lines_from_world_vertices s face.vertices
  )


(* let barycentric_of_cartesian a1 a2 a3 p = *)
