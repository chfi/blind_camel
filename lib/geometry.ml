open Core.Std

(* type world_point = Wp of float * float * float *)
type world_point = { wx : float;
                     wy : float;
                     wz : float; }

(* type screen_point = Sp of int * int *)
type screen_point = { sx : int;
                      sy : int; }

(* TODO: make the distinction between world and screen points clearer;
   this needs more thought *)

(* used when transforming from world to screen coordinates;
   it's more handy to have the types be floats, then *)
type screen = { width : float;
                height : float;
                x_mult : float;
                y_mult : float; }

let map_pair f (x, y) = (f x, f y)

(* let world_point_of_vertex (x,y,z,w) = Wp (x,y,z) *)
let world_point_of_vertex (wx,wy,wz,w) = { wx; wy; wz }

(* this results in orthographic projection *)
let screen_of_world s wp =
  let f_sx = Float.round ((s.width /. 2.) +. (s.x_mult *. wp.wx)) in
  let f_sy = Float.round ((s.height /. 2.) +. (s.y_mult *. wp.wy)) in
  let (sx,sy) = map_pair int_of_float (f_sx,f_sy) in
  { sx; sy }

(* val get_line_points : screen_point -> screen_point -> world_point list *)
(** Returns a list of points constituting a line between the two
    points on the screen **)
let get_line_screen_points sp1 sp2 =
  (* maybe rewrite using only integer points? *)
  let point_count =
    (Int.max (Int.abs (sp2.sy - sp1.sy)) (Int.abs (sp2.sx - sp1.sx))) in
  let delta = 1.0 /. (float_of_int point_count) in
  List.init point_count
      ~f:(fun i ->
          let t = (float_of_int i) *. delta in
          let (x1,y1) = map_pair float_of_int (sp1.sx,sp1.sy) in
          let (x2,y2) = map_pair float_of_int (sp2.sx,sp2.sy) in
          let sx = int_of_float (x1 *. (1.0 -. t) +. x2 *. t) in
          let sy = int_of_float (y1 *. (1.0 -. t) +. y2 *. t) in
          { sx; sy })

let get_empty_triangle_screen_points sp1 sp2 sp3 =
  List.concat
    [get_line_screen_points sp1 sp2;
     get_line_screen_points sp2 sp3;
     get_line_screen_points sp3 sp1]


(** Returns a list of list of points constituting the lines between all
    vertices given. The vertices represent world coordinates. *)

(* TODO: should this function return world coordinates as well? *)
let get_screen_lines_from_world_vertices s vs =
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


let get_wireframe_screen_lines s v f =
  let get_vertex_from_index i = match (List.nth v (i-1)) with
    | Some v' -> v'
    | None -> raise (Failure ("Error: could not find vertex " ^
                              (string_of_int i)))
  in
  let get_vertices_from_indices vi =
    Array.map vi ~f:get_vertex_from_index
  in

  List.map f
    ~f:(fun face ->
        let vi = Model.get_v_indices_from_face face in
        let vs = get_vertices_from_indices vi in
        get_screen_lines_from_world_vertices s vs)

