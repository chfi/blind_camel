open Core.Std

type point_f = (float * float)
type point_i = (float * float)

let point_f_of_i (x,y) = ((float_of_int x),(float_of_int y))
let point_i_of_f (x,y) = (int_of_float (Float.round x),
                          int_of_float (Float.round y))

let get_line_points (x1,y1) (x2,y2) =
  let point_count = (Int.max (Int.abs (y2 - y1)) (Int.abs (x2 - x1)))  in
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

let get_v_indices_from_face face =
  Array.map face ~f:(fun (v,_,_) -> v)

(* draws a line from each vertex to the next *)
let get_lines_from_vertices vs =
  Array.mapi vs ~f:(fun i v ->
      let v2 =
        if i > 0 then
          vs.(i-1)
        else
          Array.nget vs (-1)
      in
      let (x1,y1,_,_) = v in
      let (x2,y2,_,_) = v2 in
      let x1 = 450. +. (x1 *. 400.) in
      let y1 = 450. +. (y1 *. 400.) in
      let x2 = 450. +. (x2 *. 400.) in
      let y2 = 450. +. (y2 *. 400.) in
      let (x1,y1) = point_i_of_f (x1,y1) in
      let (x2,y2) = point_i_of_f (x2,y2) in
      get_line_points (x1,y1) (x2,y2))

let get_wireframe_lines v f =
  List.concat
    (Array.to_list
        (Array.concat
           (List.map f
              ~f:(fun f' ->
                  let vi = get_v_indices_from_face f' in
                  let vs = Array.map vi ~f:(fun i ->
                      match (List.nth v (i-1)) with
                      | Some v' -> v'
                      | None -> raise (Failure ("Error: could not find vertex " ^
                                                (string_of_int i)))
                    )
                  in
                  get_lines_from_vertices vs
                ))))

let () =
  let file = In_channel.create "african_head.obj" in
  let input = In_channel.input_all file in

  let model = Model.parse_model input in

  (* ignore (Vec3.of_tuple (1.,2.,2.)); *)

  let c = Canvas.create_canvas 900 900 in
  (* let l = Geometry. *)
  (* let l = Renderer.get_wireframe_lines v f in *)
  (*
  Canvas.draw_list c l (255,255,255);
  Canvas.render_canvas c "test.bmp"
     *)
  ()
