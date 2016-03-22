open Core.Std

(* TODO: refactor the hell outta this *)
(* TODO: Specifically, make it explicit when screen coordinates are used
   and when world coordinates are used, and make it easier to translate
   between them, using some sort of multiplier and offset *)


let get_triangle_points p1 p2 p3 =
  let module R = Renderer in
  List.concat
  [R.get_line_points p1 p2;
   R.get_line_points p2 p3;
   R.get_line_points p3 p1]

let get_filled_triangle p1 p2 p3 =
  let module R = Renderer in
  let [p1;p2;p3] = List.sort [p1;p2;p3]
      ~cmp:(fun (_,y1) (_,y2) -> compare y2 y1)
  in

  let btm_i = p3 in
  let mid_i = p2 in
  let top_i = p1 in
  let o_i = btm_i in
  let o_f = R.point_f_of_i btm_i in

  (* normalize the coordinates; now the points are all with respect to
     the triangle's first point. *)
  let y_length = Int.abs ((snd top_i) - (snd btm_i)) in

  let btm_i = ((fst btm_i) - (fst o_i), (snd btm_i) - (snd o_i)) in
  let mid_i = ((fst mid_i) - (fst o_i), (snd mid_i) - (snd o_i)) in
  let top_i = ((fst top_i) - (fst o_i), (snd top_i) - (snd o_i)) in

  let btm_f = R.point_f_of_i btm_i in
  let mid_f = R.point_f_of_i mid_i in
  let top_f = R.point_f_of_i top_i in

  (* the number of rows making up segment a, b1, b2 *)
  let ps_a = Int.abs ((snd top_i) - (snd btm_i)) in
  let ps_b1 = Int.abs ((snd mid_i) - (snd btm_i)) in
  let ps_b2 = Int.abs ((snd top_i) - (snd mid_i)) in

  (* the step size for each segment *)
  let d_a = 1.0 /. (float_of_int ps_a) in
  let d_b1 = 1.0 /. (float_of_int ps_b1) in
  let d_b2 = 1.0 /. (float_of_int ps_b2) in

  let rows = List.init y_length ~f:(fun i -> i) in
  List.concat (List.map rows
    ~f:(fun y ->
         let t_a = (float_of_int y) *. d_a in
         let (xa1,_) = btm_f in
         let (xa2,_) = top_f in
         let x_a = xa1 *. (1.0 -. t_a) +. xa2 *. t_a in
         (* whether we're drawing segment b1 or b2 *)
         let x_b = if (y < (snd mid_i))
           then
             let t_b = (float_of_int y) *. d_b1 in
             let xb1 = xa1 in
             let (xb2,_) = mid_f in
             xb1 *. (1.0 -. t_b) +. xb2 *. t_b
           else
             let y' = (float_of_int y) -. (snd mid_f) in
             let t_b = y' *. d_b2 in
             let (xb1,_) = mid_f in
             let xb2 = xa2 in
             xb1 *. (1.0 -. t_b) +. xb2 *. t_b
         in
         let p1 = R.point_i_of_f ((x_a) +. (fst o_f),
                                  (float_of_int y) +. (snd o_f)) in
         let p2 = R.point_i_of_f ((x_b) +. (fst o_f),
                                  (float_of_int y) +. (snd o_f)) in

        Renderer.get_line_points p1 p2
         (* for some reason the above leaves holes between triangles...
            so we need to draw a "border" around this triangle as well *)
       )) @ get_triangle_points p1 p2 p3


(* assume we get a triangle, i.e. three vertices *)
let get_triangles_from_vertices vs =
  let module R = Renderer in
  let v1 = vs.(0) in
  let v2 = vs.(1) in
  let v3 = vs.(2) in

  let (x1,y1,_,_) = v1 in
  let (x2,y2,_,_) = v2 in
  let (x3,y3,_,_) = v3 in

  let offset = 450. in
  let mult = 400. in

  let x1 = offset +. (x1 *. mult) in
  let y1 = offset +. (y1 *. mult) in
  let x2 = offset +. (x2 *. mult) in
  let y2 = offset +. (y2 *. mult) in
  let x3 = offset +. (x3 *. mult) in
  let y3 = offset +. (y3 *. mult) in

  let p1 = R.point_i_of_f (x1,y1) in
  let p2 = R.point_i_of_f (x2,y2) in
  let p3 = R.point_i_of_f (x3,y3) in
  [|get_filled_triangle p1 p2 p3|]


let cross_product (ux,uy,uz) (vx,vy,vz) =
  let x = (uy *. vz) -. (uz *. vy) in
  let y = (uz *. vx) -. (ux *. vz) in
  let z = (ux *. vy) -. (uy *. vx) in
  (x,y,z)

let dot_product (ux,uy,uz) (vx,vy,vz) =
  (ux*.vx) +. (uy*.vy) +. (uz*.vz)

let length (ux,uy,uz) =
  ((ux ** 2.) +. (uy ** 2.) +. (uz ** 2.)) ** 0.5

let normalize (ux,uy,uz) =
  let len = length (ux,uy,uz) in
  let x = ux /. len in
  let y = uy /. len in
  let z = uz /. len in
  (x,y,z)

let get_triangle_lighting (x1,y1,z1,_) (x2,y2,z2,_) (x3,y3,z3,_) =
  (* subtract two points from one of the points to get a pair of sides *)
  let u = (x2-.x1,y2-.y1,z2-.z1) in
  let v = (x3-.x1,y3-.y1,z3-.z1) in
  (* calculate cross product of two of the triangle's sides,
     which we can use to find the light intensity *)

  let n = cross_product u v in
  let n' = normalize n in
  let light = (0.,0.,1.) in

  let intensity = dot_product n' light in
  intensity

let get_triangles_list v f =
  let tris =
     (List.map f
        ~f:(fun f' ->
            let vi = Model.get_v_indices_from_face f' in
            let vs = Array.map vi ~f:(fun i ->
                match (List.nth v (i-1)) with
                | Some v' -> v'
                | None -> raise (Failure ("Error: could not find vertex " ^
                                          (string_of_int i)))
              )
            in
            let light = get_triangle_lighting vs.(0) vs.(1) vs.(2) in
            (get_triangles_from_vertices vs, light)
          ))
  in
  (List.map tris ~f:(fun (ar,l) ->
      (List.concat (Array.to_list ar)),l))

(* TODO: Clearly there's something wrong with the function I use to
   find the normals to the triangles. Fix that, or use the normals from
   the model. *)

let () =
  let file = In_channel.create "african_head.obj" in
  let input = In_channel.input_all file in
  let model = Model.parse_model input in
  let (v,t,n,f) = model in

  let tl = get_triangles_list v f in

  let c = Canvas.create_canvas 900 900 in

  List.iter tl ~f:(fun (t,l) ->
      let col = (int_of_float (255. *. l)) in
      if l >= 0. then
      Canvas.draw_list c t (col,col,col));

  Canvas.render_canvas c "test.bmp"
