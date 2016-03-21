open Core.Std


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

  (* print_endline ("p1.y. " ^ (string_of_int (snd p1))); *)
  (* print_endline ("p2.y. " ^ (string_of_int (snd p2))); *)
  (* print_endline ("p3.y. " ^ (string_of_int (snd p3))); *)

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
       ))


let () =
  (* let file = In_channel.create "african_head.obj" in *)
  (* let input = In_channel.input_all file in *)
  (* let model = Model.parse_model input in *)
  (* let (v,t,n,f) = model in *)

  let t1 = get_filled_triangle (400, 200) (700, 120) (500, 400) in
  let t1' = get_triangle_points (400, 200) (700, 120) (500, 400) in
  let t2 = get_filled_triangle (300, 200) (50, 620) (100, 140) in
  let t2' = get_triangle_points (300, 200) (50, 620) (100, 140) in
  let t3 = get_filled_triangle (100, 700) (800, 860) (300, 550) in
  let t3' = get_triangle_points (100, 700) (800, 860) (300, 550) in


  let c = Canvas.create_canvas 900 900 in
  (* let l = Renderer.get_wireframe_lines v f in *)
  Canvas.draw_list c t1 (255,0,0);
  Canvas.draw_list c t2 (0,255,0);
  Canvas.draw_list c t3 (0,0,255);
  Canvas.draw_list c t1' (255,255,255);
  Canvas.draw_list c t2' (255,255,255);
  Canvas.draw_list c t3' (255,255,255);
  Canvas.render_canvas c "test.bmp"
