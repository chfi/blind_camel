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

  (* let seg_a = (p1,p3) in *)
  (* let seg_b1 = (p1,p2) in *)
  (* let seg_b2 = (p2,p3) in *)


  (* TODO: rewrite this, think. *)

  let rows = List.init (Int.abs ((snd p3)-(snd p1))) ~f:(fun i -> i+(snd p1)) in
  List.concat (List.map rows
    ~f:(fun y ->
         (* just fucking rewrite this shit goddamn it *)

        let y = float_of_int y in
        (* the top (bottom?) point *)
        let x1 = float_of_int (fst p1) in
        let y1 = float_of_int (snd p1) in
        let y' = y -. y1 in
        (* the left-most point in the triangle *)
        (* let l = float_of_int (Int.min (fst p2) (fst p3)) in *)
        let l = if (fst p2) < (fst p3) then p2 else p3 in
        let l = R.point_f_of_i l in

        let r = if (fst p2) < (fst p3) then p3 else p2 in
        let r = R.point_f_of_i r in

        let yl' = Float.abs (y1 -. (snd l)) in
        let yr' = Float.abs (y1 -. (snd r)) in

        let xl' = Float.abs (x1 -. (fst l)) in
        let xr' = Float.abs (x1 -. (fst r)) in

        let d_l = (y' /. yl') *. xl' in
        let d_r = (y' /. yr') *. xr' in

        let x_l = x1 -. d_l in
        let x_r = x1 +. d_r in
        print_endline ("y_l: " ^ (string_of_float (y /. yl')));
        print_endline ("y_r: " ^ (string_of_float (y /. yr')));
        print_endline ("d_l: " ^ (string_of_float d_l));
        print_endline ("d_r: " ^ (string_of_float d_r));
        let p1' = R.point_i_of_f (x_l,900.-.y) in
        let p2' = R.point_i_of_f (x_r,900.-.y) in
        Renderer.get_line_points p1' p2'
      ))


let () =
  (* let file = In_channel.create "african_head.obj" in *)
  (* let input = In_channel.input_all file in *)
  (* let model = Model.parse_model input in *)
  (* let (v,t,n,f) = model in *)

  let t1 = get_filled_triangle (200, 200) (500, 220) (300, 400) in
  let t2 = get_triangle_points (200, 200) (500, 220) (300, 400) in

  let c = Canvas.create_canvas 900 900 in
  (* let l = Renderer.get_wireframe_lines v f in *)
  Canvas.draw_list c t1 (255,0,0);
  Canvas.draw_list c t2 (255,255,255);
  Canvas.render_canvas c "test.bmp"
