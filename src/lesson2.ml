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

  let seg_a = (p1,p3) in
  let seg_b1 = (p1,p2) in
  let seg_b2 = (p2,p3) in
  let rows = List.init ((snd p3)-(snd p1)) ~f:(fun i -> i+(snd p1)) in
  List.map rows
    ~f:(fun y ->
        let y = float_of_int y in
        (* this is a clusterfuck and i can't think atm *)
        let left = Int.min (fst p2) (fst p3) in
        let right = Int.max (fst p2) (fst p3) in
        let d_l = y. /. ((float_of_int left) -. x1) in
        let d_r = y. /. ((float_of_int right) -. x2) in
        if y < snd p2 then

          let x1 = float_of_int (fst p1) in
          let y1 = float_of_int (snd p1) in
          let x2 = float_of_int (fst p2) in
          let y2 = float_of_int (snd p2) in
          let x3 = float_of_int (fst p3) in
          let y3 = float_of_int (snd p3) in
          let d1 = y /. ((float_of_int left) -. x1) in
          let d2 = y /. ((float_of_int right) -. x1) in

          let xa = x1 -. (d_l *. 

          let d = y /. Float.abs (y2 -. y1) in
          let xa = x1 -. d *. Float.abs (x2 -. x1) in
          let xb = x1 

        else






let () =
  (* let file = In_channel.create "african_head.obj" in *)
  (* let input = In_channel.input_all file in *)
  (* let model = Model.parse_model input in *)
  (* let (v,t,n,f) = model in *)

  let t1 = get_triangle_points (200, 200) (500, 220) (700, 400) in



  let c = Canvas.create_canvas 900 900 in
  (* let l = Renderer.get_wireframe_lines v f in *)
  Canvas.draw_list c t1 (255,0,0);
  Canvas.render_canvas c "test.bmp"
