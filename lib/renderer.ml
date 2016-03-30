open Core.Std
(* TODO: Rewrite, use more types, split into more modules if necessary *)

(* TODO: make these records instead; p1.x would be easier than fst p1 *)
type point_f = (float * float)
type point_i = (int * int)

type world_coordinates = (float * float * float)
type screen_coordinates = (int * int)

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


(* draws a line from each vertex to the next *)
(* TODO: fix the hardcoded sizes *)
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


let get_wireframe_lines vertices faces =
  List.map faces ~f:(fun f' ->
      Model.get_v_indices_from_face f'
      |> Array.map ~f:(fun i ->
          match (List.nth vertices (i-1)) with
          | Some v' -> v'
          | None ->
            raise (Failure ("Error: could not find vertex " ^ (string_of_int i))))
      |> get_lines_from_vertices)
  |> Array.concat
  |> Array.to_list
  |> List.concat

