open Core.Std

(* represents a model parsed from a wavefront .obj file *)

(* lets start with just reading vertices *)
type vertex = (float * float * float * float)
type texture_vertex = (float * float * float)
type normal_vertex = (float * float * float)

type face = (int * int option * int option) array

(* todo deal with this better *)
let f_pair_from_s_list l =
  let u = float_of_string (List.nth_exn l 0) in
  let v = float_of_string (List.nth_exn l 1) in
  (u,v)

let f_triple_from_s_list l =
  let x = float_of_string (List.nth_exn l 0) in
  let y = float_of_string (List.nth_exn l 1) in
  let z = float_of_string (List.nth_exn l 2) in
  (x,y,z)

let f_quad_from_s_list l =
  let u = float_of_string (List.nth_exn l 0) in
  let v = float_of_string (List.nth_exn l 1) in
  let w = float_of_string (List.nth_exn l 2) in
  let x = float_of_string (List.nth_exn l 3) in
  (u,v,w,x)

let create_vertex words =
  if (List.length words) = 4 then
    f_quad_from_s_list words
  else
  let (x,y,z) = f_triple_from_s_list words in
  (x,y,z,1.0)

let create_t_vertex words =
  if (List.length words) = 3 then
    f_triple_from_s_list words
  else
    let (u,v) = f_pair_from_s_list words in
    (u,v,0.0)

let create_n_vertex words =
  f_triple_from_s_list words

let create_face words =
  let parse_vertex word =
    let split = String.split ~on:'/' word in
    let v = int_of_string (List.nth_exn split 0) in
    let vt = Option.map (List.nth split 1) ~f:int_of_string in
    let vn = Option.map (List.nth split 2) ~f:int_of_string in
    (v,vt,vn)
  in
  let parsed = List.map words ~f:parse_vertex in
  Array.of_list parsed

(* TODO: deal with lines being of wrong length, i.e. having too few/too many entries *)

let parse_line line so_far =
  let (vs,vts,vns,fs) = so_far in
  (* split the line into words, first *)
  let words =
    (* and deal with any double-spaces (TODO: make this better.) *)
    List.filter ~f:(fun s -> s <> "") (
    String.split_on_chars ~on:[' ';'\t'] line) in
  let rest = List.slice words 1 0 in
  let result = match List.hd words with
    | None -> raise (Invalid_argument "Line empty!") (* TODO: handle this better *)
    | Some "v" -> (vs @ [create_vertex rest], vts, vns, fs)
    | Some "vt" -> (vs, vts @ [create_t_vertex rest], vns, fs)
    | Some "vn" -> (vs, vts, vns @ [create_n_vertex rest], fs)
    | Some "f" -> (vs, vts, vns, fs @ [create_face rest])
    | _ -> so_far
  in
  result

(* TODO: make this output an array, and make parse_line reverse the lists instead
   of appending to the end *)
let parse_model file =
  let lines = String.split_lines file in
  (* pop out empty lines and comments *)
  let filtered = List.filter lines ~f:(fun l ->
      (String.length l) > 0 && (l.[0] <> '#')) in
  List.fold filtered ~init:([],[],[],[]) ~f:(fun acc line -> parse_line line acc)


let show_vertex (x,y,z,w) =
  "(" ^ (string_of_float x) ^ ", " ^
  (string_of_float y) ^ ", " ^ (string_of_float z) ^
  (string_of_float w) ^ ")"

let show_t_vertex (x,y,z) =
  "(" ^ (string_of_float x) ^ ", " ^
  (string_of_float y) ^ ", " ^
  (string_of_float z) ^ ")"

let show_n_vertex (x,y,z) =
  "(" ^ (string_of_float x) ^ ", " ^
  (string_of_float y) ^ ", " ^
  (string_of_float z) ^ ")"

