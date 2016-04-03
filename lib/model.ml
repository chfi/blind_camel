open Core.Std
(* TODO: Rewrite, use more types, split into more modules if necessary *)

(* TODO: this whole thing should be rewritten so that a face actually contains
   vertices, instead of just indices to vertices. I mean, that'll be inefficient,
   but w/e; it'll be nicer to work with. *)

(* represents a model parsed from a wavefront .obj file *)

(* lets start with just reading vertices *)

module Vertex = struct
  type t = { x : float;
             y : float;
             z : float;
             w : float; }

  let of_tuple (x,y,z,w) =
    let w = match w with
      | Some w -> w
      | None -> 1.
    in
    { x; y; z; w }
end

module Texture_coordinate = struct
  type t = { u : float;
             v : float;
             w : float; }

  let of_tuple (u,v,w) =
    let w = match w with
      | Some w -> w
      | None -> 0.
    in
    { u; v; w }
end

module Vertex_normal = struct
  type t = { x : float;
             y : float;
             z : float; }

  let of_tuple (x,y,z) = { x; y; z }
end

module Face_indices = struct
  type t = { vertex_indices : int list;
             texture_indices : int list;
             normal_indices : int list; }

  let of_tuple_list l =
    let vertex_indices = List.map l ~f:(fun (v,_,_) -> v) in
    let texture_indices = List.filter_map l ~f:(fun (_,tc,_) -> tc) in
    let normal_indices = List.filter_map l ~f:(fun (_,_,vn) -> vn) in
    { vertex_indices; texture_indices; normal_indices }
end

module Face_vertices = struct
  type t = { vertices : Vertex.t list;
             texture_coordinates : Texture_coordinate.t list;
             vertex_normals : Vertex_normal.t list; }

  let of_indices indices v_list tv_list vn_list =
    let vertices = List.map indices.Face_indices.vertex_indices
        ~f:(fun i -> match List.nth v_list (i - 1) with
            | Some v -> v
            | None -> failwith ("Could not find vertex " ^ (string_of_int i))
          )
    in
    let texture_coordinates = List.map indices.Face_indices.texture_indices
        ~f:(fun i -> match List.nth tv_list (i - 1) with
            | Some c -> c
            | None -> failwith ("Could not find texture coordinates " ^
                                (string_of_int i))
          )
    in
    let vertex_normals = List.map indices.Face_indices.normal_indices
        ~f:(fun i -> match List.nth vn_list (i - 1) with
            | Some n -> n
            | None -> failwith ("Could not find vertex normal " ^ (string_of_int i))
          )
    in

    { vertices; texture_coordinates; vertex_normals }

end



let pair_of_list l =
  let u = (List.nth_exn l 0) in
  let v = (List.nth_exn l 1) in
  (u,v)

let triple_of_list l =
  let x = (List.nth_exn l 0) in
  let y = (List.nth_exn l 1) in
  let z = (List.nth_exn l 2) in
  (x,y,z)

let quadruple_of_list l =
  let u = (List.nth_exn l 0) in
  let v = (List.nth_exn l 1) in
  let w = (List.nth_exn l 2) in
  let x = (List.nth_exn l 3) in
  (u,v,w,x)

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

let vertex_of_words words =
  match List.map words ~f:float_of_string with
  | [] -> failwith "Attempted to parse vertex from empty list"
  | [x; y; z] -> Vertex.of_tuple (x, y, z, None)
  | [x; y; z; w] -> Vertex.of_tuple (x, y, z, Some w)
  | _ -> failwith "Vertex entry contained incorrect number of coordinates"

let texture_coordinate_of_words words =
  match List.map words ~f:float_of_string with
  | [] -> failwith "Attempted to parse texture coordinate from empty list"
  | [u; v] -> Texture_coordinate.of_tuple (u, v, None)
  | [u; v; w] -> Texture_coordinate.of_tuple (u, v, Some w)
  | _ -> failwith "Texture coordinate entry contained incorrect number of coordinates"

let vertex_normal_of_words words =
  match List.map words ~f:float_of_string with
  | [] -> failwith "Attempted to parse vertex normal from empty list"
  | [x; y; z] -> Vertex_normal.of_tuple (x, y, z)
  | _ -> failwith "Vertex normal entry contained incorrect number of coordinates"


let face_indices_of_words words =
  let parse_vertex word =
    let split = String.split ~on:'/' word in
    (* let ints = List.map ~f:int_of_string split in *)
    match split with
    | [] -> failwith "Attempted to parse vertex index from empty list"
    | [v] -> (v, None, None)
    | [v;"";vn] -> (v, None, Some vn)
    | [v;tc] -> (v, Some tc, None)
    | [v;tc;vn] -> (v, Some tc, Some vn)
    | _ -> failwith "Face entry incorrectly structured"
  in

  List.map words ~f:parse_vertex
  |> List.map ~f:(fun (a,b,c) ->
      (int_of_string a,
       Option.map ~f:int_of_string b,
       Option.map ~f:int_of_string c))
  |> Face_indices.of_tuple_list



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
    | Some "v" -> (vs @ [vertex_of_words rest], vts, vns, fs)
    | Some "vt" -> (vs, vts @ [texture_coordinate_of_words rest], vns, fs)
    | Some "vn" -> (vs, vts, vns @ [vertex_normal_of_words rest], fs)
    | Some "f" -> (vs, vts, vns, fs @ [face_indices_of_words rest])
    | _ -> so_far
  in
  result

(* TODO: make this output an array, and make parse_line reverse the lists instead
   of appending to the end *)
let parse_model file =
  String.split_lines file
  (* pop out empty lines and comments *)
  |> List.filter ~f:(fun l -> (String.length l) > 0 && (l.[0] <> '#'))
  |> List.fold ~init:([],[],[],[]) ~f:(fun acc line -> parse_line line acc)


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

let get_v_indices_from_face face =
  Array.map face ~f:(fun (v,_,_) -> v)
