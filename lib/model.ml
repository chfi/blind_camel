open Core.Std

(* represents a model parsed from a wavefront .obj file *)

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

  let to_tuple { x; y; z; w } = (x,y,z,w)
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

  let to_tuple { u; v; w } = (u,v,w)
end

module Vertex_normal = struct
  type t = { x : float;
             y : float;
             z : float; }

  let of_tuple (x,y,z) = { x; y; z }

  let to_tuple { x; y; z } = (x,y,z)
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


type obj_type = [ `Vertex of Vertex.t
          | `Texture of Texture_coordinate.t
          | `Normal of Vertex_normal.t
          | `Face of Face_indices.t ]


let parse_line line : obj_type =
  let words =
    String.split_on_chars ~on:[' ';'\t'] line
    |> List.filter ~f:((<>) "")
  in
  let head = List.hd words in
  let tail = List.tl words in
  match head, tail with
  | None, _ | _, None -> failwith "Line empty or only one word"
  | Some "v", Some t -> `Vertex (vertex_of_words t)
  | Some "vt", Some t -> `Texture (texture_coordinate_of_words t)
  | Some "vn", Some t -> `Normal (vertex_normal_of_words t)
  | Some "f", Some t -> `Face (face_indices_of_words t)
  | Some _, _ -> failwith "Could not parse line"


let build_model obj =
  let (vs, tcs, vns, fis) =
    List.fold obj ~init:([],[],[],[]) ~f:(fun (v',tn',vn',fi') x -> match x with
        | `Vertex x  -> x :: v', tn',      vn',      fi'
        | `Texture x -> v',      x :: tn', vn',      fi'
        | `Normal x  -> v',      tn',      x :: vn', fi'
        | `Face x    -> v',      tn',      vn',      x :: fi')
  in
  let vs = List.rev vs in
  let tcs = List.rev tcs in
  let vns = List.rev vns in
  let fis = List.rev fis in
  List.map fis ~f:(fun f -> Face_vertices.of_indices f vs tcs vns)


let parse_model file =
  String.split_lines file
  (* pop out empty lines and comments *)
  |> List.filter ~f:(fun l -> (String.length l) > 0 && (l.[0] <> '#'))
  |> List.fold ~init:[] ~f:(fun acc line -> (parse_line line) :: acc)
  |> build_model


