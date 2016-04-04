
module Vertex :
  sig
    type t = { x : float; y : float; z : float; w : float; }
    val of_tuple : float * float * float * float option -> t
    val to_tuple : t -> float * float * float * float
  end

module Texture_coordinate :
  sig
    type t = { u : float; v : float; w : float; }
    val of_tuple : float * float * float option -> t
    val to_tuple : t -> float * float * float
  end

module Vertex_normal :
  sig
    type t = { x : float; y : float; z : float; }
    val of_tuple : float * float * float -> t
    val to_tuple : t -> float * float * float
  end


module Face_vertices :
  sig
    type t = {
      vertices : Vertex.t list;
      texture_coordinates : Texture_coordinate.t list;
      vertex_normals : Vertex_normal.t list;
    }
  end

type t = Face_vertices.t list

val parse_model : string -> Face_vertices.t list
