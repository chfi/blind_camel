open Core.Std

(* represents a model parsed from a wavefront .obj file *)

type vertex = (float * float * float * float)
type texture_vertex = (float * float)
type normal_vertex = (float * float * float)
type face = (vertex array) *
            ((texture_vertex array) option) *
            ((normal_vertex array) option)
type model =
  {
    vertices : vertex array;
    texture_vertices : texture_vertex array;
    normal_vertices : normal_vertex array;
    faces : face array;
  }

