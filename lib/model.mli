type vertex = (float * float * float * float)
type texture_vertex = (float * float * float)
type normal_vertex = (float * float * float)

type face = (int * int option * int option) array

val parse_model : string ->
  (vertex list * texture_vertex list * normal_vertex list * face list)

val show_vertex : vertex -> string

val show_t_vertex : texture_vertex -> string

val show_n_vertex : normal_vertex -> string

