type point_f = (float * float)
type point_i = (int * int)

val point_f_of_i : point_i -> point_f
val point_i_of_f : point_f -> point_i

val get_line_points : point_i -> point_i -> point_i list

val get_lines_from_vertices : Model.vertex array -> point_i list array

val get_wireframe_lines : Model.vertex list -> Model.face list -> point_i list
