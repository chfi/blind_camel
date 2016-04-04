type screen = {
  width : float;
  height : float;
  x_mult : float;
  y_mult : float;
}

val orthogonal_projection : screen -> float Vec3.t -> int Vec2.t

(* val map_pair : ('a -> 'b) -> 'a * 'a -> 'b * 'b *)

val vec3_of_vertex : Model.Vertex.t -> float Vec3.t

val get_line_screen_points : int Vec2.t -> int Vec2.t -> int Vec2.t list

val get_empty_triangle_screen_points :
  int Vec2.t -> int Vec2.t -> int Vec2.t -> int Vec2.t list

val get_filled_triangle_screen_points :
  int Vec2.t -> int Vec2.t -> int Vec2.t -> int Vec2.t list

val get_screen_lines_from_world_vertices :
  screen -> Model.Vertex.t list -> int Vec2.t list

val get_wireframe_screen_lines :
  screen -> Model.Face_vertices.t list -> int Vec2.t list

val barycentric_of_cartesian :
  float Vec2.t -> float Vec2.t -> float Vec2.t -> float Vec2.t -> float Vec3.t

val cartesian_of_barycentric :
  float Vec2.t -> float Vec2.t -> float Vec2.t -> float Vec3.t -> float Vec2.t
