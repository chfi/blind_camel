(** A simple canvas type for drawing pixels onto.
    Can be rendered into BMP files, using SDL. **)

type canvas

val create_canvas : int -> int -> canvas

val set_canvas_pixel : canvas -> int -> int -> (int * int * int) -> unit

val draw_list : canvas -> (int * int) list -> (int * int * int) -> unit

val draw_colored_list : canvas -> (int * int * (int * int * int)) list -> unit

val render_canvas : canvas -> string -> unit
