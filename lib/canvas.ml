open Core.Std

type canvas =
  { width : int;
    height : int;
    array : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
    flipped_vert : bool; (* if true, the origin is in the bottom left. *)
  }

let create_canvas w h =
  { width = w;
    height = h;
    array = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (w*h);
    flipped_vert = true;
  }

let int32_of_rgb r g b =
  let ri = Int32.shift_left (Int32.of_int_exn r) 24 in
  let gi = Int32.shift_left (Int32.of_int_exn g) 16 in
  let bi = Int32.shift_left (Int32.of_int_exn b) 8 in
  (Int32.bit_or ri (Int32.bit_or gi (Int32.bit_or bi 0xffl)))

let set_canvas_pixel c x y (r,g,b) =
  if (x > c.width) || (y > c.height)
  then raise (Invalid_argument "Tried to draw outside canvas");
  let y = if c.flipped_vert then (c.height - y) else y in
  c.array.{(c.width*y) + x} <- (int32_of_rgb r g b)

let draw_list c l col =
  List.iter l ~f:(fun (x,y) ->
      set_canvas_pixel c x y col)

let draw_colored_list c l =
  List.iter l ~f:(fun (x,y,col) ->
      set_canvas_pixel c x y col)

let render_canvas c filename =
  let module S = Tsdl.Sdl in
  let surf =
    match S.create_rgb_surface_from c.array ~w:c.width ~h:c.height
            ~depth:32 ~pitch:c.width
            0xFF000000l 0x00FF0000l 0x0000FF00l 0x000000FFl
    with
    | Error (`Msg e) -> raise (Failure ("Surface creation failure: " ^ e))
    | Ok s -> s
  in
  match S.save_bmp surf filename with
  | Error (`Msg e) -> raise (Failure ("BMP save failure: " ^ e))
  | Ok () -> ()
