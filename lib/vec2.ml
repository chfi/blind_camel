open Core.Std

module Vec2 = struct
  type 'a t = { x : 'a;
                y : 'a; }

  let of_tuple (x,y) = { x; y }
  let to_tuple { x; y } = (x,y)

  let map { x; y } ~f = { x = (f x); y = (f y) }

end

module Vec2f : sig
  include (module type of Vec2)

  type t' = (float Vec2.t)

  val of_tuple : float * float -> t'
  val to_tuple : t' -> float * float

  val map : t' -> f:(float -> float) -> t'

end = struct
  open Vec2

  type t' = float Vec2.t

  include Vec2
end

module Vec2i : sig
  type t' = (int Vec2.t)

  val of_tuple : int * int -> t'
  val to_tuple : t' -> int * int

  val map : t' -> f:(int -> int) -> t'

end = struct
  open Vec2

  type t' = int t

  include Vec2
end

let vec2f_of_vec2i = Vec2.map ~f:float_of_int

let vec2i_of_vec2f ?dir =
  let dir = Option.value ~default:`Down dir in
  Vec2.map ~f:(fun x -> Float.iround_exn ~dir x)

