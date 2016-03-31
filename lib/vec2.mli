
module Vec2 : sig
  type 'a t = { x : 'a;
                y : 'a; }

  val map : ('a t) -> f:('a -> 'a) -> ('a t)
end

module Vec2f : sig
  include (module type of Vec2)

  type t' = (float Vec2.t)

  val of_tuple : float * float -> t'
  val to_tuple : t' -> float * float

end

module Vec2i : sig
  type t' = (int Vec2.t)

  val of_tuple : int * int -> t'
  val to_tuple : t' -> int * int

  val map : t' -> f:(int -> int) -> t'

end

val vec2f_of_vec2i : Vec2i.t' -> Vec2f.t'

val vec2i_of_vec2f : Vec2f.t' -> Vec2i.t'
