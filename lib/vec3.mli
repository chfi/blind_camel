
module Vec3 : sig
  type 'a t = { x : 'a;
                y : 'a;
                z : 'a; }

  val map : ('a t) -> f:('a -> 'a) -> ('a t)
end

module Vec3f : sig
  include (module type of Vec3)

  type t' = (float Vec3.t)

  val of_tuple : float * float -> t'
  val to_tuple : t' -> float * float

end

module Vec3i : sig
  type t' = (int Vec3.t)

  val of_tuple : int * int -> t'
  val to_tuple : t' -> int * int

  val map : t' -> f:(int -> int) -> t'

end

val vec3f_of_vec3i : Vec3i.t' -> Vec3f.t'

val vec3i_of_vec3f : Vec3f.t' -> Vec3i.t'
