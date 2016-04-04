open Core.Std

type 'a t = { x : 'a;
              y : 'a;
              z : 'a; }


let of_tuple (x,y,z) = { x; y; z }
let to_tuple { x; y; z } = (x,y,z)

let map { x; y; z } ~f = { x = (f x); y = (f y); z = (f z) }

(*
module Vec3 = struct
  type 'a t = { x : 'a;
                y : 'a;
                z : 'a; }

  let of_tuple (x,y,z) = { x; y; z }
  let to_tuple { x; y; z } = (x,y,z)

  let map { x; y; z } ~f = { x = (f x); y = (f y); z = (f z) }

end

module Vec3f : sig
  include (module type of Vec3)

  type t' = (float Vec3.t)

  val of_tuple : float * float * float -> t'
  val to_tuple : t' -> float * float * float

  val map : t' -> f:(float -> float) -> t'

end = struct
  open Vec3

  type t' = float Vec3.t

  include Vec3
end

module Vec3i : sig
  type t' = (int Vec3.t)

  val of_tuple : int * int * int -> t'
  val to_tuple : t' -> int * int * int

  val map : t' -> f:(int -> int) -> t'

end = struct
  open Vec3

  type t' = int t

  include Vec3
end

   *)

let vec3f_of_vec3i = map ~f:float_of_int

let vec3i_of_vec3f ?dir =
  let dir = Option.value ~default:`Down dir in
  map ~f:(fun x -> Float.iround_exn ~dir x)

