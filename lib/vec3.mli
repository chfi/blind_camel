(* module Vec3 : *)
(*   sig *)
    type 'a t = { x : 'a; y : 'a; z : 'a; }
    val of_tuple : 'a * 'a * 'a -> 'a t
    val to_tuple : 'a t -> 'a * 'a * 'a
    val map : 'a t -> f:('a -> 'b) -> 'b t
  (* end *)
        (*

module Vec3f :
  sig
    type 'a t = { x : 'a; y : 'a; z : 'a; }
    type t' = float Vec3.t
    val of_tuple : float * float * float -> t'
    val to_tuple : t' -> float * float * float
    val map : t' -> f:(float -> float) -> t'
  end

module Vec3i :
  sig
    type t' = int Vec3.t
    val of_tuple : int * int * int -> t'
    val to_tuple : t' -> int * int * int
    val map : t' -> f:(int -> int) -> t'
  end
           *)

val vec3f_of_vec3i : int t -> float t

val vec3i_of_vec3f :
  ?dir:[ `Down | `Nearest | `Up | `Zero ] -> float t -> int t
