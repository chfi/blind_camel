(*
module Vec2 :
  sig
    type 'a t = { x : 'a; y : 'a; }
    val of_tuple : 'a * 'a -> 'a t
    val to_tuple : 'a t -> 'a * 'a
    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

module Vec2f :
  sig
    type 'a t = { x : float; y : float; }
    type t' = float Vec2.t
    val of_tuple : float * float -> t'
    val to_tuple : t' -> float * float
    val map : t' -> f:(float -> float) -> t'
  end

module Vec2i :
  sig
    type 'a t = { x : int; y : int }
    type t' = int Vec2.t
    val of_tuple : int * int -> t'
    val to_tuple : t' -> int * int
    val map : t' -> f:(int -> int) -> t'
  end
   *)

type 'a t = { x : 'a; y : 'a; }
val of_tuple : 'a * 'a -> 'a t
val to_tuple : 'a t -> 'a * 'a
val map : 'a t -> f:('a -> 'b) -> 'b t

val vec2f_of_vec2i : int t -> float t

val vec2i_of_vec2f :
  ?dir:[ `Down | `Nearest | `Up | `Zero ] -> float t -> int t

