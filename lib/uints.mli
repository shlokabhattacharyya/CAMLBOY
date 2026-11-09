(* unsigned 8-bit, unsigned 16-bit, and signed 8-bit integers, since OCaml has none built in *)

module Uint8 : sig 
  type t

  val of_int : int -> t
  val to_int : t -> int
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val pred : t -> t
  val succ : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val equal : t -> t -> bool
  val to_string : t -> string
end

module Uint16 : sig
  type t 

  val of_int : int -> t
  val to_int : t -> int
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val pred : t -> t
  val succ : t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hi : t -> Uint8.t 
  val lo : t -> Uint8.t
  val of_bytes : hi:Uint8.t -> lo:Uint8.t -> t
  val to_string : t -> string
  val ( <= ) : t -> t -> bool
end

module Int8 : sig
  type to_int

  val of_uint8 : Uint8.t -> t 
  val to_int : t -> int
  val to_string : t -> string 
end 

type uint8 = Uint8.t 
type uint16 = Uint16.t
type int8 = Int8.t