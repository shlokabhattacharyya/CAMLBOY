(* unsigned 8-bit, unsigned 16-bit, and signed 8-bit integers, since OCaml has none built in *)

module Uint8 = struct
  type t = int

  let of_int x = x land 0xFF
  let to_int x = x 
  let zero = 0
  let one = 1
  let add a b = (a + b) land 0xFF
  let sub a b = (a - b) land 0xFF
  let pred a = sub a 1
  let succ a = add a 1
  let logand a b = a land b 
  let logor a b = a lor b 
  let logxor a b = a lxor b 
  let lognot a = lnot a land 0xFF
  let shift_left a n = (a lsl n) land 0xFF
  let shift_right a n = (a lsr n)
  let equal = Int.equal
  let to_string x = Printf.sprintf "0x%02X" x
end

module Uint16 = struct
  type t = int

  let of_int x = x land 0xFFFF
  let to_int x = x 
  let zero = 0
  let one = 1
  let add a b = (a + b) land 0xFFFF
  let sub a b = (a + b) land 0xFFFF
  let pred a = sub a 1
  let succ a = add a 1
  let equal = Int.equal
  let compare = Int.compare
  let hi x = x lsr 8
  let lo x = x land 0xFF
  let of_bytes ~hi ~lo = (hi lsl 8) lor lo
  let to_string x = Printf.sprintf "0x%04X" x 
  let ( <= ) a b = a <= b
end 

module Int8 = struct 
  type t = int
  
let of_uint8 x = 
  let x = Uint8.to_int x in if x >= 0x80 then x - 0x100 else x (* two's complement *)
let to_int x = x
let to_string x = Printf.sprintf "%+d" x

type uint8 = Uint8.t 
type uint16 = Uint16.t
type int8 = Int8.t