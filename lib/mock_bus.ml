(* mock bus for tests: just one flat array of memory, starting from address 0 *)

open Uints 

type t = Bytes.t

let create ~size = 
  Bytes.make size '\000'

let accepts t addr = 
  Uint16.to_int addr < Bytes.length t 

let read_byte t addr =
  Bytes.get t (Uint16.to_int addr) |> Char.code |> Uint8.of_int

let write_byte t ~addr ~data = 
  Bytes.set t (Uint16.to_int addr) (Char.chr (Uint8.to_int data))

let read_word t addr = 
  let lo = read_byte t addr in
  let hi = read_byte t (Uint16.succ addr) in
  Uint16.of_bytes ~hi ~lo

let write_word t ~addr ~data =
  write_byte t ~addr ~data:(Uint16.lo data);
  write_byte t ~addr:(Uint16.succ addr) ~data:(Uint16.hi data) 