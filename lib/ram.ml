(* a block of memory covering addresses start_addr to end_addr [inclusive] *)

open Uints

type t = {
  start_addr : uint16;
  end_addr : uint16;
  bytes : Bytes.t;
}

let create ~start_addr ~end_addr = 
  let size = Uint16.to_int end_addr - Uint16.to_int start_addr + 1 in
  { start_addr; end_addr; bytes = Bytes.make size '\000' }

let accepts t addr =
  Uint16.(t.start_addr <= addr && addr <= t.end_addr)

let offset t addr = 
  Uint16.to_int addr - Uint16.to_int t.start_addr

let read_byte t addr =
  Bytes.get t.bytes (offset t addr) |> Char.code |> Uint8.of_int

let write_byte t ~addr ~data = 
  Bytes.set t.bytes (offset t addr) (Char.chr (Uint8.to_int data))