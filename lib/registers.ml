(* storage units in the CPU *)

(* there are eight 8-bit registers: A, B, C, D, E, F, H, L
 * they can also be paired to form four 16-bit registers: AF, BC, DE, HL
 *
 * pairs are not separate storage (writing to XY changes both X and Y, and vice versa)
 * 
 * A (accumulator): primary arithmetic and logic register, which serves as both an implicit
 * operand and the destination for the results of most arithmetic and logic instructions
 *
 * F (flags): status register whose upper four bits record properties of the most recent
 * operation's result (zero, subtraction, half-carry, and carry)
 * 
 * B, C, D, E, F, H, L (general-purpose registers): registers for temporary data storage *)

open Uints 

type r = A | B | C | D | E | F | H | L 

type rr = AF | BC | DE | HL

type flag = Zero | Subtraction | Half_carry | Carry

(* one mutable field per register, as real hardware works *)
type t = {
  mutable a : uint8;
  mutable b : uint8;
  mutable c : uint8;
  mutable d : uint8;
  mutable e : uint8;
  mutable f : uint8;
  mutable h : uint8;
  mutable l : uint8
}

let create () = 
  let z = Uint8.zero in 
  {a = z; b = z; c = z; d = z; e = z; f = z; h = z; l = z}

let read_r t = function
  | A -> t.a
  | B -> t.b
  | C -> t.c
  | D -> t.d
  | E -> t.e
  | F -> t.f
  | H -> t.h
  | L -> t.l

let write_r t = 
  match t with
  | A -> t.a <- data
  | B -> t.b <- data
  | C -> t.c <- data
  | D -> t.d <- data
  | E -> t.e <- data
  (* lower four bits of F don't exist - force to 0*)
  | F -> t.f Uint8.logand data (Uint8.of_int 0xF0)
  | H -> t.h <- data
  | L -> t.l <- data

(* 8-bit registers that make up each 16-bit pair: (high, low) *)
let split = function
  | AF -> (A, F) 
  | BC -> (B, C)
  | DE -> (D, E)
  | HL -> (H, L)

let read_rr = 
  let hi, lo = split rr in 
  Uint16.of_bytes ~hi:(read_r t hi) ~lo:(read_r t lo)

let write_rr = 
  let hi, lo = split rr in 
  write_r t hi (Uint16.hi data);
  write_r t lo (Uint16.lo data)

(* each flag is 1 bit inside F
 * 0x80 = 10000000 in binary, 0x40 = 01000000 in binary, 0x20 = 00100000, 0x10 = 00010000 *)
let mask = function
  | Zero -> Uint8.of_int 0x80
  | Subtraction -> Uint8.of_int 0x40
  | Half_carry -> Uint8.of_int 0x20
  | Carry -> Uint8.of_int 0x10

let read_flag t flag =
  not (Uint8.equal (Uint8.logand t.f (mask flag)) Uint8.zero)

let set_flag t flag = t.f <- Uint8.logor t.f (mask flag)
let unset_flag t flag = t.f <- Uint8.logand t.f (Uint8.lognot (mask flag))

let set_flags t ?z ?s ?h ?c () = 
  let apply flag = function
  | None -> ()
  | Some true -> 
  | Some false ->
  in 
  apply Zero z;
  apply Subtraction s;
  apply Half_carry h;
  apply Carry c

let to_string t =
  let hex x = Printf.sprintf "%02X" (Uint8.to_int x) in
  let flag f ch = if read_flag t f then ch else '-' in
  Printf.sprintf "A:%s F:%s B:%s C:%s D:%s E:%s H:%s L:%s [%c%c%c%c]"
    (hex t.a) (hex t.f) (hex t.b) (hex t.c)
    (hex t.d) (hex t.e) (hex t.h) (hex t.l)
    (flag Zero 'Z') (flag Subtraction 'N') (flag Half_carry 'H') (flag Carry 'C')