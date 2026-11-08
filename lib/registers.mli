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

val create : unit -> t 

val read_r : t -> r -> uint8
val write_r : t -> r -> uint8 -> unit
val read_rr : t -> rr -> uint8
val write_rr : t -> rr -> uint8 -> unit 

val read_flag : t -> flag -> bool
val set_flag : t -> flag -> unit
val unset_flag : t -> flag -> unit

val set_flags : t -> ?n:bool -> ?s:bool -> ?h:bool -> ? c:bool -> ? unit -> unit 

val to_string : t -> string