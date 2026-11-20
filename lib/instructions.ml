(* every GameBoy CPU instruction, described as OCaml data
 *
 * GADT design: an argument remembers in its type what type of value it holds *)

open Uints

(* where an instruction reads a value from or writes it to *)
type _ arg =
  | Immediate8 : uint8 -> uint8 arg          (* a number written right in the code *)
  | Immediate16 : uint16 -> uint16 arg       (* a number written right in the code *)
  | Direct8 : uint16 -> uint8 arg            (* the byte in memory at this address *)
  | Direct16 : uint16 -> uint16 arg          (* the word in memory at this address *)
  | R : Registers.r -> uint8 arg             (* an 8-bit register, e.g. A *)
  | RR : Registers.rr -> uint16 arg          (* a register pair, e.g. HL *)
  | RR_indirect : Registers.rr -> uint8 arg  (* the byte at the address held in a pair: (HL) *)
  | FF00_offset : uint8 -> uint8 arg         (* the byte at 0xFF00 + n *)
  | FF00_C : uint8 arg                       (* the byte at 0xFF00 + register C *)
  | HL_inc : uint8 arg                       (* (HL), then add 1 to HL *)
  | HL_dec : uint8 arg                       (* (HL), then subtract 1 from HL *)
  | SP : uint16 arg                          (* the stack pointer *)
  | SP_offset : int8 -> uint16 arg           (* SP plus a signed number *)

(* when a jump/call/return should happen *)
type condition =
  | Always
  | NZ  (* only if the Zero flag is OFF *)
  | Z   (* only if the Zero flag is ON *)
  | NC  (* only if the Carry flag is OFF *)
  | C   (* only if the Carry flag is ON *)

type t =
  (* loads (copying values around) *)
  | LD8 of uint8 arg * uint8 arg     (* LD dst, src *)
  | LD16 of uint16 arg * uint16 arg
  (* arithmetic and logic *)
  | ADD8 of uint8 arg * uint8 arg
  | ADD16 of uint16 arg * uint16 arg
  | ADDSP of int8
  | ADC of uint8 arg * uint8 arg     (* add with carry *)
  | SUB of uint8 arg * uint8 arg
  | SBC of uint8 arg * uint8 arg     (* subtract with carry *)
  | AND of uint8 arg * uint8 arg
  | OR of uint8 arg * uint8 arg
  | XOR of uint8 arg * uint8 arg
  | CP of uint8 arg * uint8 arg      (* compare: subtract, keep only the flags *)
  | INC of uint8 arg
  | INC16 of uint16 arg
  | DEC of uint8 arg
  | DEC16 of uint16 arg
  | SWAP of uint8 arg                (* swap the two halves of a byte *)
  | DAA                              (* fix A up after decimal math *)
  | CPL                              (* flip every bit of A *)
  | CCF                              (* flip the Carry flag *)
  | SCF                              (* set the Carry flag *)
  (* CPU control *)
  | NOP
  | HALT
  | STOP
  | DI                               (* disable interrupts *)
  | EI                               (* enable interrupts *)
  (* rotates and shifts: sliding the bits of a byte left or right *)
  | RLCA
  | RLA
  | RRCA
  | RRA
  | RLC of uint8 arg
  | RL of uint8 arg
  | RRC of uint8 arg
  | RR of uint8 arg                  (* note: a different RR from arg's RR! *)
  | SLA of uint8 arg
  | SRA of uint8 arg
  | SRL of uint8 arg
  (* single-bit operations *)
  | BIT of int * uint8 arg           (* test bit n *)
  | SET of int * uint8 arg           (* turn bit n on *)
  | RES of int * uint8 arg           (* turn bit n off *)
  (* the stack *)
  | PUSH of Registers.rr
  | POP of Registers.rr
  (* jumps, calls, returns *)
  | JP of condition * uint16 arg     (* jump to an address *)
  | JR of condition * int8           (* jump relative: "skip ahead/back n bytes" *)
  | CALL of condition * uint16       (* jump, but remember where to come back *)
  | RST of uint16                    (* a short CALL to a fixed address *)
  | RET of condition                 (* come back from a CALL *)
  | RETI                             (* RET + enable interrupts *)


(* --- turning instructions into readable text --- *)

let string_of_r : Registers.r -> string = function
  | A -> "A" | B -> "B" | C -> "C" | D -> "D"
  | E -> "E" | F -> "F" | H -> "H" | L -> "L"

let string_of_rr : Registers.rr -> string = function
  | AF -> "AF" | BC -> "BC" | DE -> "DE" | HL -> "HL"

(* "type a." is how you write a function over ALL kinds of arg, both
   uint8 arg and uint16 arg (needed whenever you match on a GADT) *)
let string_of_arg : type a. a arg -> string = function
  | Immediate8 n -> Uint8.to_string n
  | Immediate16 n -> Uint16.to_string n
  | Direct8 addr | Direct16 addr -> "(" ^ Uint16.to_string addr ^ ")"
  | R r -> string_of_r r
  | RR rr -> string_of_rr rr
  | RR_indirect rr -> "(" ^ string_of_rr rr ^ ")"
  | FF00_offset n -> "(0xFF00+" ^ Uint8.to_string n ^ ")"
  | FF00_C -> "(0xFF00+C)"
  | HL_inc -> "(HL+)"
  | HL_dec -> "(HL-)"
  | SP -> "SP"
  | SP_offset n -> "SP" ^ Int8.to_string n

let string_of_condition = function
  | Always -> "" | NZ -> "NZ" | Z -> "Z" | NC -> "NC" | C -> "C"

let to_string (inst : t) =
  let one name x = name ^ " " ^ string_of_arg x in
  let two name x y = name ^ " " ^ string_of_arg x ^ ", " ^ string_of_arg y in
  let with_cond name cond rest =
    match cond with
    | Always -> name ^ " " ^ rest
    | c -> name ^ " " ^ string_of_condition c ^ ", " ^ rest
  in
  match inst with
  | LD8 (x, y) -> two "LD" x y
  | LD16 (x, y) -> two "LD" x y
  | ADD8 (x, y) -> two "ADD" x y
  | ADD16 (x, y) -> two "ADD" x y
  | ADDSP n -> "ADD SP, " ^ Int8.to_string n
  | ADC (x, y) -> two "ADC" x y
  | SUB (x, y) -> two "SUB" x y
  | SBC (x, y) -> two "SBC" x y
  | AND (x, y) -> two "AND" x y
  | OR (x, y) -> two "OR" x y
  | XOR (x, y) -> two "XOR" x y
  | CP (x, y) -> two "CP" x y
  | INC x -> one "INC" x
  | INC16 x -> one "INC" x
  | DEC x -> one "DEC" x
  | DEC16 x -> one "DEC" x
  | SWAP x -> one "SWAP" x
  | DAA -> "DAA" | CPL -> "CPL" | CCF -> "CCF" | SCF -> "SCF"
  | NOP -> "NOP" | HALT -> "HALT" | STOP -> "STOP"
  | DI -> "DI" | EI -> "EI"
  | RLCA -> "RLCA" | RLA -> "RLA" | RRCA -> "RRCA" | RRA -> "RRA"
  | RLC x -> one "RLC" x
  | RL x -> one "RL" x
  | RRC x -> one "RRC" x
  | RR x -> one "RR" x
  | SLA x -> one "SLA" x
  | SRA x -> one "SRA" x
  | SRL x -> one "SRL" x
  | BIT (n, x) -> Printf.sprintf "BIT %d, %s" n (string_of_arg x)
  | SET (n, x) -> Printf.sprintf "SET %d, %s" n (string_of_arg x)
  | RES (n, x) -> Printf.sprintf "RES %d, %s" n (string_of_arg x)
  | PUSH rr -> "PUSH " ^ string_of_rr rr
  | POP rr -> "POP " ^ string_of_rr rr
  | JP (cond, x) -> with_cond "JP" cond (string_of_arg x)
  | JR (cond, n) -> with_cond "JR" cond (Int8.to_string n)
  | CALL (cond, addr) -> with_cond "CALL" cond (Uint16.to_string addr)
  | RST addr -> "RST " ^ Uint16.to_string addr
  | RET Always -> "RET"
  | RET cond -> "RET " ^ string_of_condition cond
  | RETI -> "RETI"