(* phase 2 tests: checks that
 *   - 8-bit registers store values independently
 *   - register pairs (BC, HL, ...) combine their two halves correctly
 *   - the lower 4 bits of F always stay 0
 *   - flags can be set and cleared without disturbing each other
 *   - to_string prints the registers in a readable format *)

open Camlboy
open Uints
open Registers

let failures = ref 0

let check name cond =
  if cond then Printf.printf "  PASS  %s\n" name
  else (incr failures; Printf.printf "  FAIL  %s\n" name)

let u8 = Uint8.of_int
let u16 = Uint16.of_int

let () =
  print_endline "8-bit registers:";
  let regs = create () in
  check "start at 0" (Uint8.equal (read_r regs A) Uint8.zero);
  write_r regs B (u8 0x12);
  check "write B then read B" (Uint8.equal (read_r regs B) (u8 0x12));
  check "writing B doesn't touch C" (Uint8.equal (read_r regs C) Uint8.zero);

  print_endline "16-bit pairs:";
  write_r regs C (u8 0x34);
  check "B=0x12, C=0x34 means BC=0x1234" (Uint16.equal (read_rr regs BC) (u16 0x1234));
  write_rr regs HL (u16 0xABCD);
  check "write HL=0xABCD sets H=0xAB" (Uint8.equal (read_r regs H) (u8 0xAB));
  check "write HL=0xABCD sets L=0xCD" (Uint8.equal (read_r regs L) (u8 0xCD));
  write_rr regs DE (u16 0xBEEF);
  check "DE round-trips" (Uint16.equal (read_rr regs DE) (u16 0xBEEF));

  print_endline "F register quirk:";
  write_r regs F (u8 0xFF);
  check "lower 4 bits of F are forced to 0" (Uint8.equal (read_r regs F) (u8 0xF0));
  write_rr regs AF (u16 0x12FF);
  check "same when writing through AF" (Uint16.equal (read_rr regs AF) (u16 0x12F0));

  print_endline "Flags:";
  let regs = create () in
  check "all flags start off"
    (not (read_flag regs Zero || read_flag regs Subtraction
          || read_flag regs Half_carry || read_flag regs Carry));
  set_flag regs Zero;
  check "set Zero turns on bit 7 of F" (Uint8.equal (read_r regs F) (u8 0x80));
  set_flag regs Carry;
  check "set Carry turns on bit 4 too" (Uint8.equal (read_r regs F) (u8 0x90));
  unset_flag regs Zero;
  check "unset Zero leaves Carry alone"
    (not (read_flag regs Zero) && read_flag regs Carry);
  set_flags regs ~z:true ~h:true ~c:false ();
  check "set_flags changes only the flags given"
    (read_flag regs Zero && (not (read_flag regs Subtraction))
     && read_flag regs Half_carry && not (read_flag regs Carry));
  write_r regs F (u8 0x50);
  check "writing F directly changes flags (0x50 = N and C)"
    ((not (read_flag regs Zero)) && read_flag regs Subtraction
     && (not (read_flag regs Half_carry)) && read_flag regs Carry);

  print_endline "Debug output:";
  let regs = create () in
  write_rr regs AF (u16 0x01B0);
  write_rr regs BC (u16 0x0013);
  write_rr regs DE (u16 0x00D8);
  write_rr regs HL (u16 0x014D);
  let s = to_string regs in
  print_endline ("        " ^ s);
  check "to_string looks right"
    (s = "A:01 F:B0 B:00 C:13 D:00 E:D8 H:01 L:4D [Z-HC]");

  if !failures = 0 then print_endline "\nAll Phase 2 tests passed!"
  else (Printf.printf "\n%d test(s) failed\n" !failures; exit 1)