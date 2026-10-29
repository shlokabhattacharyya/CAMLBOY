(* phase 1 tests: checks that
 *   - bytes wrap around correctly
 *   - RAM stores and finds its addresses
 *   - mock bus reads/writes 16-bit words low byte first *)

open Camlboy
open Uints

let failures = ref 0

let check name cond =
  if cond then Printf.printf "  PASS  %s\n" name
  else (incr failures; Printf.printf "  FAIL  %s\n" name)

let u8 = Uint8.of_int
let u16 = Uint16.of_int

let () = 
  print_endline "uints:";
  check "0xFF + 1 wraps to 0x00" (Uint8.equal (Uint8.add (u8 0xFF) Uint8.one) Uint8.zero);
  check "0x00 - 1 wraps to 0xFF" (Uint8.equal (Uint8.pred Uint8.zero) (u8 0xFF));
  check "0xFFFF + 1 wraps to 0x0000" (Uint16.equal (Uint16.succ (u16 0xFFFF)) Uint16.zero);
  check "of_bytes 0x12 0x34 = 0x1234"
    (Uint16.equal (Uint16.of_bytes ~hi:(u8 0x12) ~lo:(u8 0x34)) (u16 0x1234));
  check "hi/lo split 0xABCD"
    (Uint8.equal (Uint16.hi (u16 0xABCD)) (u8 0xAB)
     && Uint8.equal (Uint16.lo (u16 0xABCD)) (u8 0xCD));

  print_endline "ram:";
  let ram = Ram.create ~start_addr:(u16 0xC000) ~end_addr:(u16 0xDFFF) in
  check "accepts 0xC000 (start)" (Ram.accepts ram (u16 0xC000));
  check "accepts 0xDFFF (end)" (Ram.accepts ram (u16 0xDFFF));
  check "rejects 0xBFFF (before)" (not (Ram.accepts ram (u16 0xBFFF)));
  check "rejects 0xE000 (after)" (not (Ram.accepts ram (u16 0xE000)));
  check "starts zeroed" (Uint8.equal (Ram.read_byte ram (u16 0xC123)) Uint8.zero);
  Ram.write_byte ram ~addr:(u16 0xC123) ~data:(u8 0x42);
  check "write then read" (Uint8.equal (Ram.read_byte ram (u16 0xC123)) (u8 0x42));

  print_endline "mock_bus:";
  let bus = Mock_bus.create ~size:0x100 in
  Mock_bus.write_word bus ~addr:(u16 0x10) ~data:(u16 0x1234);
  check "word stored low byte first"
    (Uint8.equal (Mock_bus.read_byte bus (u16 0x10)) (u8 0x34)
     && Uint8.equal (Mock_bus.read_byte bus (u16 0x11)) (u8 0x12));
  check "read_word gets it back"
    (Uint16.equal (Mock_bus.read_word bus (u16 0x10)) (u16 0x1234));

  if !failures = 0 then print_endline "\nall phase 1 tests passed!"
  else (Printf.printf "\n%d test(s) failed\n" !failures; exit 1)