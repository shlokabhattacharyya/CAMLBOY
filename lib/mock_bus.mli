(* mock bus for tests: just one flat array of memory, starting from address 0 *)

type t 

val create : size:int -> t 

include Word_addressable_intf.S with type t := t