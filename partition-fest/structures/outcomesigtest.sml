signature OUTCOME  = sig
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }
  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }

  val compareOutcome : outcome * outcome -> order
  val eqOutcome      : outcome * outcome -> bool

  val sameTest : (string * int) * (string * int)  -> bool

  exception DifferentTests
  val compare : (string * int * string * outcome) * (string * int * string * outcome)  -> order

  val eq : (string * int * string * outcome) * (string * int * string * outcome)  -> bool 
           (* does not compare solnid *)
end