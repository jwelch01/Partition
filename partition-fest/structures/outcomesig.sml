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
  val compare : t * t -> order

  val eq : t * t -> bool (* does not compare solnid *)
end