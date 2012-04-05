signature OUTCOME  = sig
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }
    | DNR
  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }

  val compare     : outcome * outcome -> order
  val compareMult : outcome * outcome -> order

  val eq            : outcome * outcome -> bool 
  val eqMult        : outcome * outcome -> bool
  val identical     : outcome * outcome -> bool
  val identicalMult : outcome * outcome -> bool

  val boolTests : string * string * (string * outcome) list ->
                 (bool * string * string * string * (string * bool) list) list

end