structure Outcome = struct
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }

  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }



fun cmpOutcomes (PASSED, PASSED)  = EQUAL
  | cmpOutcomes (PASSED, _   )  = GREATER
  | cmpOutcomes (NOTPASSED _, PASSED)  = LESS
  | cmpOutcomes (NOTPASSED _, _   )  = EQUAL

fun cmpOutcomesEq (o1, o2) = 
  (case cmpOutcomes (o1, o2)
    of EQUAL => true
     | _     => false)

end

