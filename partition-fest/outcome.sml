structure Outcome :> OUTCOME = struct
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }
    | DNR

  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }


fun compare (PASSED, NOTPASSED _)  = GREATER
  | compare (PASSED, _   )         = EQUAL
  | compare (NOTPASSED _, PASSED)  = LESS
  | compare (NOTPASSED _, _   )    = EQUAL
  | compare (DNR, _)               = EQUAL

fun eq (o1, o2) = 
  (case compare (o1, o2)
    of EQUAL => true
     | _     => false)

end
