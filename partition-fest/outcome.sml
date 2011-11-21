structure Outcome :> OUTCOME = struct
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }

  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }


fun compare (PASSED, PASSED)  = EQUAL
  | compare (PASSED, _   )  = GREATER
  | compare (NOTPASSED _, PASSED)  = LESS
  | compare (NOTPASSED _, _   )  = EQUAL

fun eq (o1, o2) = 
  (case compare (o1, o2)
    of EQUAL => true
     | _     => false)

end
