structure OutcomeSingle :> OUTCOME = struct
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }
    | DNR
 
  exception DNRComparison

  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }


fun compare (PASSED, NOTPASSED _)      = GREATER
  | compare (PASSED, PASSED   )        = EQUAL
  | compare (NOTPASSED _, PASSED)      = LESS
  | compare (NOTPASSED _, NOTPASSED _) = EQUAL
  | compare (DNR, DNR)                 = EQUAL
  | compare (_,_)                      = raise DNRComparison

fun eq (o1, o2) = 
  (case compare (o1, o2)
    of EQUAL => true
     | _     => false) handle DNRComparison => false

end

structure OutcomeMultiple :> OUTCOME = struct
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }
    | DNR
 
  exception DNRComparison

fun compare (PASSED, NOTPASSED _) = GREATER
  | compare (PASSED, _) = EQUAL
  | compare (NOTPASSED _, PASSED) = LESS
  | compare (NOTPASSED {outcome = out1, witness = wit1}, 
                 NOTPASSED {outcome = out2, witness = wit2}) = 
			if out1 = out2 then EQUAL
                        else if out1 = "errored" then LESS else GREATER
  | compare (_, _) = EQUAL

fun eq (o1, o2) = 
  (case compare (o1, o2)
    of EQUAL => true
     | _     => false)

end

structure Outcome = OutcomeMultiple