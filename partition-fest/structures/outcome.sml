structure Outcome :> OUTCOME = struct
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }

  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }



fun compareOutcome (PASSED, PASSED)  = EQUAL
  | compareOutcome (PASSED, _   )  = GREATER
  | compareOutcome (NOTPASSED _, PASSED)  = LESS
  | compareOutcome (NOTPASSED _, _   )  = EQUAL

fun eqOutcome (o1, o2) = 
  (case compareOutcome (o1, o2)
    of EQUAL => true
     | _     => false)

fun sameTest ((id1, num1), (id2, num2)) = 
  id1= id2 andalso num1 = num2

exception DifferentTests
fun compare ((id1, num1, sid1, out1), (id2, num2, sid2, out2)) = 
                       if sameTest ((id1, num1), (id2, num2))
		       then compareOutcome (out1, out2)
		       else raise DifferentTests

fun eq ((id1, num1, sid1, out1), (id2, num2, sid2, out2)) =
  sameTest ((id1, num1), (id2, num2)) andalso eqOutcome (out1, out2)

end
