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

fun compareMult (PASSED, NOTPASSED _) = GREATER
  | compareMult (PASSED, _) = EQUAL
  | compareMult (NOTPASSED _, PASSED) = LESS
  | compareMult (NOTPASSED {outcome = out1, witness = wit1}, 
                 NOTPASSED {outcome = out2, witness = wit2}) = 
			if out1 = out2 then EQUAL
                        else if out1 = "errored" then LESS else GREATER
  | compareMult (_, _) = EQUAL

fun eq (o1, o2) = 
  (case compare (o1, o2)
    of EQUAL => true
     | _     => false)


fun eqMult (o1, o2) = 
  (case compareMult (o1, o2)
    of EQUAL => true
     | _     => false)
fun identical (PASSED, PASSED) = true
  | identical (NOTPASSED _, NOTPASSED _) = true
  | identical (DNR, DNR) = true
  | identical (_,_) = false

fun identicalMult (PASSED, PASSED) = true
  | identicalMult (NOTPASSED {outcome = out1, witness = wit1}, 
               NOTPASSED {outcome = out2, witness = wit2}) = out1 = out2
  | identicalMult (DNR, DNR) = true
  | identicalMult (_,_) = false

fun boolTests (test, num, l) = 
  let fun f ((soln, out)::xs) outGoal bools= 
        if identical (out, outGoal) then f xs outGoal ((soln,true)::bools)
                                    else f xs outGoal ((soln,false)::bools)
        | f [] _ bools = bools
  in (true, test, num, "PASSED", f l PASSED [])::
     (true, test, num, "FAILED", f l (NOTPASSED {outcome = "", witness = ""}) [])::
     (true, test, num, "DNR", f l DNR [])::[]
  end

end
