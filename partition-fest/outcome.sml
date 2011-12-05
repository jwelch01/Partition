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

fun identical (PASSED, PASSED) = true
  | identical (NOTPASSED _, NOTPASSED _) = true
  | identical (DNR, DNR) = true
  | identical (_,_) = false

fun boolTests (test, num, l) = 
  let fun f ((soln, out)::xs) outGoal bools= 
        if identical (out, outGoal) then f xs outGoal ((soln,true)::bools)
                                    else f xs outGoal ((soln,false)::bools)
        | f [] _ bools = bools
  in (true, test, num, "PASSED", f l PASSED [])::
     (true, test, num, "NOTPASSED", f l (NOTPASSED {outcome = "", witness = ""}) [])::
     (true, test, num, "DNR", f l DNR [])::[]
  end

end
