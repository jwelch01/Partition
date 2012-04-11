signature OUTCOME = sig
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }
    | DNR

  exception DNRComparison

  val compare     : outcome * outcome -> order
  val eq            : outcome * outcome -> bool 

end

(*
signature OUTCOME  = sig
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }
    | DNR
  
  structure CmpSingle   : OUTCOME_COMPARISON where type outcome = outcome
  structure CmpMultiple : OUTCOME_COMPARISON where type outcome = outcome

  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }

end
*)
