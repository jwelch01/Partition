structure Outcome = struct
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }

  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }
end

