structure FileReader : sig
  val readToMap : TextIO.instream -> (string * Outcome.outcome) M.map
end = 
struct

  (* Build a map from tests to student * outcome lists from input file *)
  fun readToMap fd =
    let fun build fd map =
      case TextIO.inputLine fd
        of NONE => map
         | SOME line => let val {num, outcome, solnid, testid} = 
              OutcomeReader.outcome line
                        in build fd 
             (M.add ((explode testid, explode (Int.toString num)), 
                                  (solnid, outcome), 
                map))
              end
    in build fd M.empty
    end

end