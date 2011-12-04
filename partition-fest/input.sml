structure FileReader : sig
  val readToMap : TextIO.instream -> DB.db
end = 
struct

  (* Build a map from tests to student * outcome lists from input file *)
  fun readToMap fd =
    let fun build fd db =
      case TextIO.inputLine fd
        of NONE => db
         | SOME line => let val {num, outcome, solnid, testid} = 
              OutcomeReader.outcome line
                        in build fd 
             (DB.bind (testid, Int.toString num, solnid, outcome, db))
              end
    in build fd DB.empty
    end

end