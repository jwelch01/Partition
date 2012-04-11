structure SolnString = struct

  fun separate (zero, sep) =  (* print list with separator *)
      let fun s []     = zero
            | s [x]    = x
            | s (h::t) = h ^ sep ^ s t
      in  s
      end

  fun elem (soln, results) =
      let fun result ((id, n), outcome) =
             String.concat [Outcome.toString outcome, " ", id, "-", n]
      in  String.concat [soln, ": ",
                         separate ("<no results>", ", ") (map result results)]
      end

  fun set elems = "{" ^ separate (" ", "; ") (map elem elems) ^ "}"

end
                         

