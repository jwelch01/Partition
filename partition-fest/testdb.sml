functor TestDB (structure M1 : STRING_MAP
		structure M2 : STRING_MAP
                structure M3 : OUTCOME_MAP) : TEST_DB = struct

  type db = M3.map M2.map M1.map * string list

  exception NotDone

  val empty = (M1.empty, [])

  fun member (_, []) = false
    | member (x, y::ys) = if x = y then true else member (x, ys)

  fun add (x, l) = if member (x, l) then l else x::l

  fun testlook (t, m) = M1.lookup (t, m) handle M1.NotFound _ => M2.empty
  fun testnolook (t, m) = M2.lookup (t, m) handle M2.NotFound _ => M3.empty

  fun bind (test, testno, soln, out, (map, list)) = 
    let val testnomap   = testlook (explode test, map)
        val outmap = testnolook (explode testno, testnomap)
    in (M1.bind (explode test,
                 M2.bind (explode testno,
                          M3.bind (explode soln, out, outmap),
                          testnomap),
                 map), add (soln, list))
    end
  
  fun lookup (test, testno, soln, (map, list)) = 
    M3.lookup (explode soln, testnolook(explode testno, 
                             testlook (explode test, map)))
    handle M3.NotFound _ => Outcome.DNR
  fun fold f y (map, list) = 
    M1.mapFold
     (fn (k, m, y2) => 
      M2.mapFold 
         (fn (k2, m2, y3) =>
           let fun checkSolns ([], y4) = y4
                 | checkSolns (x::xs, y4) = 
                    f(implode k, implode k2, x, M3.lookup (explode x, m2), y4)
           in checkSolns (list, y3)
           end)
         y2 m)
    y map

end