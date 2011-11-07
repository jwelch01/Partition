structure map = TernaryStringMap(TernaryKeyChar)
structure listMap = ListMapFn(map)
structure m = EmbedMap(structure L1 = map
                       structure L2 = listMap)
structure g = BasicGraph
structure tSet = TestSet(Outcome)
structure sSet = SolnSet(Outcome)


(* Build a map from tests to student * outcome lists from input file *)
fun readToMap fd =
  let fun build fd map =
    case TextIO.inputLine fd
      of NONE => map
       | SOME line => let val {num, outcome, solnid, testid} = 
			  OutcomeReader.outcome line
                      in build fd 
			 (m.add ((explode testid, explode (Int.toString num)), 
                                (solnid, outcome), 
				map))
		      end
  in build fd m.empty
  end


(* Turn map into a TestSet *)
fun makeTestSet map = m.mapFold 
  (fn ((k1,k2), resultList, set) => tSet.add ((implode k1, implode k2, resultList), set))
  tSet.empty map

(* Partition TestSet *)
val partitionTests = tSet.partition tSet.eq

(* Make map from solns -> test * outcome list using a representative from each
eq class *)
exception Impossible
fun addToMap (set, map) = case tSet.representative set
                            of SOME (name, number, ol) => foldr
                               (fn ((soln, out), m) => 
                                 listMap.add (explode soln, 
                                             (name, number, out),m))
                               map ol
                             | NONE => raise Impossible

fun makeSolnMap set = foldr addToMap listMap.empty set

(* Turn map into SolnSet *)
fun makeSolnSet map = listMap.mapFold
  (fn (k, testList, set) => sSet.add ((implode k, testList), set))
  sSet.empty map

(* Partition SolnSet *)
val partitionSolns = sSet.partition sSet.eq

(* Find subset relations *)

(* produce graph *)