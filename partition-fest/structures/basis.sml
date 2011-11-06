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
fun eqResult ((id, num, ol), (id2, num2, ol2)) = 
       (ListPair.foldrEq (fn ((_,out1), (_,out2), flag) =>
		Outcome.eq (out1, out2) andalso flag) true (ol, ol2)
        handle UnequalLengths => false)
val partitionTests = tSet.partition eqResult

(* Make map from solns -> test * outcome list using a representative from each
eq class *)

(* Turn map into SolnSet *)

(* Partition SolnSet *)

(* Find subset relations *)

(* produce graph *)