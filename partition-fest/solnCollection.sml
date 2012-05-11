functor SolnCollection (Outcome : OUTCOME) : COMPARABLE_COLLECTION = struct
  structure LP = ListPair
  
  type testid = string * string
  type solnid = string
  type elem = solnid * (testid * Outcome.outcome) list
  type collection = elem list

  exception NotFound
  exception NotImplemented

  val empty = []
  val add = op ::
  val fold = foldr
  val isEmpty = null
  fun getId _ = raise NotImplemented

  fun toString set = foldr (fn ((x, _), y) => x ^ "\n" ^ y) "" set

  fun cmpTests ((id1, num1), (id2, num2)) =
    case String.compare (id1, id2)
      of EQUAL => String.compare (num1, num2)
       | x     => x
  fun cmpTestsO (((id1, num1), _), ((id2, num2), _)) = 
    cmpTests ((id1, num1),(id2, num2))

  fun eq ((id1, ol1), (id2, ol2)) =
    (ListPair.foldrEq (fn ((_,out1), (_,out2), flag) =>
             Outcome.eq (out1, out2) andalso flag) true
             (Util.insertion_sort cmpTestsO ol1, 
              Util.insertion_sort cmpTestsO ol2))
    handle UnequalLengths => false

  fun representative [] = NONE
    | representative (x::_) = SOME x
  fun rep x = case representative x of SOME y => y
                                     | NONE   => raise NotFound


  fun partition f s = 
   let fun partitionOne x l =
       let fun findEqClass [] = [[x]]
             | findEqClass (y::ys) = if f (x, (rep y)) 
                                     then (x::y)::ys
				     else y::(findEqClass ys)
       in findEqClass l
       end
   in foldr (fn (elem, classes) => partitionOne elem classes) [] s
   end


  fun isDNR (_, Outcome.DNR) = true
    | isDNR _ = false

  fun stripDNR outcomes = List.filter 
                           (fn (out1, out2) => 
			     not (isDNR out1 orelse isDNR out2)) outcomes

  (* contract:
       1. caller must ensure that inputs (e1, e2) have the same
          set of test ids

       2. rankElem (e1, e2) gives a partial order of the outcomes
          compared pointwise, ignoring any test on which at least one
          solution DNR
   *)
  val rankElem : elem * elem -> order option = fn ((_, v1), (_, v2)) => 
      (* v1 and v2 are vectors of (tagged) outcomes *)
    let val (v1, v2) = (Util.insertion_sort cmpTestsO v1,
                        Util.insertion_sort cmpTestsO v2)
        val (v1, v2) = (LP.unzip o stripDNR o LP.zip) (v1, v2)
        fun compare ((_, out1), (_,out2)) = Outcome.compare (out1, out2)
    in  Util.vcompare compare (v1, v2)
    end

  val rank : collection * collection -> order option = fn (set1, set2) => 
      rankElem (rep set1, rep set2)

  fun /<=/ (elem1, elem2) = case rankElem (elem1, elem2)
                            of SOME LESS => true
                             | SOME EQUAL => true
                             | _ => false
                                
  fun /</ (elem1, elem2) = case rankElem (elem1, elem2)
                            of SOME LESS => true
                             | _ => false
                                
  fun /==/ (set1, set2) = case rank (set1, set2)
                            of SOME EQUAL => true
                             | _ => false
                                

  fun member (elem, set) = List.exists 
                             (fn e2 => /==/ ([e2], [elem])) set
end
