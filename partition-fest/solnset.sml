functor SolnSet (Outcome : OUTCOME) : SET = struct
  structure LP = ListPair
  
  type testid = string * string
  type solnid = string
  type elem = solnid * (testid * Outcome.outcome) list
  type set = elem list

  exception NotFound
  exception NotImplemented

(* Helper functions *)

fun cmpTests ((id1, num1), (id2, num2)) =
  case String.compare (id1, id2)
    of EQUAL => String.compare (num1, num2)
     | x     => x

fun getId _ = raise NotImplemented


fun cmpTestsO (((id1, num1), _), ((id2, num2), _)) = 
  cmpTests ((id1, num1),(id2, num2))

fun eq ((id1, ol1), (id2, ol2)) =
  (ListPair.foldrEq (fn ((_,out1), (_,out2), flag) =>
           Outcome.eq (out1, out2) andalso flag) true
           (Util.insertion_sort cmpTestsO ol1, Util.insertion_sort cmpTestsO ol2))
  handle UnequalLengths => false


(* Real functions *)

  val empty = []
  fun add (x, s) = x::s

  fun representative [] = NONE
    | representative (x::_) = SOME x
  fun rep x = case representative x of SOME y => y
                                     | NONE   => raise NotFound

  fun isEmpty [] = true
    | isEmpty _ = false

  fun fold f y s = foldr f y s

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

(* A < B iff for all shared tests, if A has passed, B has passed *)
  
  fun isDNR (_, x)  = Outcome.eq (x, Outcome.DNR)
  fun stripDNR outcomes = List.filter 
                           (fn (out1, out2) => 
			     not (isDNR out1 orelse isDNR out2)) outcomes

  fun /<=/ (set1, set2) =
    let val ((_, out1), (_, out2)) = (rep set1, rep set2)
        val (out1, out2) = (Util.insertion_sort cmpTestsO out1,
                            Util.insertion_sort cmpTestsO out2)
        val (out1, out2) = (LP.unzip o stripDNR o LP.zip) (out1, out2)
        fun compare ((_, out1), (_,out2)) = Outcome.compare (out1, out2)
        val rank = Util.vcompare compare (out1, out2)
    in rank = SOME LESS
    end

(* A = B iff for all tests T, T(A) = T(B). Soln name is not considered *)
  fun /==/ (set1, set2) =
    let val ((_, out1), (_, out2)) = (rep set1, rep set2)
        val (out1, out2) = (Util.insertion_sort cmpTestsO out1,
                            Util.insertion_sort cmpTestsO out2)
        fun compare ((_, out1), (_,out2)) = Outcome.compare (out1, out2)
        val rank = Util.vcompare compare (out1, out2)
    in rank = SOME EQUAL
    end

  fun member (elem, set) = List.exists 
                             (fn e2 => /==/ ([e2], [elem])) set

end
