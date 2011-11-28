functor SolnSet (Outcome : OUTCOME) : SET = struct
  type elem = (string * (string * string * Outcome.outcome) list)
  type set = elem list

  exception NotFound
  exception NotImplemented

(* Helper functions *)

fun insertion_sort _ [] = []
 | insertion_sort cmp (x::xs) = insert cmp x (insertion_sort cmp xs)
and insert _ x [] = [x]
 | insert cmp x (l as y::ys) =
      case cmp (x, y) of GREATER => y :: insert cmp x ys
                       | _       => x :: l

fun cmpTests ((id1, num1), (id2, num2)) =
  case String.compare (id1, id2)
    of EQUAL => String.compare (num1, num2)
     | x     => x

fun cmpTestsO ((id1, num1, _), (id2, num2, _)) = 
  cmpTests ((id1, num1),(id2, num2))

fun eq ((id1, ol1), (id2, ol2)) =
  (ListPair.foldrEq (fn ((_,_,out1), (_,_,out2), flag) =>
           Outcome.eq (out1, out2) andalso flag) true
           (insertion_sort cmpTestsO ol1, insertion_sort cmpTestsO ol2))
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

(* equivalence, tests a representative of each set without respect
   to soln name *)
  fun /==/ (set1, set2) = 
    let fun eq ((t1, num1, out1)::xs, (t2, num2, out2)::ys) = 
         (case cmpTests ((t1, num1), (t2, num2))
            of EQUAL => if Outcome.eq (out1, out2) then eq (xs, ys)
                                                   else false
             | _     => false)
          | eq ([], []) = true
          | eq (_, _) = false
        val (_,list1) = rep set1
        val (_,list2) = rep set2
    in eq (insertion_sort cmpTestsO list1, insertion_sort cmpTestsO list2)
    end

fun member (x, y) = raise NotImplemented
(*
  fun member ((id, ol), s) = 
    let val tmpSet = add ((id, ol), empty)
    in List.exists (fn (id2, ol2) => /==/ ((id2, ol2), tmpSet)) s
    end
*)
(* A < B iff for all shared tests, if A has passed, B has passed *)
  fun /<=/ (set1, set2) =
    let fun cmp ((t1, num1, out1)::xs, (t2, num2, out2)::ys) = 
         (case cmpTests ((t1, num1), (t2, num2))
            of LESS => cmp (xs, (t2, num2, out2)::ys)
             | GREATER => cmp ((t1, num1, out1)::xs, ys)
             | EQUAL => (case Outcome.compare (out1, out2)
                           of GREATER => false
                            | _       => cmp(xs, ys)))
          | cmp ([], _) = true
          | cmp (_, []) = true
        val (_,list1) = rep set1
        val (_,list2) = rep set2
    in cmp (insertion_sort cmpTestsO list1, insertion_sort cmpTestsO list2)
    end

(*
  fun /<=/ (set1, set2) =
    let fun cmp ((t1, num1, out1)::xs, (t2, num2, out2)::ys) = 
         (case cmpTests ((t1, num1), (t2, num2))
            of LESS => false
             | GREATER => false
             | EQUAL => (case Outcome.compare (out1, out2)
                           of GREATER => false
                            | _       => cmp(xs, ys)))
          | cmp ([], _) = true
          | cmp (_, []) = true
        val (_,list1) = rep set1
        val (_,list2) = rep set2
    in cmp (insertion_sort cmpTestsO list1, insertion_sort cmpTestsO list2)
    end
*)

  fun /*/ (_,_)= raise NotImplemented
  fun /+/ (_,_)= raise NotImplemented
  fun /-/ (_,_)= raise NotImplemented

end