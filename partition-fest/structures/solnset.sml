functor SolnSet (Outcome : OUTCOME) : SET = struct
  type elem = (string * (string * int * Outcome.outcome) list)
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
    of EQUAL => Int.compare (num1, num2)
     | x     => x


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



(* Dangerous, should be designed out. 2 sets are eq if they have the same 
   result for all shared tests. Thus the empty set is eq to all sets. Used to
   avoid problems in subset if a solution has completed more tests than another
   but has the same results on shared tests *) (* TODO : EDIT OUT *)
  fun /==/ (set1, set2) = 
    let fun eq ((t1, num1, out1)::xs, (t2, num2, out2)::ys) = 
         (case cmpTests ((t1, num1), (t2, num2))
            of LESS => eq (xs, (t2, num2, out2)::ys)
             | GREATER => eq ((t1, num1, out1)::xs, ys)
             | EQUAL => if Outcome.eq (out1, out2) then eq (xs, ys)
                                                   else false)
          | eq ([], _) = true
          | eq (_, _) = true
        val (_,list1) = rep set1
        val (_,list2) = rep set2
    in eq (list1, list2)
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
                            | _       => true ) )
          | cmp ([], _) = true
          | cmp (_, _) = true
        val (_,list1) = rep set1
        val (_,list2) = rep set2
    in cmp (list1, list2)
    end

  fun /*/ (_,_)= raise NotImplemented
  fun /+/ (_,_)= raise NotImplemented
  fun /-/ (_,_)= raise NotImplemented

end