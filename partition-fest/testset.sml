functor TestSet (Outcome : OUTCOME) : SET = struct
  type elem = (string * string * (string * Outcome.outcome) list)
  type set = elem list

  exception NotFound
  exception NotImplemented

(* Helper functions *)

fun cmpResultName ((x, _), (y, _)) = String.compare (x, y)

fun sort l = Util.insertion_sort cmpResultName l

fun getId (name, num, _) = (name, num)

fun eq ((_, _, ol), (_, _, ol2)) = 
       (ListPair.foldrEq (fn ((_,out1), (_,out2), flag) =>
		Outcome.eq (out1, out2) andalso flag) true (ol, ol2)
        handle UnequalLengths => false)


fun eqResult ((id, num, ol), (id2, num2, ol2)) = 
  id = id2 andalso num = num2 andalso eq ((id, num, ol), (id2, num2, ol2))

(* real functions *)
  val empty = []
  fun add (x, s) = x::s

  fun member ((id, num, ol), s) = List.exists 
    (fn (id2, num2, ol2) => eqResult ((id, num, ol), (id2, num2, ol2))) s

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

  fun /</ (_,_) = raise NotImplemented
  fun /==/ (set1, set2) = eqResult (rep set1, rep set2)

  fun /*/  (_,_) = raise NotImplemented
  fun /+/  (_,_) = raise NotImplemented
  fun /-/  (_,_) = raise NotImplemented

end