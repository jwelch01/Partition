functor TestCollection (Outcome : OUTCOME) : COMPARABLE_COLLECTION = struct
  type elem = (string * string * (string * Outcome.outcome) list)
  type collection = elem list

  exception NotFound
  exception NotImplemented

  val empty = []
  val isEmpty = null
  val fold = foldr
  val add = op ::

  fun getId (name, num, _) = (name, num)

  fun eq ((_, _, ol), (_, _, ol2)) = 
         (ListPair.foldrEq (fn ((_,out1), (_,out2), flag) =>
	   Outcome.eq (out1, out2) andalso flag) true (ol, ol2)
         handle UnequalLengths => false)

  fun fullyEq ((id, num, ol), (id2, num2, ol2)) = 
    id = id2 andalso num = num2 andalso eq ((id, num, ol), (id2, num2, ol2))

  fun representative [] = NONE
    | representative (x::_) = SOME x

  fun rep x = case representative x of SOME y => y
                                     | NONE   => raise NotFound

  fun toString set = 
    let fun idToString (name, num) = name ^ " " ^ num
    in foldr (fn (x, y) => (idToString o getId) x ^ "\n" ^ y) "" set
    end


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
  fun /==/ (set1, set2) = eq (rep set1, rep set2)


  fun member ((id, num, ol), s) = List.exists 
    (fn (id2, num2, ol2) => fullyEq ((id, num, ol), (id2, num2, ol2))) s

end