functor Set (Outcome : OUTCOME) : SET = struct
  type elem = Outcome.t
  type set = elem list

  exception NotFound
  exception NotFinished


  val empty = []
  fun add (x, s) = x::s
  fun member (x, s) = List.exists (fn y => Outcome.eq (y,x)) s
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

  fun /<=/ (_,_) = raise NotFinished
  fun /==/ (_,_) = raise NotFinished

  fun /*/  (_,_) = raise NotFinished
  fun /+/  (_,_) = raise NotFinished
  fun /-/  (_,_) = raise NotFinished

end