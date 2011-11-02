structure Set : SET = struct
  type elem = 'a
  type set = elem list

  exception NotFound
  exception NotFinished


  val empty = []
  fun add x s = x::s
  fun member x s = List.exists (fn y => y=x) s
  fun representative [] = raise NotFound
    | representative (x::_) = x


  fun fold f y s = foldr f y s


  fun partition f s = 
   let fun partitionOne x l =
       let fun findEqClass [] = [[x]]
             | findEqClass (y::ys) = if f (x, (representative y)) 
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