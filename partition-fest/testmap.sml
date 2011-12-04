functor TestMap(structure L1 : FINITE_MAP 
                structure L2 : FINITE_MAP
		structure OUT: OUTCOME    ) : MONO_MAP = struct

  type key = (L1.key * L2.key)  

  type value = OUT.outcome

  type map = value L2.map L1.map * string list

  val empty = (L1.empty, [])

  exception NotFound of key

  fun member (x, []) = false
    | member (x, y::ys) = if x = y then true
                                   else member (x, ys)

  fun look (k, (m,_)) = L1.lookup (k, m) handle L1.NotFound _ => L2.empty

  fun bind ((k1, k2), v, (m,solns)) = 
      (L1.bind (k1, (L2.bind (k2, v,look (k1, (m,[])))), m), 
       if member (implode k1, solns) then [] else [])
  fun lookup ((k1, k2), (m,_)) =  L2.lookup (k2, (L1.lookup (k1, m)))

  fun mapFold f y (m,_) =
    let fun fold_inner (k, m, y) = L2.mapFold (fn (k2, v, y2) =>
                                               f ((k, k2), v, y2)) y m
    in L1.mapFold fold_inner y m
    end 
end