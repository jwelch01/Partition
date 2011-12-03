functor EmbedListMap(structure L1 : FINITE_MAP 
                     structure L2 : LIST_MAP) : LIST_MAP = struct

  type key = (L1.key * L2.key)

  type 'a map = 'a L2.map L1.map

  val empty = L1.empty

  fun look (k, m) = L1.lookup (k, m) handle L1.NotFound _ => L2.empty

  fun add ((k1,k2), v, m) = L1.bind (k1, (L2.add (k2, v, look (k1, m))), m)
  fun bind ((k1, k2), v, m) = L1.bind (k1, (L2.bind (k2, v,look (k1, m))), m)
  fun lookup ((k1, k2), m) = L2.lookup (k2, (look (k1, m)))

  fun mapFold f y m =
    let fun fold_inner (k, m, y) = L2.mapFold (fn (k2, v, y2) =>
                                               f ((k, k2), v, y2)) y m
    in L1.mapFold fold_inner y m
    end 
end