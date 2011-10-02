functor ListMapFn(M : FINITE_MAP) : LIST_MAP = struct
  
  type key = M.key; 
  type 'a map = 'a list M.map

  val empty = M.empty
 
  fun lookup (k, m) = M.lookup(k, m) handle M.NotFound _ => []
  
  fun add (k, v, m) = M.bind(k, v :: lookup(k, m), m)

  fun bind (k, v,m) = M.bind(k, [v], m)

  fun mapFold f y m = M.mapFold f y m

end