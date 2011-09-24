functor ListValFn(M : FINITE_MAP) : LIST_MAP = struct
  
  type Key = M.Key 
  type 'a Map = 'a list M.Map

  val empty = M.empty
 
  fun lookup (k, m) = M.lookup(k, m) handle M.NotFound _ => []
  
  fun add (k, v, m) = M.bind(k, v :: lookup(k, m), m)

end