structure BasicMap : FiniteMap = struct
  type Key = string
  type 'a Map = (Key * 'a) list

  exception NotFound of Key

  val empty : 'a Map = []
  fun bind (n, x, m) = (n,x)::m
  fun lookup(n,[]) = raise NotFound n
    | lookup(n, (n', x)::l) = 
        if n = n' then x else lookup (n,l)
end