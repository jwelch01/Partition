functor StringMapFn(Key: TERNARY_KEY) : STRING_MAP = struct
  structure Key = Key 

  datatype 'a map = NODE of { key : Key.ord_key, value: 'a, 
                              lt : 'a map, eq : 'a map, gt : 'a map }
                  | LEAF

  exception NotFound of string
  exception AlreadyPresent

  val empty  : 'a map = LEAF

  fun bind (h::t,x,LEAF) = 
        NODE {key = h, value = x,
              lt = LEAF, gt = LEAF, eq  = bind (t, x, LEAF)}
    | bind ([], x, LEAF) = 
        NODE { key = Key.sentinel, eq = LEAF, lt = LEAF, gt = LEAF, value = x}
    | bind (h::t, x, NODE {key, value, lt, eq, gt}) =
        (case Key.compare (h, key)
           of LESS    => NODE {key = key, value = value,
                               gt = gt, lt = bind (h::t, x, lt), eq = eq}
            | GREATER =>  NODE {key = key, value = value,
				lt = lt, eq = eq, gt = bind(h::t, x, gt)}
            | EQUAL   =>  NODE {key = key, value = value,
                                lt = lt, gt = gt, eq  = bind(t, x, eq)})
    | bind ([], x, NODE {key, value, lt, eq, gt}) =
	(case Key.compare (Key.sentinel, key)
           of LESS    => NODE{key = key, value = value,
                              lt = bind([], x, lt), gt = gt, eq = eq}
            | GREATER => NODE {key = key, value = value,
			       lt = lt, gt = bind ([], x, gt), eq = eq}
	    | EQUAL   => NODE {key = key, value = x,
				lt = lt, gt = gt, eq = eq})
  fun lookup (n, LEAF) = raise NotFound (implode n)
    | lookup (h::t, NODE {key, value, lt, eq, gt}) =
	(case Key.compare (h, key)
           of EQUAL   => lookup(t, eq)
            | LESS    => lookup(h::t, lt)
            | GREATER => lookup(h::t, gt))
    | lookup ([], NODE {key, value, lt, eq, gt}) =
        (case Key.compare (Key.sentinel, key)
           of EQUAL   => value
            | LESS    => lookup([], lt)
            | GREATER => lookup([], gt))

fun mapFold f y m =
  let fun fold _ y _ LEAF = y
        | fold f y k (NODE {key, value, lt, eq, gt}) =
            (case Key.compare (Key.sentinel, key)
               of EQUAL => f(k, value, (fold f (fold f y k lt) k gt))
                | _     => fold f (fold f (fold f y (k @ (key::[])) eq) k lt) k gt)
  in fold f y [] m 
  end



end