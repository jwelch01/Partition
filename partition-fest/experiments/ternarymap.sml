functor TernaryStringMap(Key: TERNARY_KEY) : FiniteMap = struct
  structure key = Key 

  type Key = Key.ord_key list
  
  datatype 'a Map = NODE of { key : Key.ord_key, value: 'a, 
                              lt : 'a Map, eq : 'a Map, gt : 'a Map }
                  | LEAF

  exception NotFound of Key
  exception AlreadyPresent

  val empty  : 'a Map = LEAF

  fun bind (h::t,x,LEAF) = 
        NODE {key = h, value = x,
              lt = LEAF, gt = LEAF, eq  = bind (t, x, LEAF)}
    | bind ([], x, LEAF) = 
        NODE { key = key.sentinel, eq = LEAF, lt = LEAF, gt = LEAF, value = x}
    | bind (h::t, x, NODE {key, value, lt, eq, gt}) =
        (case key.compare (h, key)
           of LESS    => NODE {key = key, value = value,
                               gt = gt, lt = bind (h::t, x, lt), eq = eq}
            | GREATER =>  NODE {key = key, value = value,
				lt = lt, eq = eq, gt = bind(h::t, x, gt)}
            | EQUAL   =>  NODE {key = key, value = value,
                                lt = lt, gt = gt, eq  = bind(t, x, eq)})
    | bind ([], x, NODE {key, value, lt, eq, gt}) =
	(case key.compare (key.sentinel, key)
           of LESS    => NODE{key = key, value = value,
                              lt = bind([], x, lt), gt = gt, eq = eq}
            | GREATER => NODE {key = key, value = value,
			       lt = lt, gt = bind ([], x, gt), eq = eq}
	    | EQUAL   => raise AlreadyPresent)
  fun lookup (n, LEAF) = raise NotFound n
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
end