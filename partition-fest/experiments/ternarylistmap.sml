functor TernaryStringMap(Key: TERNARY_KEY) : LIST_MAP where type Key = Key.ord_key list = struct
  structure key = Key 

  type Key = Key.ord_key list
  
  datatype 'a Map = NODE of { key : Key.ord_key, value: 'a list, 
                              lt : 'a Map, eq : 'a Map, gt : 'a Map }
                  | LEAF

  exception NotFound of Key
  exception AlreadyPresent

  val empty  : 'a Map = LEAF

  fun add (h::t,x,LEAF) = 
        NODE {key = h, value = [],
              lt = LEAF, gt = LEAF, eq  = add (t, x, LEAF)}
    | add ([], x, LEAF) = 
        NODE { key = key.sentinel, eq = LEAF, lt = LEAF, gt = LEAF, value = [x]}
    | add (h::t, x, NODE {key, value, lt, eq, gt}) =
        (case key.compare (h, key)
           of LESS    => NODE {key = key, value = value,
                               gt = gt, lt = add (h::t, x, lt), eq = eq}
            | GREATER =>  NODE {key = key, value = value,
				lt = lt, eq = eq, gt = add(h::t, x, gt)}
            | EQUAL   =>  NODE {key = key, value = value,
                                lt = lt, gt = gt, eq  = add(t, x, eq)})
    | add ([], x, NODE {key, value, lt, eq, gt}) =
	(case key.compare (key.sentinel, key)
           of LESS    => NODE{key = key, value = value,
                              lt = add([], x, lt), gt = gt, eq = eq}
            | GREATER => NODE {key = key, value = value,
			       lt = lt, gt = add ([], x, gt), eq = eq}
	    | EQUAL   => NODE {key = key, value = x::value,
			       lt = lt, gt = gt, eq = eq})
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