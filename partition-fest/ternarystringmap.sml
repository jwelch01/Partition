functor StringMap(Key: TERNARY_KEY) : FINITE_MAP = struct
  structure Key = Key 

  type key = string
  
  datatype 'a map = NODE of { key : Key.ord_key, value: 'a, 
                              lt : 'a map, eq : 'a map, gt : 'a map }
                  | LEAF

  exception NotFound of key
  exception AlreadyPresent

  val empty  : 'a map = LEAF



  fun bind0 (h::t,x,LEAF) = 
        NODE {key = h, value = x,
              lt = LEAF, gt = LEAF, eq  = bind0 (t, x, LEAF)}
    | bind0 ([], x, LEAF) = 
        NODE { key = Key.sentinel, eq = LEAF, lt = LEAF, gt = LEAF, value = x}
    | bind0 (h::t, x, NODE {key, value, lt, eq, gt}) =
        (case Key.compare (h, key)
           of LESS    => NODE {key = key, value = value,
                               gt = gt, lt = bind0 (h::t, x, lt), eq = eq}
            | GREATER =>  NODE {key = key, value = value,
				lt = lt, eq = eq, gt = bind0(h::t, x, gt)}
            | EQUAL   =>  NODE {key = key, value = value,
                                lt = lt, gt = gt, eq  = bind0(t, x, eq)})
    | bind0 ([], x, NODE {key, value, lt, eq, gt}) =
	(case Key.compare (Key.sentinel, key)
           of LESS    => NODE{key = key, value = value,
                              lt = bind0([], x, lt), gt = gt, eq = eq}
            | GREATER => NODE {key = key, value = value,
			       lt = lt, gt = bind0 ([], x, gt), eq = eq}
	    | EQUAL   => NODE {key = key, value = x,
				lt = lt, gt = gt, eq = eq})

  fun bind (key, value, root) = bind0 (explode key, value, root)
  
  exception NotF

  fun look (n, LEAF) = raise NotF
    | look (h::t, NODE {key, value, lt, eq, gt}) =
	(case Key.compare (h, key)
           of EQUAL   => look(t, eq)
            | LESS    => look(h::t, lt)
            | GREATER => look(h::t, gt))
    | look ([], NODE {key, value, lt, eq, gt}) =
        (case Key.compare (Key.sentinel, key)
           of EQUAL   => value
            | LESS    => look([], lt)
            | GREATER => look([], gt))

  fun lookup (key, root) = look (explode key, root)
                           handle NotF => raise NotFound key


fun mapFold f y m =
  let fun fold _ y _ LEAF = y
        | fold f y k (NODE {key, value, lt, eq, gt}) =
            (case Key.compare (Key.sentinel, key)
               of EQUAL => f(implode k, value, (fold f (fold f y k lt) k gt))
                | _     => fold f (fold f (fold f y (k @ (key::[])) eq) k lt) k gt)
  in fold f y [] m 
  end



end
