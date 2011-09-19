functor ListTernaryFn(Key : TERNARY_KEY) : LTERN = struct
  structure Key = Key

  type key = Key.ord_key
  type item = Key.ord_key list

  datatype set = NODE of { key : Key.ord_key, lt : set, eq : set, gt : set }
               | LEAF

  val empty = LEAF

  fun member (_, LEAF) = false
    | member (h::t, NODE {key, lt, eq, gt}) =
        (case Key.compare (h, key)
           of EQUAL   => member(t, eq)
            | LESS    => member(h::t, lt)
            | GREATER => member(h::t, gt))
    | member ([], NODE {key, lt, eq, gt}) =
        (case Key.compare (Key.sentinel, key)
           of EQUAL   => true
            | LESS    => member([], lt)
            | GREATER => member([], gt))

  exception AlreadyPresent

  fun insert(h::t, LEAF) =
        NODE { key = h, eq = insert(t, LEAF), lt = LEAF, gt = LEAF }
    | insert([], LEAF) =
        NODE { key = Key.sentinel, eq = LEAF, lt = LEAF, gt = LEAF }
    | insert(h::t, NODE {key, lt, eq, gt}) =
        (case Key.compare (h, key)
           of EQUAL   => NODE {key = key, lt = lt, gt = gt, eq = insert(t, eq)}
            | LESS    => NODE {key = key, lt = insert(h::t, lt), gt = gt, eq = eq}
            | GREATER => NODE {key = key, lt = lt, gt = insert(h::t, gt), eq = eq})
    | insert([], NODE {key, lt, eq, gt}) =
        (case Key.compare (Key.sentinel, key)
           of EQUAL   => raise AlreadyPresent
            | LESS    => NODE {key = key, lt = insert([], lt), gt = gt, eq = eq}
            | GREATER => NODE {key = key, lt = lt, gt = insert([], gt), eq = eq})

  fun add(l, n) = insert(l, n) handle AlreadyPresent => n
  val addNotPresent = insert
end
