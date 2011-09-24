functor TernaryStringMap(Key: TERNARY_KEY) : FINITE_MAP = struct
  structure Key = Key
  type key = string

  datatype 'a map = NODE of { key : Key.ord_key, value: 'a, 
                              lt : 'a map, eq : 'a map, gt : 'a map }
                  | LEAF