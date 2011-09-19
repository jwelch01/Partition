include ORD_KEY

type key = Key.ord_key
type item = Key.ord_key list

datatype outcome = PASSED | FAILED
type result = item * outcome
type testnum = int



(* value is the complete list of results for a given test *)
datatype set = NODE of { key : key, value: (testnum * result list) list, 
                         lt : set, eq : set, gt : set }
             | LEAF

val empty = LEAF

fun member (_, LEAF) = false
  | member (h::t, NODE {key, v, lt, eq, gt}) =
      (case Key.compare (h, key)
         of EQUAL   => member(t, eq)
          | LESS    => member(h::t, lt)
          | GREATER => member(h::t, gt))
  | member ([], NODE {key, v, lt, eq, gt}) =
      (case Key.compare (Key.sentinel, key)
         of EQUAL   => true
          | LESS    => member([], lt)
          | GREATER => member([], gt))

fun addToTestSet([], tnum, r) = [(tnum, [r])]
  | addToTestSet((tnum1, rlist)::l, tnum2, r) = 
      if(tnum1 = tnum2)
      then (tnum1, r::rlist)::l
      else (tnum1, [r])::(tnum1, rlist)::l

fun insert(h::t, LEAF, tnum, r) = 
      NODE { key = h, value = [], 
             eq = insert(t, LEAF, tnum, r), lt = LEAF, gt = LEAF}
  | insert([], LEAF) =
      NODE { key = Key.sentinel, value = [(tnum, [r])], 
             eq = LEAF, lt = LEAF, gt = LEAF }
  | insert(h::t, NODE{key, v, lt, eq, gt}) =
      (case Key.compare (h, key)
         of EQUAL   => NODE {key = key, value = [],
                             lt = lt, gt = gt, eq = insert(t, eq, tnum, r)}
          | LESS    => NODE {key = key, value = [], 
                             lt = insert(h::t, lt), gt = gt, eq = eq}
          | GREATER => NODE {key = key, value = [],
                             lt = lt, gt = insert(h::t, gt, tnum, r), eq = eq})
  | insert([], NODE {key, v, lt, eq, gt}) =
      (case Key.compare (Key.sentinel, key)
         of EQUAL   => NODE {key = key, value = addToTestSet(v, tnum, r),
                             lt = lt, gt = gt, eq = eq}
          | LESS    => NODE {key = key, value = v, 
                             lt = insert([], lt), gt = gt, eq = eq}
          | GREATER => NODE {key = key, value = v,
                             lt = lt, gt = (insert [], gt), eq = eq})