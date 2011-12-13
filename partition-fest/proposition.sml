structure Prop :> PROPOSITION where type testset = TestSet.set = 
struct
  type testset = TestSet.set
  type prop = (bool * string * string * string * (string * bool) list)
  type simpProp = (string * (string * bool) list)

  exception AlreadyNegative
  exception Impossible


 fun insertion_sort _ [] = []
 | insertion_sort cmp (x::xs) = insert cmp x (insertion_sort cmp xs)
and insert _ x [] = [x]
 | insert cmp x (l as y::ys) =
      case cmp (x, y) of GREATER => y :: insert cmp x ys
                       | _       => x :: l

  fun cmpPropName ((name, _), (name2,_)) = String.compare (name, name2)

  val propSort : (string * bool) list -> (string * bool) list  = 
      insertion_sort cmpPropName

  fun propExists (_,_,_,_, prop) = 
    foldr (fn ((_,out), flag) => out orelse flag) false prop
  fun testRep s = case TestSet.representative s
                    of SOME y => y
                     | NONE   => raise Impossible

 fun negateProposition (false, _, _, _, _) = raise AlreadyNegative
    | negateProposition (_, test, num, out, l) = 
        let fun neg ((soln, result)::xs) negs = neg xs ((soln, not result)::negs)
              | neg [] negs = negs
        in (false, test, num, out, neg l [])
        end


  fun makePositivePropositionList testSetList = 
    foldr (fn (testList, props) => 
            (Outcome.boolTests (testRep testList)) @ props)
    [] testSetList

  fun filt (_,_,_,_, (soln, p)::props) = 
       if p then not (foldr (fn ((_, out), flag) => out andalso flag) 
                      true props)
            else not (foldr (fn ((_, out), flag) => (not out) andalso flag)
                      true props)
    | filt (_,_,_,_, []) = raise Impossible
    

  fun makePropList testSetList = List.filter filt (
    foldr (fn (prop, props) =>
            (prop::(negateProposition prop)::props))
    [] (makePositivePropositionList testSetList))

  fun condenseNames [] = raise Impossible
    | condenseNames ((b, test, num, out, props)::xs) =
       let val name = if b then test ^ " " ^ num ^ " " ^ out
                          else test ^ " " ^ num ^ " not " ^ out
       in (name ^ (foldr (fn ((b, test, num, out, props), n) =>
                  if b then "\\n" ^ test ^ " " ^ num ^ " " ^ out ^  n
                          else "\\n" ^test ^ " " ^ num ^ " not " ^ out ^ n)
                   "" xs), props)
       end

  fun makePropMapAndSet propList =
   let val (s, m, _) =
    foldr (fn (pList, (s, m, c)) =>
      let val (name, props) = condenseNames pList
          val node = "N" ^ Int.toString(c)
      in (((node,props)::s),
           Map.bind (explode node, name, m),
           c+1)
      end)
    ([], Map.empty, 1) propList
   in (s, m) end


   fun /->/ ((_,prop1), (_,prop2)) =
     ListPair.foldr (fn ((_, out1), (_, out2), flag) =>
                     if out1 then out2 andalso flag
                             else flag)
     true (propSort prop1, propSort prop2)
 
  infixr 0 /->/

  fun eq ((_, _, _, _, prop1), (_, _, _ ,_, prop2)) = 
    ListPair.foldr (fn ((_, out1), (_, out2), flag) =>
                     out1 = out2 andalso flag)
    true (propSort prop1, propSort prop2)

  fun representative [] = NONE
    | representative (x::_) = SOME x
  fun rep x = case representative x of SOME y => y
                                     | NONE   => raise Impossible

  fun edge id1 label id2 = G.makeEdge (G.makeNode id1, label, G.makeNode id2)

  fun partition f s = 
   let fun partitionOne x l =
       let fun findEqClass [] = [[x]]
             | findEqClass (y::ys) = if f (x, (rep y)) 
                                     then (x::y)::ys
				     else y::(findEqClass ys)
       in findEqClass l
       end
   in foldr (fn (elem, classes) => partitionOne elem classes) [] s
   end


  fun makePropGraph propList = 
    foldr (fn ((node, props), impls) =>
      let val impls_ = G.addNode (node, impls)
      in foldr (fn ((node2, props2), impls2) =>
                if (node, props) /->/ (node, props2)
                then G.addEdge (edge node "" node2, impls2) 
                else impls2)
         impls_ propList
      end)
    G.empty propList


end