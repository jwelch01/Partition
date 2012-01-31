structure Prop :> PROPOSITION = struct
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

  val cmp : prop * prop -> order =
  fn ((b1, n1, num1, test1, l1), (b2, n2, num2, test2, l2)) => 
    if n1 < n2 then LESS
     else if n1 = n2 then if num1 < num2 then LESS
		           else if num1 = num2 then if test1 < test2 then LESS
                                                     else if test1 = test2
                                                          then EQUAL
							  else GREATER
                                 else GREATER
           else GREATER

  val equiv : prop * prop * bool -> bool =
  fn ((_, n1, num1, test1, _), (_, n2, num2, test2, _), flag) =>
    n1 = n2 andalso num1 = num2 andalso test1 = test2 andalso flag

  val equivPropLists : prop list * prop list -> bool =
  fn (l1, l2) => 
   ListPair.foldrEq equiv true (insertion_sort cmp l1, insertion_sort cmp l2)
   handle UnequalLengths => false
(*
  fun addPropList (l1, propCollection) = 
   if foldr (fn (l2, flag) => if equivPropLists (l1, l2) then true else flag)
       false propCollection
   then propCollection else l1::propCollection
*)  
  val isRun : prop -> bool =
  fn (b, n1, _, _, _) => not b andalso n1 = "DNR"

  fun choose (l1, l2) =
   if List.exists isRun l1 then l2 else l1

  fun add (l1, []) = [l1]
    | add (l1, p::ps) = if equivPropLists (l1, p)
                        then (choose (l1, p))::ps
			else p::(add (l1, ps))

  fun removeDuals propCollection =
   foldr add [] propCollection


end