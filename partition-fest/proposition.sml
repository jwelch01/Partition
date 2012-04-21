structure Prop :> PROPOSITION  = 
struct
  type testset = TestSet.set
  type result = (string * bool) list
  (*datatype prop = PROP of { flag : bool, test : string, number : string,
			      outcome : outcome, results : result }
*)
  type prop = bool * string * string * string * result
  type stringProp = string * result

  exception AlreadyNegative
  exception Impossible
  exception NotImplemented

  fun getId (_, test, num, _, _) = (test, num)

  fun tempProp ol = (true, "", "", "", ol)

  fun outcomes (_, _, _, _, ol) = ol

  fun cmpPropName ((name, _), (name2,_)) = String.compare (name, name2)

  val propSort : result -> result = 
      Util.insertion_sort cmpPropName

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

  fun union (p1, p2) = 
    let val (ol1, ol2) = ((propSort o outcomes) p1, (propSort o outcomes) p2)
        val ol         =  ListPair.foldr (fn ((n1, o1), (n2, o2), ol) => 
                                           (n1, o1 orelse o2)::ol) [] (ol1,ol2)
    in tempProp ol
    end 

  fun getPropsWithResult [] _ = []
    | getPropsWithResult ((flag, name, num, out, pl)::xs) (flag2, out2) = 
         if flag = flag2 andalso out = out2
         then (flag, name, num, out, pl)::(getPropsWithResult xs (flag2, out2))
         else (getPropsWithResult xs (flag2, out2))

  exception UnionOfNilList
  fun unionstar []      = raise UnionOfNilList
    | unionstar (p::ps) = foldr union p ps

  fun boolTests (test, num, l) = 
    let fun f ((soln, out)::xs) outGoal bools= 
            if Outcome.eq (out, outGoal) 
            then f xs outGoal ((soln,true)::bools)
            else f xs outGoal ((soln,false)::bools)
          | f [] _ bools = bools
    in (true, test, num, "PASSED", f l Outcome.PASSED [])::
       (true, test, num, "FAILED", 
            f l (Outcome.NOTPASSED {outcome = "", witness = ""}) [])::
       (true, test, num, "DNR", f l Outcome.DNR [])::[]
    end

  fun trueResults (_,_,_,_, xs) = 
    List.filter (fn (_, bool) => bool) xs
 
  fun falseResults (_,_,_,_, xs) = 
    List.filter (fn (_, bool) => bool) xs

  fun goodTest t = 
    length (trueResults t) > 1 andalso length (falseResults t) > 1

  fun makePositivePropositionList testSetList = 
    foldr (fn (test, props) => 
            (boolTests test) @ props)
    [] testSetList

  fun makePropList testSetList = ( 
   List.filter goodTest (
    foldr (fn (prop, props) =>
            (prop::(negateProposition prop)::props))
    [] (makePositivePropositionList testSetList)))

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
    foldr (fn (props as (_, _, _, _, result)::_, (s, m, c)) =>
                  let val node = "N" ^ Int.toString(c)
                  in ( (node, result)::s
                     , Map.bind (node, props, m)
                     , c+1
                     )
                  end
              | ([], _) => let exception EmptyProps in raise EmptyProps end
           )
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

  fun complement ((b, name, num, test, _),
                  (b2, name2, num2, test2, _)) =
    b = not b2 andalso name = name2 andalso num = num2 andalso test = test2

  fun complementList (p1, p2) =
    foldr (fn (prop, flag) => flag andalso
             List.exists (fn prop2 => complement (prop, prop2)) p2)
    true p1

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
                if (node, props) /->/ (node, props2) andalso not (node = node2) 
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
   ListPair.foldrEq equiv true (Util.insertion_sort cmp l1, Util.insertion_sort cmp l2)
   handle UnequalLengths => false


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

  val tautCheck : prop * prop list -> bool =
  fn ((bool, name, num, result, _), pList2) =>
   if result = "PASSED" andalso bool
   then List.exists (fn (bool2, name2, num2, result2, _) =>
     name = name2 andalso num = num2 andalso
     ((result2 = "FAILED" andalso not bool2) orelse
      (result2 = "DNR" andalso not bool2))) pList2
   else if result = "FAILED" andalso bool
        then List.exists (fn (bool2, name2, num2, result2, _) =>
          name = name2 andalso num = num2 andalso
           ((result2 = "PASSED" andalso not bool2) orelse
            (result2 = "DNR" andalso not bool2))) pList2
        else if result = "DNR" andalso bool
             then List.exists (fn (bool2, name2, num2, result2, _) =>
               name = name2 andalso num = num2 andalso
                 ((result2 = "PASSED" andalso not bool2) orelse
                  (result2 = "FAILED" andalso not bool2))) pList2
             else false

  fun tautology (pList1, pList2) = 
    foldr (fn (prop, flag) => tautCheck (prop, pList2) orelse flag)
    false pList1

  fun chooseProp ((bool, name, num, result, l),
                  (bool2, name2, num2, result2, l2)) =
      if bool then (bool, name, num, result, l)
              else (bool2, name2, num2, result2, l2) 

  fun sameTest ((bool, name, num, result, l),
                  (bool2, name2, num2, result2, l2)) =
    name = name2 andalso num = num2

  fun addProp (l1, []) = [l1]
    | addProp (l1, x::xs) = if sameTest (l1, x)
                            then chooseProp (l1, x)::xs
                            else x::(addProp (l1, xs))
 
  fun removeIntraNodeTautologies pList = 
    foldr addProp [] pList

  fun positive [] = true
    | positive ((bool, _,_,_,_)::ps) = if bool then positive ps else false

  fun toString [] = ""
    | toString ((b, test, num, out, props)::xs) =
       let val name = if b then "\\n" ^ test ^ " " ^ num ^ " " ^ out
                           else "\\n" ^test ^ " " ^ num ^ " not " ^ out
       in name ^ toString xs
       end

end
