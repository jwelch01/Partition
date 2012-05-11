structure Prop :> PROPOSITION  = 
struct
  type testset = TestCollection.collection
  type result = (string * bool) list
  datatype prop = PROP of { flag : bool, test : string, number : string,
			      outcome : Outcome.outcome, results : result }


  exception AlreadyNegative
  exception Impossible
  exception NotImplemented

  fun getId (PROP { test = test, number = num, ...}) = (test, num)

  fun outcomes (PROP {results =  ol, ...}) = ol

  fun cmpPropName ((name, _), (name2,_)) = String.compare (name, name2)

  val propSort : result -> result = 
      Util.insertion_sort cmpPropName

  fun propExists (PROP { results = prop, ... }) = 
    foldr (fn ((_,out), flag) => out orelse flag) false prop
 
  fun testRep s = case TestCollection.representative s
                    of SOME y => y
                     | NONE   => raise Impossible

  fun negateProposition (PROP { flag = flag, test = test, number = num,
                                outcome = out, results = l }) =
    if not flag then raise AlreadyNegative else
        let fun neg ((soln, result)::xs) negs = neg xs ((soln, not result)::negs)
              | neg [] negs = negs
        in (PROP{flag = false, test=test, number=num, outcome=out, results=neg l []})
        end

(* returns a proposition with arbitrary values for all fields except results,
   which are filled with the union of the results of each solution *)
  fun union (p1, p2) = 
    let val (ol1, ol2) = ((propSort o outcomes) p1, (propSort o outcomes) p2)
        val ol         =  ListPair.foldr (fn ((n1, o1), (n2, o2), ol) => 
                                           (n1, o1 orelse o2)::ol) [] (ol1,ol2)
    in PROP {flag = true, test = "tmp", number = "1", 
             outcome = Outcome.DNR, results = ol}
    end 

  fun getPropsWithResult [] _ = []
    | getPropsWithResult ((prop as PROP { flag = flag, outcome = out, ... })::xs)
                         (flag2, out2) =
         if flag = flag2 andalso Outcome.eq(out, out2)
         then prop::(getPropsWithResult xs (flag2, out2))
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
    in (PROP {flag = true, test = test, number = num,
              outcome = Outcome.PASSED, results = f l Outcome.PASSED []})::
       (PROP {flag = true, test = test, number = num,
              outcome = (Outcome.NOTPASSED { outcome = "", witness = "" }),
              results = f l (Outcome.NOTPASSED { outcome = "", witness = "" })
                        []})::
       (PROP {flag = true, test = test, number = num,
              outcome = Outcome.DNR, results = f l Outcome.DNR []})::[]
    end

  fun makePositivePropositionList testSetList = 
    foldr (fn (testList, props) => 
            (boolTests testList) @ props)
    [] testSetList

(* HELPER FUNCTIONS
   Removes from the proposition list any test that has a universal result
   or a result achieved by exactly one eq class. Universality improves
   readability of implication graph. Removing propositions with exactly one
   result solves "Know one thing know all things" problem with witness
   reduction *)
  fun trueResults (PROP { results = rs, ...}) = 
    List.filter (fn (_,bool) => bool) rs
  fun falseResults (PROP { results = rs, ...}) =
    List.filter (fn (_,bool) => not bool) rs
  fun goodTest t = 
    length (trueResults t) > 1 andalso length (falseResults t) > 1 

  fun makePropList testSetList = List.filter goodTest ( 
    foldr (fn (prop, props) =>
            (prop::(negateProposition prop)::props))
    [] (makePositivePropositionList testSetList))

  fun makePropMapAndSet propList =
   let val (s, m, _) =
    foldr (fn (props as (PROP { results = result, ...})::_, (s, m, c)) =>
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

  fun eq (PROP {results = prop1, ...}, PROP {results = prop2,...}) =
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
                if (node, props) /->/ (node, props2) andalso not (node = node2) 
                then G.addEdge (edge node "" node2, impls2) 
                else impls2)
         impls_ propList
      end)
    G.empty propList

  val cmp : prop * prop -> order =
  fn (PROP{flag=b1, test=n1, number=num1, outcome=out1, ...}, 
      PROP{flag=b2, test=n2, number=num2, outcome=out2, ...}) => 
    if n1 < n2 then LESS
     else if n1 = n2 then if num1 < num2 then LESS
		          else if num1 = num2 then Outcome.compare(out1,out2)
                               else GREATER
          else GREATER

  val equiv : prop * prop * bool -> bool =
  fn (PROP{flag=b1, test=n1, number=num1, outcome=out1, ...}, 
      PROP{flag=b2, test=n2, number=num2, outcome=out2, ...},flag) => 
    n1 = n2 andalso num1 = num2 andalso Outcome.eq(out1,out2) andalso flag

  val equivPropLists : prop list * prop list -> bool =
  fn (l1, l2) => 
   ListPair.foldrEq equiv true (Util.insertion_sort cmp l1, Util.insertion_sort cmp l2)
   handle UnequalLengths => false


  
(* PLACEHOLDERS FOR TAUTOLOGY REMOVAL *)
  exception NotImplemented
  fun tautology _ = raise NotImplemented
  fun removeIntraNodeTautologies _ = raise NotImplemented


  fun toString [] = ""
    | toString (PROP{flag=b, test=test, number=num, outcome=out, results=props}::xs) =
       let val name = if b then "\\n" ^ test ^ " " ^ num ^ " " ^ Outcome.toString out
                           else "\\n" ^test ^ " " ^ num ^ " not " ^ Outcome.toString out
       in name ^ toString xs
       end

end


(*
  TAUTOLOGY REMOVAL CODE: NOT UPDATED FOR NEW REPRESENTATION

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
*)