structure Basis : sig 
  val buildGraph : string -> string -> unit 
   (* read outcomes & write dot to file "out" *)
end =
struct

  (* Turn map into a TestSet *)
fun makeTestSet db = 
  DB.foldLists (fn (test, testno, rList, set) => 
                TestSet.add ((test, testno, rList), set))
               TestSet.empty db

  (* Partition TesTestSet *)
  val partitionTests : TestSet.set -> TestSet.set list = TestSet.partition TestSet.eq

  (* Make map from solns -> test * outcome list using a representative from each
  eq class *)
  exception Impossible
  fun addToMap (set, map) = case TestSet.representative set
                              of SOME (name, number, ol) => foldr
                                 (fn ((soln, out), m) => 
                                   ListMap.add (explode soln, 
                                               (name, number, out),m))
                                 map ol
                               | NONE => raise Impossible


  val makeSolnMap :  
    TestSet.set list -> (string * string * Outcome.outcome) ListMap.map =
      fn set => foldr addToMap ListMap.empty set

  (* Turn map into SolnSet *)
  val makeSolnSet : 
    (string * string * Outcome.outcome) ListMap.map -> SolnSet.set = 
       fn map => ListMap.mapFold
         (fn (k, testList, set) => SolnSet.add ((implode k, testList), set))
         SolnSet.empty map

  (* Partition SolnSet *)
  val partitionSolns : SolnSet.set -> SolnSet.set list = SolnSet.partition SolnSet.eq

  (* Produce graph using subset relations *)

  val /<=/ = SolnSet./<=/
  val /==/ = SolnSet./==/
  infix 3 /<=/ /==/


  fun solnRep s = case SolnSet.representative s
                    of SOME y => y
                     | NONE   => raise Impossible

  fun testRep s = case TestSet.representative s
                    of SOME y => y
                     | NONE   => raise Impossible

  (* Make a new set list with renamed members, and a map to the students that
     the new names represent *)
  val buildMapAndSet : SolnSet.set list -> SolnSet.set list * string Map.map =
  fn sl =>
   let val (s, m, _) =
    foldr (fn (s, (set, map, c)) =>
    let val string = SolnSet.fold (fn ((n, _), str) => n^(" "^ str)) "" s
        val (_, l) = solnRep s
        val node = "N"^Int.toString(c)
    in (SolnSet.add((node, l), set), 
        Map.bind(explode node, string, map), c+1) end) 
    (SolnSet.empty, Map.empty, 1) sl
   in (SolnSet.partition SolnSet.eq s,m) end

  fun edge id1 label id2 = G.makeEdge (G.makeNode id1, label, G.makeNode id2)

  (* Make the graph structure *)
  val makeGraph : SolnSet.set list -> BasicGraph.graph =
  fn sl => 
    foldr (fn (x, graph) => 
     let val (id1, _) = solnRep x
     in foldr (fn (y, g) =>
      let val (id2, _) = solnRep y
      in if x /<=/ y andalso not (y /<=/ x) 
         then G.addEdge (edge id2 "" id1, g)
         else g
      end)
         (G.addNode(G.makeNode id1, graph)) sl
     end)
    G.empty sl

  type proposition = (bool * string * string * string * (string * bool) list)

  exception AlreadyNegative

 fun insertion_sort _ [] = []
 | insertion_sort cmp (x::xs) = insert cmp x (insertion_sort cmp xs)
and insert _ x [] = [x]
 | insert cmp x (l as y::ys) =
      case cmp (x, y) of GREATER => y :: insert cmp x ys
                       | _       => x :: l

  fun cmpPropName ((name, _), (name2,_)) = String.compare (name, name2)

  val propSort : (string * bool) list -> (string * bool) list  = 
      insertion_sort cmpPropName

  fun propExists prop = foldr (fn ((_,out), flag) => out orelse flag) false prop

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

  fun makePropositionList testSetList = 
    foldr (fn (prop, props) =>
            (prop::(negateProposition prop)::props))
    [] (makePositivePropositionList testSetList)

  fun makePropMapAndSet propList =
   let val (s, m, _) =
    foldr (fn ((b, test, num, out, props), (s, m, c)) =>
      let val name = if b then test ^ " " ^ num ^ " " ^ out
                          else test ^ " " ^ num ^ " not " ^ out
          val node = "N" ^ Int.toString(c)
      in (((name,props)::s),
           Map.bind (explode node, name, m),
           c+1)
      end)
    ([], Map.empty, 1) propList
   in (s, m) end

  val /->/ : (string * bool) list * (string * bool) list -> bool = 
   fn (prop1, prop2) =>
    if (propExists prop1) then
     ListPair.foldr (fn ((_, out1), (_, out2), flag) =>
                     if out1 then out2 andalso flag
                             else flag)
     true (propSort prop1, propSort prop2)
    else false
 
  infixr 0 /->/

  fun filt (node, (soln, p)::props) = 
       if p then not (foldr (fn ((_, out), flag) => out andalso flag) 
                      true props)
            else not (foldr (fn ((_, out), flag) => (not out) andalso flag)
                      true props)
    | filt (_, []) = raise Impossible
    
  val filterProps : (string * (string * bool) list) list ->
                    (string * (string * bool) list) list
  = List.filter filt


  fun makePropositionGraph propList = 
    foldr (fn ((node, props), impls) =>
      let val impls_ = G.addNode (node, impls)
      in foldr (fn ((node2, props2), impls2) =>
                if props /->/ props2 
                then G.addEdge (edge node "" node2, impls2) 
                else impls2)
         impls_ propList
      end)
    G.empty propList
        


  infixr 0 $
  fun f $ x = f x

  fun buildGraph infile outfile = 
    let val (s, m) = buildMapAndSet $ partitionSolns $ makeSolnSet $
                     makeSolnMap $ partitionTests $ makeTestSet $
                     FileReader.readToMap $ TextIO.openIn infile
        val g = makeGraph s
    in FileWriter.printGraph g m (TextIO.openOut outfile)
    end

end
