structure Basis : sig 
  val buildGraph : string -> string -> string list -> TestSet.set list 
  val buildPropGraph : string -> string -> string list -> Prop.prop list list

end =
struct

  (* helper function because Haskell syntax is useful *)
  infixr 0 $
  fun f $ x = f x


  (* Turn map into a TestSet *)
  fun makeTestSet db = 
    DB.foldStudents (fn (test, testno, rList, set) => 
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
                                               ((name, number), out),m))
                                 map ol
                               | NONE => raise Impossible


  val makeSolnMap :  
    TestSet.set list -> ((string * string) * Outcome.outcome) ListMap.map =
      fn set => foldr addToMap ListMap.empty set


  (* Make test vector from map, with DNRs represented in the vector *)
  val makeSolnSet : 
    ((string * string) * Outcome.outcome) ListMap.map -> SolnSet.set = 
       fn map => ListMap.mapFold
         (fn (k, testList, set) => SolnSet.add ((implode k, testList), set))
         SolnSet.empty map

  (* Partition SolnSet *)
  val partitionSolns : SolnSet.set -> SolnSet.set list = SolnSet.partition SolnSet.eq

  (* Produce graph using subset relations *)

  val /<=/ = SolnSet./<=/
  infix 3 /<=/

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
    let val string = SolnSet.fold (fn ((n, _), str) => n^"\\n"^ str) "" s
        val (_, l) = solnRep s
        val node = "N"^Int.toString(c)
    in (SolnSet.add((node, l), set), 
        Map.bind(explode node, string, map), c+1) end) 
    (SolnSet.empty, Map.empty, 1) sl
   in (partitionSolns s,m) end

  (* curried, simplified function to make an edge *)
  fun edge id1 label id2 = G.makeEdge (G.makeNode id1, label, G.makeNode id2)

  (* Make the implication graph *)
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



  val partitionProps = Prop.partition Prop.eq
 
  (* Transforms a map from label -> prop list into a map from label -> string *)
  fun condenseMap map = 
    Map.mapFold (fn (key, pList, m2) => Map.bind (key, Prop.toString pList, m2))
      Map.empty map

  (* Takes a map from strings to prop lists and returns a map from
     strings to prop lists with any tautological equivalences removed *)
  fun removeIntraNodeTautologies m = 
    Map.mapFold (fn (key, pList, m2) => 
                  Map.bind (key, Prop.removeIntraNodeTautologies pList, m2))
      Map.empty m

  fun removeTautologies (g, m) =
    let val edges = BasicGraph.getEdges g
        val edges2 = 
          foldr (fn (e, es) => 
            if Prop.tautology (Map.lookup (explode (BasicGraph.getIn e), m),
                               Map.lookup (explode (BasicGraph.getOut e), m))
            then es
            else e::es)
            [] edges
    in (BasicGraph.getGraphFromEdges edges2, m)
    end

  fun contraNodes (n1, n2) m =
    Prop.complementList (Map.lookup (explode n1, m),
                         Map.lookup (explode n2, m))
  fun contraEdges (e1, e2) m = 
    contraNodes (BasicGraph.getIn e1, BasicGraph.getOut e2) m andalso
    contraNodes (BasicGraph.getIn e2, BasicGraph.getOut e1) m

  fun chooseEdge (edge, edges) m = 
    let fun swap [] = []
          | swap (x::xs) = if contraEdges (edge, x) m
                           then edge::xs
                           else swap xs
        val swapped = swap edges
    in if (length (BasicGraph.getNodesFromEdges edges) <
           length (BasicGraph.getNodesFromEdges swapped))
       then edges else swapped
    end

  fun addingContraNode edges n m =
    let fun con e = contraNodes (n, BasicGraph.getIn e) m orelse
                    contraNodes (n, BasicGraph.getOut e) m
    in List.exists con edges
    end

  fun equiv e1 e2 m = e1 = e2 orelse contraEdges (e1, e2) m
  
  fun removeContrapositives (g, m) =
    let val edges  = BasicGraph.getEdges g
        fun posEdge e = 
          Prop.positive (Map.lookup (explode (BasicGraph.getIn e), m)) andalso
          Prop.positive (Map.lookup (explode (BasicGraph.getOut e), m)) 
        val positives = List.filter posEdge edges
        val negatives = List.filter (not o posEdge) edges
  in (BasicGraph.getGraphFromEdges (
        foldr (fn (e1, es) => if (List.exists 
                                   (fn e2 => contraEdges (e1, e2) m) es)
                              then chooseEdge (e1, es) m
                              else e1::es)
        positives negatives),
     m)
   
  end

  fun removeContra (g, m) =
    let fun getNeighbors n = BasicGraph.getPredecessorEdges (n, g) @
			     BasicGraph.getSuccessorEdges   (n, g)

        fun equiv e1 e2 = e1 = e2 orelse contraEdges (e1, e2) m

 	fun badEdge e es = addingContraNode es (BasicGraph.getIn e) m orelse
			   addingContraNode es (BasicGraph.getOut e) m 

        fun add (edge, (nodes, edges)) = 
          if List.exists (equiv edge) edges
          then (nodes, edges)
          else (BasicGraph.getIn edge :: BasicGraph.getOut edge :: nodes,
                edge::edges)

        val (n::_) = BasicGraph.getNodes g

        fun rC edges (n::ns) = 
             let val (ns2, es2) = foldr add (ns, edges) (getNeighbors n)
             in rC es2 ns2 end
          | rC edges [] = edges (* needs to be fixed *)
    in (BasicGraph.getGraphFromEdges (rC [] [n]), m)
    end
     

  
(*
  fun reducePartitions tests graph map =
    let val nodes = BasicGraph.getNodes g
        
*)
  
  fun getFalseReps pl =  Prop.getPropsWithResult pl (true, "FAILED")

  fun containsFalse pl = not $ null $ getFalseReps pl
                        

  fun propRep [] = raise Impossible
    | propRep (x::xs) = x

  val testRep = propRep


  exception NoImplyingFailures
  (* Produces the union of the failure sets of all implying proposition lists
     that have at last one failure in them, i.e. the union of all implying
     failure sets *)
  val getFalseUnion : BasicGraph.node * BasicGraph.graph * 
                      Prop.prop list Map.map -> Prop.prop = 
  (fn (n, g, m) =>
    let val implyingNodes = BasicGraph.getPredecessorNodes (n, g)
        fun look node = Map.lookup (explode $ BasicGraph.getNodeLabel node, m)
        val implyingProps = map look implyingNodes
        val falseImplyingProps = List.filter containsFalse implyingProps
        val reps = map propRep falseImplyingProps 
    in if null reps
       then raise NoImplyingFailures
       else Prop.unionstar reps
    end)

  (* Returns whether or not a given node's set of true values is equivalent to
     the union of all implying failure sets.
     PRECONDITION: n maps to a prop list that contains at least one failure *)
  fun failureRedundantUnderUnion (n, g, m) = 
    let val self = Map.lookup (explode $ BasicGraph.getNodeLabel n, m)
    in Prop.eq (propRep self, getFalseUnion (n, g, m))
       handle _ => false
    end
  

  (* Returns a proposition list where each proposition is the failure of a test
     found redundant under union using the graph g and map m *)
  fun findRedundantFailures (g, m) =
    let val nodes = BasicGraph.getNodes g
        fun addIfRedundant (n, xs) = 
          let val pl = Map.lookup (explode $ BasicGraph.getNodeLabel n, m)
          in if not $ containsFalse pl then xs
             else if failureRedundantUnderUnion (n,g,m)
                  then getFalseReps pl @ xs
                  else xs
          end
    in foldr addIfRedundant [] nodes
    end

  fun removeRedundantTests (tl, g, m) = 
    let val toBeRemoved = map Prop.getId (findRedundantFailures (g, m))
        fun redundant t = List.exists (fn n => n = TestSet.getId (testRep t)) toBeRemoved
    in foldr (fn (t, xs) => if redundant t then xs else t::xs) [] tl
    end





  fun getTestPartitions infile = partitionTests $ makeTestSet $
                                 FileReader.readToMap $ TextIO.openIn infile

  fun buildPropGraphAndMap tests = 
    let val p      = partitionProps $ Prop.makePropList tests 
        val (s, m) = Prop.makePropMapAndSet p
                  
    in (Prop.makePropGraph s, m, p)
    end

  fun testReduction tests = 
    let val (g, m, p) = buildPropGraphAndMap tests
    in removeRedundantTests (tests, g, m) 
    end

  exception InvalidFlag of string
  fun reduction [] = (fn x => x)
    | reduction (flag::xs) = 
        case flag of "-c" => reduction xs o removeContrapositives
                   | "-t" => removeTautologies o reduction xs
                   | x    => raise InvalidFlag x

  fun mapReduction [] = condenseMap
    | mapReduction (flag::xs) =
        case flag of "-t" => mapReduction xs o removeIntraNodeTautologies
                   | _    => mapReduction xs

  fun testSetReduction [] = (fn x => x)
    | testSetReduction (flag::xs) = 
        case flag of "-u" => testReduction o testSetReduction xs
                   | x    => raise InvalidFlag x 

  fun buildPropGraph infile outfile  flags =
    let val tests   = getTestPartitions infile
        val (g,  m, p) = buildPropGraphAndMap tests
        val (g', m') = reduction flags (g, m)
        val fixMap  = mapReduction flags
        val fd      = TextIO.openOut outfile
        val ()      = FileWriter.printGraph g' (fixMap m') fd false
        val ()      = TextIO.closeOut fd
    in  p
    end

  fun buildGraph infile outfile flags = 
    let val tests  = (testSetReduction flags) (getTestPartitions infile)
        val (s, m) = buildMapAndSet $ partitionSolns $ makeSolnSet $
                     makeSolnMap tests
        val g      = makeGraph s
        val ()     = FileWriter.printGraph g m (TextIO.openOut outfile) true
    in tests
    end

end
