structure Basis : sig 
  val buildGraph : string -> string -> unit 
  val buildPropGraph : string -> string -> string list -> (BasicGraph.graph * BasicGraph.graph)

end =
struct

  (* Turn map into a TestSet *)
fun makeTestSet db = 
  DB.foldLists (fn (test, testno, rList, set) => 
                TestSet.add ((test, testno, rList), set))
               TestSet.empty db

  (* Partition TesTestSet *)
  val partitionTests : TestSet.set -> TestSet.set list = TestSet.partition TestSet.eqMult

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
  val partitionSolns : SolnSet.set -> SolnSet.set list = SolnSet.partition SolnSet.eqMult

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
    let val string = SolnSet.fold (fn ((n, _), str) => n^"\\n"^ str) "" s
        val (_, l) = solnRep s
        val node = "N"^Int.toString(c)
    in (SolnSet.add((node, l), set), 
        Map.bind(explode node, string, map), c+1) end) 
    (SolnSet.empty, Map.empty, 1) sl
   in (partitionSolns s,m) end

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

  infixr 0 $
  fun f $ x = f x

  val partitionProps = Prop.partition Prop.eq
 
  fun condenseMap map = 
    Map.mapFold (fn (key, pList, m2) => Map.bind (key, Prop.toString pList, m2))
      Map.empty map

  fun removeIntraNodeTautologies m = 
    Map.mapFold (fn (key, pList, m2) => 
                  Map.bind (key, Prop.removeIntraNodeTautologies pList, m2))
      Map.empty m

  fun fixMap m = condenseMap (removeIntraNodeTautologies m) 

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
  fun buildPropGraph infile outfile =
    let val (s, m) = Prop.makePropMapAndSet $ 
                     partitionProps $ Prop.makePropList $
                     partitionTests $ makeTestSet $
                     FileReader.readToMap $ TextIO.openIn infile
        val g      = removeTautologies (Prop.makePropGraph s) m
        val fd     = TextIO.openOut outfile
        val ()     = FileWriter.printGraph g (fixMap m) fd false
        val ()     = TextIO.closeOut fd
    in  g
    end
  *)

  fun buildPropGraphAndMap infile = 
    let val (s, m) = Prop.makePropMapAndSet $ 
                     partitionProps $ Prop.makePropList $
                     partitionTests $ makeTestSet $
                     FileReader.readToMap $ TextIO.openIn infile
    in (Prop.makePropGraph s, m)
    end

  fun reduction [] = (fn x => x)
    | reduction (flag::xs) = 
        case flag of "-c" => removeContrapositives o reduction xs
                   | "-t" => removeTautologies o reduction xs
  fun buildPropGraph infile outfile  flags =
    let val (g,  m) = buildPropGraphAndMap infile
        val (g', _) = reduction flags (g, m)
        val fd     = TextIO.openOut outfile
        val ()     = FileWriter.printGraph g' (fixMap m) fd false
        val ()     = TextIO.closeOut fd
    in  (g, g')
    end

  fun buildGraph infile outfile = 
    let val (s, m) = buildMapAndSet $ partitionSolns $ makeSolnSet $
                     makeSolnMap $ partitionTests $ makeTestSet $
                     FileReader.readToMap $ TextIO.openIn infile
        val g = makeGraph s
    in FileWriter.printGraph g m (TextIO.openOut outfile) true
    end

end
