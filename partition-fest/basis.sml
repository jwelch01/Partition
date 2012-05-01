structure Basis : sig 
  val buildGraph : string -> string -> string -> string ->
                   string list -> SolnSet.set 
  val buildPropGraph : string -> string -> string list -> Prop.prop list list

  val makeChart : string -> string -> string

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
                                   ListMap.add ( soln, 
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
         (fn (k, testList, set) => SolnSet.add ((k, testList), set))
         SolnSet.empty map

  (* Partition SolnSet *)
  val partitionSolns : SolnSet.set -> SolnSet.set list = SolnSet.partition SolnSet.eq

  (* Produce graph using subset relations *)

  val /</ = SolnSet./</
  infix 3 /</

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
          Map.bind( node, string, map), c+1) 
      end) 
    (SolnSet.empty, Map.empty, 1) sl
   in (partitionSolns s,m) 
   end

  (* curried, simplified function to make an edge *)
  fun edge id1 label id2 = G.makeEdge (G.makeNode id1, label, G.makeNode id2)

  (* Make the implication graph *)
  fun id (solnid, _) = solnid
  val makeGraph : SolnSet.set list -> BasicGraph.graph =
  fn sl => 
    foldr (fn (x, graph) => 
           let val x = solnRep x
           in  foldr (fn (y, g) =>
                      let val y = solnRep y
                      in  if x /</ y andalso not (y /</ x) 
                          then G.addEdge (edge (id y) "" (id x), g)
                          else g
                      end)
               (G.addNode(G.makeNode (id x), graph)) sl
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
            if Prop.tautology (Map.lookup ( BasicGraph.getIn e, m),
                               Map.lookup ( BasicGraph.getOut e, m))
            then es
            else e::es)
            [] edges
    in (BasicGraph.getGraphFromEdges edges2, m)
    end

  fun contraNodes (n1, n2) m =
    Prop.complementList (Map.lookup ( n1, m),
                         Map.lookup ( n2, m))
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
          Prop.positive (Map.lookup (BasicGraph.getIn e, m)) andalso
          Prop.positive (Map.lookup (BasicGraph.getOut e, m)) 
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


        fun rC edges (n::ns) = 
             let val (ns2, es2) = foldr add (ns, edges) (getNeighbors n)
             in rC es2 ns2 end
          | rC edges [] = edges (* needs to be fixed *)
    in  case BasicGraph.getNodes g
          of n :: _ => (BasicGraph.getGraphFromEdges (rC [] [n]), m)
           | [] => let exception EmptyGraph in raise EmptyGraph end
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

  fun look m node = Map.lookup (BasicGraph.getNodeLabel node, m)

  fun getImplyingFailReps (n, g, m) =
    let val implyingNodes = BasicGraph.getPredecessorNodes (n, g)
        val implyingProps = map (look m) implyingNodes
        val falseImplyingProps = List.filter containsFalse implyingProps
    in map propRep falseImplyingProps
    end

  fun getImplyingFailures (n, g, m) = 
    let val implyingNodes = BasicGraph.getPredecessorNodes (n, g)
        val implyingProps = map (look m) implyingNodes
    in Util.flatten $ map getFalseReps implyingProps
    end

  fun makeFailureMap (g, m) = 
    let val nodes = BasicGraph.getNodes g
        val aList = map (fn n => (look m n, n)) nodes
        val failList = map (fn (props, n) => (getFalseReps props, n)) aList
    in foldr (fn ((tests, node), map) =>
               foldr (fn (test, map) =>
                 let val (test, num) = Prop.getId test
                     val id = test ^ num
                 in Map.bind (id, node, map) end) map tests) 
       Map.empty failList
    end


(*
  fun reduceWitness failMap nodeMap g (solnid, ol) = 
    let val failures = List.filter (fn ((_,_), Outcome.NOTPASSED {...}) => true
                                     | _                            => false)
                       ol
        fun redundantNode ((test,num), out as Outcome.NOTPASSED {...}) = 
            let val id = test ^ num
              val node  = Map.lookup (id, failMap)
              val impls = map Prop.getId $ getImplyingFailures (node,g,nodeMap)
            in not $
              List.exists (fn id1 => List.exists (fn (id2, _) => id1=id2)
                                         failures) impls
            end
          | redundantNode _ = false
     in (solnid, List.filter (not o redundantNode) failures)
     end
*)

  val causes = TextIO.openOut "causes"

  fun reduceWitness failMap nodeMap g (solnid, ol) = 
    (TextIO.output (causes, solnid^": \n");
    let val failures = List.filter (fn ((_,_), Outcome.NOTPASSED {...}) => true
                                     | _                            => false)
                       ol
        fun redundantNode ((test,num), 
                           Outcome.NOTPASSED {witness = witness, ...}) = 
            let val id = test ^ num
              val node  = Map.lookup (id, failMap)
                          handle NotFound => BasicGraph.makeNode "FAKENODE"
              val impls = map Prop.getId $ getImplyingFailures (node,g,nodeMap)
              val cause = List.find 
                            (fn (id2, _) => List.exists (fn id1 => id1 = id2)
                                            impls) failures
            in case cause 
                of NONE => false
                 | SOME ((t,n), Outcome.NOTPASSED {witness = witness2, ...}) => 
                (let val id2 = t^n
               (*  in TextIO.output (causes,
                                   "Test "^id^" made redundant by "^id2(*^
                                   "\n"^id2^"'s witness:\n"^witness2^
                                   "\n"^id^"'s witness:\n"^witness*)^"\n\n")*)
                 in TextIO.output (causes,
                                   "Test "^id^" made redundant by "^id2^"\n")
		end
                                         ; true)
                 | _ => raise Impossible
            end
          | redundantNode _ = false
     in (solnid, List.filter (not o redundantNode) failures)
     end)

  fun reduceWitnesses (g,m) solns = 
    let val failMap = makeFailureMap (g, m)
    in map (reduceWitness failMap m g) solns
    end


  exception NoImplyingFailures
  (* Produces the union of the failure sets of all implying proposition lists
     that have at last one failure in them, i.e. the union of all implying
     failure sets *)
  val getFalseUnion : BasicGraph.node * BasicGraph.graph * 
                      Prop.prop list Map.map -> Prop.prop = 
  (fn (n, g, m) =>
    let val reps = getImplyingFailReps (n, g, m)
    in if null reps
       then raise NoImplyingFailures
       else Prop.unionstar reps
    end)

  fun failureRedundantUnderClaessen (n, g, m) =
    let val self = Map.lookup (BasicGraph.getNodeLabel n, m)
        val impls = getImplyingFailures (n, g, m)
    in List.exists (fn n1 => List.exists
                               (fn n2 => Prop.eq (propRep self, 
                                                  Prop.union (n1, n2)))
                             impls) impls
    end

  (* Returns whether or not a given node's set of true values is equivalent to
     the union of all implying failure sets.
     PRECONDITION: n maps to a prop list that contains at least one failure *)
  fun failureRedundantUnderUnion (n, g, m) = 
    let val self = Map.lookup (BasicGraph.getNodeLabel n, m)
    in Prop.eq (propRep self, getFalseUnion (n, g, m))
       handle _ => false
    end
  

  (* Returns a proposition list where each proposition is the failure of a test
     found redundant under union using the graph g and map m *)
  fun findRedundantFailures (g, m) redundantF =
    let val nodes = BasicGraph.getNodes g
        fun addIfRedundant (n, xs) = 
          let val pl = Map.lookup (BasicGraph.getNodeLabel n, m)
          in if not $ containsFalse pl then xs
             else if redundantF (n,g,m)
                  then getFalseReps pl @ xs
                  else xs
          end
    in foldr addIfRedundant [] nodes
    end

  (* change this to filter *)

  fun removeRedundantTests (tl, g, m) redundantF = 
    let val toBeRemoved = map Prop.getId (findRedundantFailures (g, m) 
                                                                 redundantF)
        fun redundant t = List.exists (fn n => n = TestSet.getId (testRep t)) 
                                       toBeRemoved
    in List.filter (not o redundant) tl
    end

  fun getTestPartitions infile = partitionTests $ makeTestSet $
                                 FileReader.readToMap $ TextIO.openIn infile

  fun buildPropGraphAndMap tests = 
    let val p      = partitionProps $ Prop.makePropList tests 
        val (s, m) = Prop.makePropMapAndSet p
                  
    in (Prop.makePropGraph s, m, p)
    end

  fun unionReduction method (g, m, p) tests = 
    removeRedundantTests (tests, g, m) method


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

  fun testSetReduction [] _ = (fn x => x)
    | testSetReduction (flag::xs) (g, m, p) = 
        case flag of "-u" => (unionReduction failureRedundantUnderUnion 
                                             (g,m,p))
                              o testSetReduction xs (g,m,p)
                   | "-c" => (unionReduction failureRedundantUnderClaessen
                                             (g,m,p)) 
                              o testSetReduction xs (g,m,p)
                   | _    => testSetReduction xs (g, m, p)

  fun solnReduction [] _ = (fn x => x)
    | solnReduction (flag::xs) (g, m) = 
        case flag of "-w" => (reduceWitnesses (g,m)) o solnReduction xs (g, m)
                   | _    => solnReduction xs (g, m)


  fun anonymize solns = 
    let fun stripResults (soln, _) = soln
        val names = map stripResults solns
        val anon = AnonTest.anonymize (names, "jnw3")
    in map (fn (soln, results) => (anon soln, results)) solns
    end

  fun reduceTests tests = 
    let fun getId (x, _) = x

        val solns = map getId $ map solnRep $ partitionSolns $ 
                    anonymize $ makeSolnSet $ makeSolnMap tests

        val tests = map testRep tests

        val limit = List.filter (not o (fn (x,out) => 
                                         List.exists (fn y => x = y) solns))
                    
    in map (fn (x,y,ol) => (x,y,limit ol)) tests
    end 

(*TEMPORARY*)
   val containsNode = BasicGraph.edgeHasNode

  fun getQSort g = 
    let fun rm edge 45 = containsNode edge "N45"
          | rm edge 181 = containsNode edge "N181"
          | rm edge n  = containsNode edge ("N" ^ Int.toString n) orelse  
                         rm edge (n-1)
        val edges = BasicGraph.getEdges g
        val edges' =  List.filter (fn e => rm e 186 orelse
                                           rm e 52) edges
    in BasicGraph.getGraphFromEdges edges'
    end       

  fun buildPropGraph infile outfile  flags =
    let val tests   = getTestPartitions infile
        val (g,  m, p) = buildPropGraphAndMap $ map testRep tests
        val (g', m') = reduction flags (g, m)
        val g''     = getQSort g'
        val fixMap  = mapReduction flags
        val fd      = TextIO.openOut outfile
        val ()      = FileWriter.printGraph g' (fixMap m') fd false
        val ()      = TextIO.closeOut fd
    in  p
    end



  fun buildGraph infile outfile outfileTests outfileFailures flags = 
    let val tests     = getTestPartitions infile
(*        val tmptests      = makeTestSet $
                           FileReader.readToMap $ TextIO.openIn infile
        fun expand [] = []
          | expand (x::xs) = [x]::expand xs
	val (g',m',p) = buildPropGraphAndMap $ reduceTests tests *)
	val (g',m',p) = buildPropGraphAndMap $ map testRep tests 
	val tests'    = testSetReduction flags (g', m', p) tests
(*        val solns     = anonymize $ makeSolnSet $ makeSolnMap $ expand tmptests *)
        val solns     = anonymize $ makeSolnSet $ makeSolnMap tests'
        val (s, m)    = buildMapAndSet $ partitionSolns solns
        val g         = makeGraph s
        val (fd, tfd, ffd) = (TextIO.openOut outfile, 
                              TextIO.openOut outfileTests,
                              TextIO.openOut outfileFailures)
        val ()        = FileWriter.printSolnGraph g m s fd 
        val ()        = TextIO.output (tfd, foldr (fn (test, s) => 
                                                TestSet.toString test^"\n"^s)
                                            "" tests')
        val solns     = solnReduction flags (g' , m') solns 
        val ()        = FileWriter.printStudentFailures solns ffd
        val _         = (TextIO.closeOut fd; TextIO.closeOut tfd;
                         TextIO.closeOut ffd)
    in solns
    end

  fun tableify [] = ""
    | tableify (x::xs) = 
       let fun tab [] = " \\\\\n"
             | tab (x::xs) = " & " ^ x ^ tab xs
       in x ^ tab xs
       end

  val slength = Int.toString o length

  fun makeChart infile outfile =
    let val tests      = makeTestSet $
                           FileReader.readToMap $ TextIO.openIn infile
        val partitions = partitionTests tests
        fun expand []  = []
          | expand (x::xs) = [x]::expand xs
       
	val solns      = makeSolnSet $ makeSolnMap $ expand tests
	val solns'     = makeSolnSet $ makeSolnMap partitions

	fun fail (_, x)= Outcome.eq (x, Outcome.NOTPASSED {witness = "", outcome = ""})
fun truncToDec x y =        
   let val base = trunc (x/y)
   in real base * y end;  

        fun avgFailures solns = 
          let val outs = map (fn (_, t) => t) solns
              val fails = map (List.filter fail) outs
              val failCounts = map length fails
          in (real $ foldr op + 0 failCounts) / (real $ length failCounts)
	  end
        val (g,m,p)    = buildPropGraphAndMap $ map testRep partitions
	val claessen   = unionReduction failureRedundantUnderClaessen
                                        (g, m, p) partitions
        val union      = unionReduction failureRedundantUnderUnion
                                        (g, m, p) partitions 
        val out        = TextIO.openAppend outfile
        val tableLine  = tableify [infile, 
                                   Real.toString $ truncToDec (avgFailures solns) 0.01,
				   Real.toString $ truncToDec (avgFailures solns') 0.01]
        val ()         = TextIO.output (out, tableLine)
        val ()         = TextIO.closeOut out
    in tableLine
    end
                                                      

end
