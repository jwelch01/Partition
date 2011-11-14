structure map = TernaryStringMap(TernaryKeyChar)
structure listMap = ListMapFn(map)
structure m = EmbedMap(structure L1 = map
                       structure L2 = listMap)
structure g = BasicGraph
structure tSet = TestSet(Outcome)
structure sSet = SolnSet(Outcome)

structure Basis : sig 
  val buildGraph : string -> unit (* read outcomes & write dot to file "out" *)
end =
struct

  (* Build a map from tests to student * outcome lists from input file *)
  fun readToMap fd =
    let fun build fd map =
      case TextIO.inputLine fd
        of NONE => map
         | SOME line => let val {num, outcome, solnid, testid} = 
              OutcomeReader.outcome line
                        in build fd 
             (m.add ((explode testid, explode (Int.toString num)), 
                                  (solnid, outcome), 
                map))
              end
    in build fd m.empty
    end


  (* Turn map into a TestSet *)
  fun makeTestSet map = m.mapFold 
    (fn ((k1,k2), resultList, set) => tSet.add ((implode k1, implode k2, 
                           resultList), set))
    tSet.empty map

  (* Partition TestSet *)
  val partitionTests = tSet.partition tSet.eq

  (* Make map from solns -> test * outcome list using a representative from each
  eq class *)
  exception Impossible
  fun addToMap (set, map) = case tSet.representative set
                              of SOME (name, number, ol) => foldr
                                 (fn ((soln, out), m) => 
                                   listMap.add (explode soln, 
                                               (name, number, out),m))
                                 map ol
                               | NONE => raise Impossible

  fun makeSolnMap set = foldr addToMap listMap.empty set

  (* Turn map into SolnSet *)
  fun makeSolnSet map = listMap.mapFold
    (fn (k, testList, set) => sSet.add ((implode k, testList), set))
    sSet.empty map

  (* Partition SolnSet *)
  val partitionSolns = sSet.partition sSet.eq

  (* Produce graph using subset relations *)

  val /<=/ = sSet./<=/
  val /==/ = sSet./==/
  infix 3 /<=/ /==/


  fun rep s = case sSet.representative s
                of SOME y => y
                 | NONE   => raise Impossible

  (* Make a new set list with renamed members, and a map to the students that
     the new names represent *)
  fun buildMapAndSet sl =
   let val (s, m, _) =
    foldr (fn (s, (set, map, c)) =>
    let val string = sSet.fold (fn ((n, _), str) => n^(" "^ str)) "" s
        val (_, l) = rep s
        val node = "N"^Int.toString(c)
    in (sSet.add((node, l), set), 
        map.bind(explode node, string, map), c+1) end) 
    (sSet.empty, map.empty, 1) sl
   in (sSet.partition sSet.eq s,m) end

  fun edge id1 label id2 = g.makeEdge (g.makeNode id1, label, g.makeNode id2)

  (* Make the graph structure *)
  fun makeGraph sl = 
    foldr (fn (x, graph) => 
     let val (id1, _) = rep x
     in foldr (fn (y, g) =>
      let val (id2, _) = rep y
      in if x /<=/ y andalso not (y /<=/ x) 
         then g.addEdge (edge id2 "" id1, g)
         else g
      end)
         (g.addNode(g.makeNode id1, graph)) sl
     end)
    g.empty sl

  val output = TextIO.output

  fun printGraph graph map out =
      (output (out, "digraph testgraph { fontsize=\"9\" \nsize=\"10.3,7.7\"; ratio=compress\nnode [fontsize=\"9\"] \nedge [fontsize=\"9\"]"); 
       foldl (fn (name, _) =>
               let val label = g.getNodeLabel name
               in ((output (out, label ^ " [label=\"" ^
                                map.lookup(explode label, map) ^ "\"]\n")); [])
         end)
       [] (g.getNodes graph);
       foldl (fn (name, _) =>
               foldr (fn (name2, _) => 
                         ((output (out, g.getNodeLabel name ^ " -> " ^
                                        g.getNodeLabel name2 ^ 
                                        " [dir=back]\n")); []))
                  [] (g.getSuccessorNodes (name, graph)))
             [] (g.getNodes graph);
       (output (out, "}")))

  infixr 0 $
  fun f $ x = f x

  fun buildGraph file = 
    let val (s, m) = buildMapAndSet $ partitionSolns $ makeSolnSet $
                     makeSolnMap $ partitionTests $ makeTestSet $
                     readToMap $ TextIO.openIn file
        val g = makeGraph s
    in printGraph g m (TextIO.openOut "out")
    end

end
