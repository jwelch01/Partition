structure FileWriter : sig
  val printGraph : 
    BasicGraph.graph -> string Map.map -> TextIO.outstream -> unit
end =
struct

  val output = TextIO.output

  fun printGraph graph map out =
      (output (out, "digraph testgraph { fontsize=\"9\" \nsize=\"10.3,7.7\"; ratio=compress\nnode [fontsize=\"9\"] \nedge [fontsize=\"9\"]"); 
       foldl (fn (name, _) =>
               let val label = G.getNodeLabel name
               in ((output (out, label ^ " [label=\"" ^
                                Map.lookup(explode label, map) ^ "\"]\n")); [])
         end)
       [] (G.getNodes graph);
       foldl (fn (name, _) =>
               foldr (fn (name2, _) => 
                         ((output (out, G.getNodeLabel name ^ " -> " ^
                                        G.getNodeLabel name2 ^ 
                                        " [dir=back]\n")); []))
                  [] (G.getSuccessorNodes (name, graph)))
             [] (G.getNodes graph);
       (output (out, "}")))

end