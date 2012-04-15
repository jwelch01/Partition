structure FileWriter : sig
  val printGraph : 
    BasicGraph.graph -> string Map.map -> TextIO.outstream -> bool -> unit

  (* the bool represents whether the arrow should point backwards, true if it
     should, false if it should not *)

  val printSolnGraph :
    BasicGraph.graph -> string Map.map -> SolnSet.set list ->
    TextIO.outstream -> unit

end =
struct

  val output = TextIO.output

  fun printGraph graph map out back =
      (output (out, "digraph testgraph { fontsize=\"9\" \nsize=\"10.3,7.7\"; ratio=compress\nnode [fontsize=\"9\"] \nedge [fontsize=\"9\"]"); 
       foldl (fn (name, _) =>
               let val label = G.getNodeLabel name
               in ((output (out, label ^ " [label=\"" ^
                                Map.lookup(explode label, map) ^ "\"]\n")); [])
         end)
       [] (G.getNodes graph);
       foldl (fn (name, _) =>
               foldr (fn (name2, _) => 
                     if back then
                         ((output (out, G.getNodeLabel name ^ " -> " ^
                                        G.getNodeLabel name2 ^ 
                                        " [dir=back]\n")); [])
                             else 
                          ((output (out, G.getNodeLabel name ^ " -> " ^
                                        G.getNodeLabel name2 ^ 
                                        " \n")); []))
                  [] (G.getSuccessorNodes (name, graph)))
             [] (G.getNodes graph);
       (output (out, "}")))

  fun printSolnGraph graph map solns out =
    (output (out, "digraph testgraph { fontsize=\"9\" \nsize=\"10.3,7.7\"; ratio=compress\nnode [fontsize=\"9\"] \nedge [fontsize=\"9\"]"); 

       foldr (fn (soln, _) =>

       foldl (fn ((label, results),_) =>
         (output (out, label ^ " [label=\"" ^
                                Map.lookup(explode label, map) ^
          foldr (fn ((_, outcome), s) => if Outcome.eq (outcome, Outcome.PASSED)
                                         then "|" ^ s
                                         else "." ^ s) "" results
          ^ "\"]\n"); []))
       [] soln) [] solns;

       foldl (fn (name, _) =>
               foldr (fn (name2, _) => 
                         ((output (out, G.getNodeLabel name ^ " -> " ^
                                        G.getNodeLabel name2 ^ 
                                        " [dir=back]\n")); []))
                  [] (G.getSuccessorNodes (name, graph)))
             [] (G.getNodes graph);
       (output (out, "}")))
end