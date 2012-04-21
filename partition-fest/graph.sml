structure BasicGraph : GRAPH = struct
  type node_label = string
  type edge_label = string
  type node = node_label
  datatype edge = EDGE of { source: node, label: edge_label, dest: node }
  datatype graph = GRAPH of { nodes : node list, edges : edge list }

  exception NotFound of node

  val empty : graph = GRAPH { nodes = [], edges = [] }

  fun makeNode x = x
  fun makeEdge (s, l, d) = EDGE { source = s, label = l, dest = d }

  fun memberNode (n, GRAPH {nodes, edges}) = 
      List.exists (fn x => x=n) nodes

  fun memberEdge (e, GRAPH {nodes, edges}) =
      List.exists (fn x => x=e) edges

  fun addNode (n, GRAPH { nodes, edges }) = 
    if memberNode (n, GRAPH { nodes = nodes, edges = edges })
    then GRAPH { nodes = nodes, edges = edges }
    else GRAPH { nodes = n::nodes, edges = edges }  
  fun addEdge (e, GRAPH { nodes, edges }) = 
    if memberEdge (e, GRAPH { nodes = nodes, edges = edges })
    then GRAPH { nodes = nodes, edges = edges }
    else GRAPH { nodes = nodes, edges = e::edges }

  fun getNodes (GRAPH { nodes, edges }) = nodes
  fun getEdges (GRAPH { nodes, edges }) = edges

  fun getNodeLabel n = n 
  fun getIn (EDGE { source, label, dest }) = source
  fun getOut (EDGE { source, label, dest }) = dest
  fun getEdgeLabel (EDGE { source, label, dest }) =  label



  fun getSuccessorEdges (n, GRAPH { nodes, edges }) =
    if memberNode (n, GRAPH { nodes = nodes, edges = edges })
    then let fun find_succ ((EDGE { source, label, dest })::xs) l = 
                   if source = n
                   then find_succ xs ((EDGE {source = source, label = label, 
                                      dest = dest })::l)
                   else find_succ xs l
               | find_succ [] l = l
         in find_succ edges []
         end
    else raise NotFound n

  fun getPredecessorEdges (n, GRAPH { nodes, edges }) =
    if memberNode (n, GRAPH { nodes = nodes, edges = edges })
    then let fun find_succ ((EDGE { source, label, dest })::xs) l = 
                   if dest = n
                   then find_succ xs ((EDGE {source = source, label = label, 
                                      dest = dest })::l)
                   else find_succ xs l
               | find_succ [] l = l
         in find_succ edges []
         end
    else raise NotFound n

  fun getSuccessorNodes (n, GRAPH { nodes, edges }) =
    if memberNode (n, GRAPH { nodes = nodes, edges = edges })
    then let fun find_succ ((EDGE { source, label, dest })::xs) l = 
                   if source = n
                   then find_succ xs (dest::l)
                   else find_succ xs l
               | find_succ [] l = l
         in find_succ edges []
         end
    else raise NotFound n

  fun getPredecessorNodes (n, GRAPH { nodes, edges }) =
    if memberNode (n, GRAPH { nodes = nodes, edges = edges })
    then let fun find_succ ((EDGE { source, label, dest })::xs) l = 
                   if dest = n
                   then find_succ xs (source::l)
                   else find_succ xs l
               | find_succ [] l = l
         in find_succ edges []
         end
    else []

  fun getNode (l, g) = if memberNode (makeNode l, g) 
                       then makeNode l
                       else raise NotFound (makeNode l)

fun addNodesFromEdge (EDGE {source, label, dest}, graph) =
  (addNode (source, (addNode (dest, graph))))

fun getNodesFromEdges (edgeList) = 
  getNodes (foldr addNodesFromEdge empty edgeList)

fun getGraphFromEdges (edgeList) = 
  GRAPH {nodes = getNodesFromEdges edgeList, edges = edgeList}

end