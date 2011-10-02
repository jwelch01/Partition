signature GRAPH = 
  sig
    type node
    type node_label
    type edge
    type edge_label
    type graph

    exception NotFound of node_label

    val empty    : graph

    val makeNode : node_label -> node
    val makeEdge : node * edge_label * node -> edge

    val addNode  : node * graph -> graph
    val addEdge  : edge * graph -> graph
    
    val getNodes : graph -> node list
    val getEdges : graph -> edge list

    val getIn        : edge -> node
    val getOut       : edge -> node
    val getEdgeLabel : edge -> edge_label
    val getNodeLabel : node -> node_label    


    val getSuccessorNodes   : node * graph -> node list
    val getPredecessorNodes : node * graph -> node list
    val getSuccessorEdges   : node * graph -> edge list
    val getPredecessorEdges : node * graph -> edge list

    val memberNode : node * graph -> bool
    val memberEdge : edge * graph -> bool

    val getNode    : node_label * graph -> node

  end