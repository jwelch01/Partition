signature NODE = 
  sig
    type 'a node
    val makeNode : 'a -> 'a node
  end