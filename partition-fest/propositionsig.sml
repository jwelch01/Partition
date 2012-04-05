signature PROPOSITION = sig
  type testset
  type prop
  type stringProp (* stringProps are utilized for ease of interaction with
                     the map interface and .dot file format *)

  val /->/ : stringProp * stringProp -> bool
  val partition : (prop * prop -> bool) -> prop list -> prop list list
  val makePropList : testset list -> prop list
  val makePropMapAndSet : prop list list -> stringProp list * prop list Map.map
  val makePropGraph : stringProp list -> BasicGraph.graph
  val eq : prop * prop -> bool
  val complement: prop * prop -> bool
    (* returns true if props are contrapositives *)
  val complementList : prop list * prop list -> bool 
    (* returns true if lists are contrapositives *)
  val tautology : prop list * prop list -> bool
  val removeIntraNodeTautologies : prop list -> prop list
  val positive : prop list -> bool
  val toString : prop list -> string
end
