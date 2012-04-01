signature PROPOSITION = sig
  type testset
  type prop = (bool * string * string * string * (string * bool) list)
  type simpProp = (string * (string * bool) list)

  val /->/ : simpProp * simpProp -> bool
  val partition : (prop * prop -> bool) -> prop list -> prop list list
  val makePropList : testset list -> prop list
  val makePropMapAndSet : prop list list -> simpProp list * prop list Map.map
  val makePropGraph : simpProp list -> BasicGraph.graph
  val eq : prop * prop -> bool
  val complement: prop * prop -> bool
    (* returns true if props are contrapositives *)
  val complementList : prop list * prop list -> bool 
    (* returns true if lists are contrapositives *)
  val tautology : prop list * prop list -> bool
  val removeIntraNodeTautologies : prop list -> prop list
  val positive : prop list -> bool
end
