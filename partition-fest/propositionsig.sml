signature PROPOSITION = sig
  type testset = TestSet.set
  type result
  type prop


  val partition : (prop * prop -> bool) -> prop list -> prop list list
  val makePropList : testset -> prop list

  val eq : prop * prop -> bool

  val tautology : prop list * prop list -> bool
  val removeIntraNodeTautologies : prop list -> prop list
  
  val union : prop * prop -> prop
  val unionstar : prop list -> prop

  val getPropsWithResult : prop list -> (bool * Outcome.outcome) -> prop list
  val getId : prop -> string * string 
  val toString : prop list -> string

  (* string * result used rather than prop for ease of interaction with map
     interface and .dot file format *)
  val /->/ : (string * result) * (string * result) -> bool
  val makePropGraph : (string * result) list -> BasicGraph.graph
  val makePropMapAndSet : prop list list -> 
                          (string * result) list * prop list Map.map


end
