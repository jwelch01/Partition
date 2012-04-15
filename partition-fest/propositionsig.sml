signature PROPOSITION = sig
  type testset = TestSet.set
(*  type prop *)

  type result = (string * bool) list
  (*datatype prop = PROP of { flag : bool, test : string, number : string,
			      outcome : outcome, results : result }
*)
  type prop = bool * string * string * string * result

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
  
  val union : prop * prop -> prop
  val unionstar : prop list -> prop

  val getPropsWithResult : prop list -> (bool * string) -> prop list

  val getId : prop -> string * string

  val toString : prop list -> string
end
