signature OUTCOME_MAP = 
sig
  type map

  exception NotFound of string

  val empty  : map
  val bind   : char list * Outcome.outcome * map -> map (* replaces if already present *)
  val lookup : char list * map -> Outcome.outcome

  val mapFold   : ((char list * Outcome.outcome * 'b) -> 'b) -> 'b -> map -> 'b

end