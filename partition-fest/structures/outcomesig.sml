signature OUTCOME  = sig
  type outcome
  type t

  val cmpOutcomes : outcome * outcome -> order
  val cmpOutcomesEq : outcome * outcome -> bool
end