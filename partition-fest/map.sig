signature MAP = sig
  type map
  type key
  type value
 
  val insert : (key * value * map) -> map
  val lookup : (key * map) -> value
end

  