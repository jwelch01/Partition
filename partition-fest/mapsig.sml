signature FINITE_MAP = 
sig
  type key
  type 'a map

  exception NotFound of key

  val empty  : 'a map
  val bind   : key * 'a * 'a map -> 'a map (* replaces if alreay present *)
  val lookup : key * 'a map -> 'a

  val mapFold   : ((key * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b

end