signature LIST_MAP = 
sig
  type key
  type 'a map (* maps from key to 'a list *)

  val empty  : 'a map (* maps every key to [] *)
  val add    : key * 'a * 'a map -> 'a map (* adds to list *)
  val lookup : key * 'a map -> 'a list
  val bind   : key * 'a * 'a map -> 'a map (* replaces existing value *)  

  val mapFold   : ((key * 'a list * 'b) -> 'b) -> 'b -> 'a map -> 'b

end