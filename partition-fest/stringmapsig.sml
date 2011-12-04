signature STRING_MAP = 
sig
  type 'a map

  exception NotFound of string

  val empty  : 'a map
  val bind   : char list * 'a * 'a map -> 'a map (* replaces if already present *)
  val lookup : char list * 'a map -> 'a

  val mapFold   : ((char list * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b

end