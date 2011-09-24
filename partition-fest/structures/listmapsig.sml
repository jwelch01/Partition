signature LIST_MAP = 
sig
  type key
  type 'a map (* maps from key to 'a list *)

  val empty  : 'a map (* maps every key to [] *)
  val add    : key * 'a * 'a map -> 'a map (* adds to list *)
  val lookup : key * 'a map -> 'a list
end