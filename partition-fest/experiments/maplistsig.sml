signature LIST_MAP = 
sig
  type Key
  type 'a Map (* maps from key to 'a list *)

  exception NotFound of Key

  val empty  : 'a Map (* maps every key to [] *)
  val add    : Key * 'a * 'a Map -> 'a Map (* adds to list *)
  val lookup : Key * 'a Map -> 'a list
end