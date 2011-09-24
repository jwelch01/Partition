signature FINITE_MAP = 
sig
  type Key
  type 'a Map

  exception NotFound of Key

  val empty  : 'a Map
  val bind   : Key * 'a * 'a Map -> 'a Map
  val lookup : Key * 'a Map -> 'a
end