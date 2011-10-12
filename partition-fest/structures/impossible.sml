structure Impossible : sig
                           val impossible : string -> 'a
                           val assert : bool -> unit
                       end =
struct
  exception ThisCan'tHappen of string
  fun impossible msg = raise ThisCan'tHappen msg

  exception AssertionFailed
  fun assert p = if p then () else raise AssertionFailed
end
