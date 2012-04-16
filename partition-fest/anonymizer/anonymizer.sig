signature ANONYMIZER = sig
  type salt = string

  val toNumbers : salt * string list -> (string * int) list

   (* Given a collection of strings and a salt, returns a mapping from
      string to small integer.  The function is pure and repeatable,
      but even a small change to the string list or the salt should
      result in a very different numbering. *)
end


signature NUM_ANONYMIZER = ANONYMIZER
