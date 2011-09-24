structure OrderedString : ORD_KEY = struct
  type ord_key = string
  val compare = String.compare
end