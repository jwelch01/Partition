structure OrderedString : ord_key = struct
  type ord_key = string
  val compare = String.compare
end