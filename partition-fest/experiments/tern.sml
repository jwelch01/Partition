structure TernaryKeyChar : TERNARY_KEY = struct
  type ord_key = char
  val compare = Char.compare
  val sentinel :  ord_key = #"$"
end