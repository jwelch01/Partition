signature TERNARY_KEY = sig
  type ord_key
  val compare : (ord_key * ord_key) -> order
  val sentinel : ord_key
end