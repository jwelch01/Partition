signature TERNARY_KEY = sig
  include ORD_KEY
  val sentinel : ord_key (* guaranteed not to occur in any string *)
end