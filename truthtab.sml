structure TT = struct
  datatype outcome = PASS | FAIL | DNR


  infix 0 ==>

  fun p ==> q = not p orelse q

  fun formula1 (o1, o2) = o1 = DNR ==> o2 = DNR
  fun formula2 (o1, o2) = not (o2 = DNR) ==> not (o1 = DNR)

  val alloutcomes = [PASS, FAIL, DNR]
  val allpairs : (outcome * outcome) list =
      List.concat (map (fn o1 => map (fn o2 => (o1, o2)) alloutcomes) alloutcomes)

  fun make_tt2 p = map (fn pair => (pair, p pair)) allpairs

  val example = make_tt2 formula1
  val example2 = make_tt2 formula2
end
