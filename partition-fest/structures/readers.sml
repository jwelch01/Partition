
structure OutcomeReader : sig
    val lex     : string -> string list (* for debugging only *)
    val outcome : string -> Outcome.t
    val outcome2 : string -> string list option
end
= struct

  structure I = Impossible

  open Lex (* never do this *)

  infix 0 |||
  infix 2 >>> 
  infix 2 >>! 
  infix 5 ---

  fun dropWhile p [] = []
    | dropWhile p (c::cs) = if p c then dropWhile p cs
                            else c :: cs

  fun getWitness (_, cs) = SOME (implode (dropWhile Char.isSpace cs), [])

  fun isDelim c = c = #","   (* limited notion of delim *)
  fun idChar c = Char.isAlphaNum c orelse c = #"-" orelse c = #"."
                                   orelse c = #"/"

  val lex : string -> string list = 
     (* precondition: no space characters *)
    let fun token chars = dropwhite (  
            charEq #"-" --- charEq #"-"                  >>! getWitness
        ||| charSat isDelim                              >>> str
        ||| repeat1Chars idChar                          >>> implode
        ||| anyChar >>! invalid
        ) chars
       and invalid (c, cs) =
            I.impossible ("invalid initial character in `" ^ implode (c::cs) ^ "'")
    in #1 o valOf o repeatLex token o explode
  end


  fun toOutcome ["-given", id, "test", num, ",", soln, "passed"] =
        finish id num soln Outcome.PASSED
    | toOutcome ["-given", id, "test", num, ",", soln, badthing, witness] =
        finish id num soln (Outcome.NOTPASSED { outcome = badthing
                                              , witness = witness })
    | toOutcome _ = I.impossible "ill-formed input line"


  and finish id num soln outcome =
        case Int.fromString num
          of NONE   => I.impossible "test number would not convert to integer"
           | SOME n => { testid = id, num = n, solnid = soln, outcome = outcome }
  fun test ["-given", id, "test", num, ",", soln, "passed"] =
        NONE
    | test ["-given", id, "test", num, ",", soln, badthing, witness] =
	NONE
    | test x = SOME x;


  val outcome : string -> Outcome.t = toOutcome o lex

  val outcome2 : string -> string list option = test o lex

  structure UnitTests = struct

    val s1 = "-given bounds-check test 1, akhaku01 passed" : string
    val s2 = "-given full-methods test 3, bleike01 failed -- missing mapping methods"

    val o1 = { testid = "bounds-check", num = 1, solnid = "akhaku01"
             , outcome = Outcome.PASSED }

    val o2 = { testid = "full-methods", num = 3, solnid = "bleike01"
             , outcome = Outcome.NOTPASSED { outcome = "failed"
                                           , witness = "missing mapping methods" }
             }

    val _ = I.assert (outcome s1 = o1)
    val _ = I.assert (outcome s2 = o2)
        
  end

end
