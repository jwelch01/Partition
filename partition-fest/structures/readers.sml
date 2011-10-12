structure OutcomeReader = struct

  exception Barf of string

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

  fun lex oneline : string -> string list = 
     (* precondition: no space characters *)
    let fun token chars = dropwhite (  
            repeat1Chars idChar                          >>> implode
        ||| charSat isDelim                              >>> str
        ||| charEq #"-" --- charEq #"-"                  >>! getWitness
        ||| anyChar >>! invalid
        ) chars
       and invalid (c, cs) =
            raise Barf ("invalid initial character in `" ^ implode (c::cs) ^ "'")
    in #1 o valOf o repeatLex token o explode
  end




end
