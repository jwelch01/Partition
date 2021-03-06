structure Lex = struct
  (* Tokenization *)

  type 'a lexer = char list -> ('a * char list) option

  fun oneChar lex [] = NONE
    | oneChar lex (c::cs) = lex (c:char) cs

  exception Lex

  fun charSat p = oneChar (fn c => fn cs => if p c then SOME (c, cs) 
                                                   else raise Lex)
  fun charEq c = charSat (fn c' : char => c = c')
  val anyChar = charSat (fn _ => true)

  val isDelim = Char.contains "(); \t\n"

  infix 0 |||
  fun (l1 ||| l2) chars = l1 chars handle Lex => l2 chars

  infix 2 >>> 
  fun (lex >>> f) chars =
                 case lex chars
                   of NONE => NONE
                    | SOME (x, chars) => SOME (f x, chars)

  infix 2 >>! fun (p >>! f) chars =
                case p chars
                  of NONE => NONE
                   | SOME (x, chars) => f (x, chars)

  infix 5 ---
  fun (l1 --- l2) chars =
    case l1 chars
      of NONE => NONE
       | SOME (a, chars) =>
           case l2 chars
             of NONE => raise Lex
              | SOME (b, chars) => SOME ((a,b), chars)

  fun repeatLex l chars = 
    let fun repeat chars =
        (case l chars
           of NONE => ([], chars)
            | SOME (tok, chars') =>
                let val (rest, chars'') = repeat chars'
                in (tok::rest, chars'')
                end
        ) handle Lex => ([], chars)
    in SOME (repeat chars)
    end

  fun repeat1Lex l = l --- repeatLex l >>> op ::

  val repeat1Chars = repeat1Lex o charSat
  val repeatChars  = repeatLex o charSat

  fun optionalLex l chars =
    (case l chars of NONE => SOME (NONE, chars)
                   | SOME (token, chars) => SOME (SOME token, chars)
    ) handle Lex => SOME (NONE, chars)

  fun notLex l chars =
    case l chars handle Lex => NONE
      of NONE => SOME ((), chars)
       | SOME _ => raise Lex

  fun intToken isDelim =
    repeat1Lex (charSat Char.isDigit) --- notLex (charSat (not o isDelim))
    >>> (valOf o Int.fromString o implode o #1)

  fun dropwhite l = repeatLex (charSat Char.isSpace) >>! l o #2

  fun skipUntil (comment, token) =
    token ||| comment >>! (fn (_, chars) => skipUntil (comment, token) chars)

  val untilwhite = repeatChars (not o isDelim)

  fun stdToken loc l = 
    dropwhite l >>> (fn t => (loc, t))


end
