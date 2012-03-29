structure Parser :> PARSER = struct
  open TokenizerFactory
  open Expressions
  
  exception ParseException of string
  
  val exNames = Names.getGenerator("ex");
  
  fun backtrack(t, input : Tokenizer, ex) =
        let val t = #previous input t in ex end;
  
  fun maybe(token, input : Tokenizer) : Token option =
      let
        val t = #next input ()
      in
        if (t = token) then SOME t
        else backtrack(t, input, NONE)
      end
  
  fun name(NAME s, input : Tokenizer) : string = s
    | name(_, input) = raise ParseException ""
  
  fun number(NUMBER n, input : Tokenizer) : int = n
    | number(_, input) = raise ParseException ""
  
  fun ex m = "'" ^ m ^ "' excpected"
  
  fun parse(input : Tokenizer) =
        let
          val l = List.rev (pApp(#next input (), input, []))
        in
          List.foldl (fn(e2, e1) => Application(e1, e2)) (List.hd l) (List.tl l)
        end
  
  and pApp(token, input : Tokenizer, l : Expression list) = case token
         of EOF => l
          | RACCO => backtrack(RACCO, input, l)
          | RPAREN => backtrack(RPAREN, input, l)
          | COMMA => backtrack(COMMA, input, l)
          | IN => backtrack(IN, input, l)
          | END => backtrack(END, input, l)
          | _ =>
            let
              val e = pExpression(token, input)
            in
              pApp(#next input (), input, e :: l)
            end
  
  and pExpression(token as FN, input : Tokenizer) = pFn(token, input)
    | pExpression(token as POUND, input : Tokenizer) =
        pSel(#next input (), input)
    | pExpression(token as LET, input : Tokenizer) =
        pLet(token, input)
    | pExpression(token as LPAREN, input : Tokenizer) =
        let val e = parse(input) in case maybe(RPAREN, input)
          of SOME _ => e
           | NONE => raise ParseException ""
        end
    | pExpression(token as _, input : Tokenizer) = pConstant(token, input)
  
  and pSel(NAME s, input : Tokenizer) =
        Expressions.Selection(s, parse(input))
    | pSel _ = raise ParseException ""
  
  and pLet(LET, input : Tokenizer) =
        let val x = name(#next input (), input) in case maybe(EQUALS, input)
          of SOME _ => let val e1 = parse(input) in
              case maybe(IN, input) 
                of SOME _ => let val e2 = parse(input) in
                    case maybe(END, input)
                      of SOME _ => Expressions.Let(#create exNames x, e1, e2)
                       | NONE => raise ParseException (ex "end")
                     end
                 | NONE => raise ParseException (ex "in")
               end
           | NONE => raise ParseException (ex "=")
        end
    | pLet(_, input : Tokenizer) = raise ParseException ""
  
  and pFn(FN, input : Tokenizer) = (case maybe(LPAREN, input)
        of SOME _ => let val x = name(#next input (), input) in case maybe(RPAREN, input)
            of SOME _ => (case maybe(DARROW, input)
                of SOME _ => Expressions.Abstraction(#create exNames x, parse input)
                 | NONE => raise ParseException "")
             | NONE => raise ParseException "" end
         | NONE => let val x = name(#next input (), input) in case maybe(DARROW, input)
            of SOME _ => Expressions.Abstraction(#create exNames x, parse input)
             | NONE => raise ParseException "" end)
    | pFn(_, input : Tokenizer) = raise ParseException ""
  
  and pConstant(token, input : Tokenizer) = case token
   of NAME s => Expressions.Variable(#create exNames s)
    | NUMBER n => Expressions.Constant(Expressions.Int n)
    | TRUE => Expressions.Constant(Expressions.Bool(true))
    | FALSE => Expressions.Constant(Expressions.Bool(false))
    | PLUS => Expressions.Constant(Expressions.Plus)
    | MINUS => Expressions.Constant(Expressions.Minus)
    | SINGLEQUOTE => (case (#next input ())
        of NAME s => (case (#next input ())
          of SINGLEQUOTE => makeChar s
            | _  => raise ParseException (ex "single quote"))
         | NUMBER n => (case (#next input ())
          of SINGLEQUOTE => makeChar (Int.toString n)
            | _  => raise ParseException (ex "single quote"))
         | _  => raise ParseException "single character expected")
    | LACCO => pRecord(token, input)
    | token as _ => raise ParseException "constant expected"
  
  and makeChar s = case String.explode s
        of [c] => Expressions.Constant(Expressions.Char(c))
         | _ => raise ParseException ("single chracter expected, " ^ s ^ " found")
  
  and pRecord(LACCO, input : Tokenizer) =
        let
          val labelsExps = pLabelExp(#next input (), input, [])
        in case maybe(RACCO, input)
             of SOME _ => Expressions.Record(List.rev labelsExps)
              | NONE => raise ParseException ""
        end
    | pRecord(_, input : Tokenizer) = raise ParseException ""
  
  and pLabelExp(NAME label, input : Tokenizer, pred) =
        (case maybe(EQUALS, input)
          of SOME _ => let val newpred = (label, parse(input)) :: pred in
                 (case maybe(COMMA, input)
                   of SOME _ => pLabelExp(#next input (), input, newpred)
                    | NONE => newpred)
               end
           | NONE => raise ParseException "")
    | pLabelExp(_, input : Tokenizer, pred) = raise ParseException ""
  
  fun parseConstraint(input : Tokenizer) = pConstraint(#next input (), input)
  
  and pConstraint(token, input : Tokenizer) =
    let
      val c = case token
        of TokenizerFactory.TRUE => Types.Boolean(true)
         | TokenizerFactory.FALSE => Types.Boolean(false)
         | _ => pSubtype(token, input)
    in
      case maybe(AND, input)
        of SOME _ => Types.AndConstraint(c, pConstraint(#next input (), input))
         | NONE => c
    end
  
  and pSubtype(token, input : Tokenizer) =
    let
      val t = pType(token, input)
    in case maybe(SUBTYPE, input)
        of SOME _ => Types.SubtypingConstraint(t, pType(#next input (), input))
         | NONE => raise ParseException ""
    end
  
  and parseType input = pType (#next input (), input)
  
  and pType(token, input : Tokenizer) =
        let
          val t1 = pAtomicType(token, input)
        in
          case maybe(SARROW, input)
            of SOME _ => Types.FunType(t1, pType(#next input (), input))
            | NONE => t1
         end
  
  and pAtomicType(token, input : Tokenizer) = (case token
   of INTTYPE => Types.BaseType(Types.Int)
    | CHARTYPE => Types.BaseType(Types.Char)
    | BOOLTYPE => Types.BaseType(Types.Bool)
    | TOPTYPE => Types.TopType
    | NAME s => Types.VarType s
    | LACCO => pRecordType(token, input)
    | LPAREN => let val t = pType(#next input (), input) in
          case maybe(RPAREN, input)
            of SOME _ => t
             | NONE => raise ParseException "" end
    | _ => raise ParseException "expecting an atomic type")
  
  and pRecordType(LACCO, input : Tokenizer) =
        (case maybe(RACCO, input)
          of SOME _ => Types.RecordType StringMap.empty
           | NONE =>
              let
                val labelsExps = pLabelType(#next input (), input, StringMap.empty)
              in case maybe(RACCO, input)
                   of SOME _ => Types.RecordType labelsExps
                    | NONE => raise ParseException ""
              end)
    | pRecordType(_, input : Tokenizer) = raise ParseException ""
  
  and pLabelType(token as NAME label, input : Tokenizer, map) =
        (case maybe(COLON, input)
          of SOME _ =>
               let
                 val newmap = StringMap.insert (map, label, pType (#next input (), input))
               in
                 (case maybe(COMMA, input)
                   of SOME _ => pLabelType(#next input (), input, newmap)
                    | NONE => newmap)
               end
           | NONE => raise ParseException "no colon after label")
    | pLabelType(_, input : Tokenizer, pred) = raise ParseException "expecting a labeled type"
end