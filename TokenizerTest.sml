let
  open TokenizerFactory
  
  fun makeTest(input : string, tokens) = (input,
      fn() => Assert.assertTrue(let
        val t = tokenize(input);
      in
        List.all (fn(token) => token = #next t ()) tokens
      end));
  
  fun makeBacktrackTest(name, tokens) = ("backtract " ^ name,
     fn() => Assert.assertTrue(let
       val t = tokenize("")
       val t2 = List.map (fn(token) => #previous t token) (List.rev tokens)
     in
        List.all (fn(token) => token = #next t ()) tokens andalso
        #next t () = EOF
      end));
  
  val testCase = [
  makeTest("0", [NUMBER(0), EOF]),
  makeTest("9", [NUMBER(9), EOF]),
  makeTest("a", [NAME("a"), EOF]),
  makeTest("Z", [NAME("Z"), EOF]),
  makeTest("true", [TRUE, EOF]),
  makeTest("false", [FALSE, EOF]),
  makeTest("'", [SINGLEQUOTE, EOF]),
  makeTest("\"", [DOUBLEQUOTE, EOF]),
  makeTest("#", [POUND, EOF]),
  makeTest("fn", [FN, EOF]),
  makeTest("let", [LET, EOF]),
  makeTest("(", [LPAREN, EOF]),
  makeTest(")", [RPAREN, EOF]),
  makeTest(".", [PERIOD, EOF]),
  makeTest("{", [LACCO, EOF]),
  makeTest("}", [RACCO, EOF]),
  makeTest(",", [COMMA, EOF]),
  makeTest(":", [COLON, EOF]),
  makeTest("+", [PLUS, EOF]),
  makeTest("-", [MINUS, EOF]),
  makeTest("=>", [DARROW, EOF]),
  makeTest("->", [SARROW, EOF]),
  makeTest("val", [VAL, EOF]),
  makeTest("^", [AND, EOF]),
  makeTest("<:", [SUBTYPE, EOF]),
  makeTest("and", [AND, EOF]),
  makeTest("end", [END, EOF]),
  makeTest("Int", [INTTYPE, EOF]),
  makeTest("Char", [CHARTYPE, EOF]),
  makeTest("Bool", [BOOLTYPE, EOF]),
  makeTest("top", [TOPTYPE, EOF]),
  makeTest("=", [EQUALS, EOF]),
  makeTest("endin", [END, IN, EOF]),
  makeTest("===>", [EQUALS, EQUALS, DARROW, EOF]),
  makeTest("falsefn", [FALSE, FN, EOF]),
  makeTest("fi", [NAME "fi", EOF]),
  makeTest("falsefnfit", [FALSE, FN, NAME "fit", EOF]),
  makeTest("let    val    in", [LET, VAL, IN, EOF]),
  makeTest("'7'", [SINGLEQUOTE, NUMBER 7, SINGLEQUOTE, EOF]),
  makeTest("{l=4,m=8}",
    [LACCO, NAME "l", EQUALS, NUMBER 4, COMMA, NAME "m", EQUALS, NUMBER 8, RACCO, EOF]),
  makeTest("8   \n   3", [NUMBER 8, NUMBER 3, EOF]),
  makeTest("fn(x) => 4",
    [FN, LPAREN, NAME "x", RPAREN, DARROW, NUMBER 4, EOF]),
  makeBacktrackTest("FN LPAREN", [FN, LPAREN]),
  makeBacktrackTest("NUMBER NUMBER", [NUMBER 798, NUMBER 12]),
  makeBacktrackTest("EOF NUMBER", [EOF, NUMBER 12]),
  ("f 8: read, read, unread, read", fn () => 
    Assert.assertTrue(
      let
        val t = tokenize "f 8"
      in
        (#next t () = NAME("f")) andalso
        (#next t () = NUMBER(8)) andalso
        (#previous t (NUMBER 8) = ()) andalso
        (#next t () = NUMBER(8))
      end))];
in
  ConsoleTestRunner.runTestCase(testCase)
end