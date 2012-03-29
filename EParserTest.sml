let
  fun makeTest(input : string) = (input,
      fn() => Assert.assertTrue(let
        val ex = Parser.parse(TokenizerFactory.tokenize input);
      in
        input = Expressions.toString(ex)
      end));
  
  fun makeTest2(input : string, output : string) = (input ^ " to " ^ output,
      fn() => Assert.assertTrue(let
        val ex = Parser.parse(TokenizerFactory.tokenize input);
      in
        output = Expressions.toString(ex)
      end));
  
  val exNames = Names.getGenerator("ex");
  
  val labelNames = Names.getGenerator("label");
  
  val testCase = [
    makeTest("3"),
    makeTest("145698"),
    makeTest("true"),
    makeTest("false"),
    makeTest("'a'"),
    makeTest("'b'"),
    makeTest("'6'"),
    makeTest("'0'"),
    makeTest("'9'"),
    makeTest("x"),
    makeTest("+"),
    makeTest("-"),
    makeTest("f 8"),
    makeTest("{x=5}"),
    makeTest("{x='z', y=true}"),
    makeTest("#l 4"),
    makeTest("#l x"),
    makeTest("#x {x=5, y=8}"),
    makeTest("#y {x=5, y=fn y => y}"),
    makeTest("+ x 1"),
    makeTest("- x y"),
    makeTest("fn x => x x"),
    makeTest("fn x => fn f => f x"),
    makeTest("xyz"),
    makeTest("foo"),
    makeTest2("fn(x) => x", "fn x => x"),
    makeTest("fn x => x"),
    makeTest("fn x => x 5"),
    makeTest("fn w => 5 fn x => x 7"),
    makeTest2("fn w => 5 (fn x => x 7)", "fn w => 5 fn x => x 7"),
    makeTest("(fn w => 5) ((fn x => x) 7)"),
    makeTest("let x = 4 in x end"),
    makeTest("let x = (let y = 7 in y end) in x end"),
    makeTest2("(3)", "3")];
in
  ConsoleTestRunner.runTestCase(testCase)
end