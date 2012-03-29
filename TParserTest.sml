let
  open Constraints
  
  fun makeTest(input : string) = (input,
      fn() => Assert.assertTrue(
        Types.toString_mono (Parser.parseType(TokenizerFactory.tokenize input)) = input))
  
  val testCase = [
    makeTest("Int"),
    makeTest("Char"),
    makeTest("Bool"),
    makeTest("Char -> Int"),
    makeTest("{}"),
    makeTest("{l:Int}"),
    makeTest("Int -> {l:Int}"),
    makeTest("{l:X} -> {l:Int}")]
in
  ConsoleTestRunner.runTestCase(testCase)
end