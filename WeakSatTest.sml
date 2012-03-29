let
  open Assert
  open Parser
  open TokenizerFactory
  open Types
  
  fun makeTest(constraint : string, issat : bool) =
    let
      val c = parseConstraint(tokenize constraint)
    in if issat then ("ws|=  " ^ constraint,
         fn() => assertTrue(Constraints.ws c))
       else ("ws|/= " ^ constraint,
         fn() => assertFalse(Constraints.ws c))
    end
  
  val testCase = [
    makeTest("true", true),
    makeTest("false", false),
    makeTest("Int <: Char", true),
    makeTest("Bool <: Int", true),
    makeTest("Bool -> Int <: Bool -> Int", true),
    makeTest("Bool -> Int <: Char -> Int", true),
    makeTest("Bool -> Int <: Int -> Bool", true),
    makeTest("Char <: X", true),
    makeTest("X <: Bool", true),
    makeTest("X <: X", true),
    makeTest("Bool -> Int <: X", true),
    makeTest("X -> Int <: Y", true),
    makeTest("Char -> Y <: X", true),
    makeTest("{y: Int} <: {y: Int}", true),
    makeTest("{y: Int} <: {y: X}", true),
    makeTest("{y: Int} <: {x: Y}", false),
    makeTest("{y: Int} <: {}", true),
    makeTest("{} <: {y:Int}", false),
    makeTest("{y: Int} <: X -> Y", false),
    makeTest("{lab: Int} <: {lab: X -> X}", false),
    makeTest("(Int -> Int) -> Int <: Y -> X", true),
    makeTest("X <: X -> X", false),
    makeTest("X -> X <: X", false),
    makeTest("(Int -> Int) -> Int <: X -> X", false),
    makeTest("Int <: X ^ Char <: X", true),
    makeTest("Int <: X ^ Int -> Int <: X", false),
    makeTest("X <: Int ^ X -> X <: Y ^ Int <: Y", false),
    makeTest("X <: Int ^ X -> X <: Y ^ Int -> Char <: Y", true),
    makeTest("X <: Int ^ true ^ X -> X <: Y ^ true ^ Int -> Char <: Y", true),
    makeTest("X <: Int ^ false ^ X -> X <: Y ^ true ^ Int -> Char <: Y", false),
    makeTest("X <: {l:Int} ^ X <: {m:Bool}", true),
    makeTest("{l:Y} <:  {l:Y,m:Y}", false)]
in
  ConsoleTestRunner.runTestCase(testCase)
end