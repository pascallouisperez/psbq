let
  open Types
  open Constraints
  
  val testCase = [
    ("* = * u []", fn() => Assert.assertTrue(SOME [] = unify_mono(StarType, StarType)))];
in
  ConsoleTestRunner.runTestCase(testCase)
end