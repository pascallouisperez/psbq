let
  open Parser
  open TokenizerFactory
  open Types
  open Constraints
  open Set
  
  fun makeTest(constraint : string, varsList : string list) =
        let
          val vars = List.foldl (fn(x, b) => Set.add(b, x)) Set.empty varsList
        in
          ("ftv " ^ constraint(* ^ " = " ^ Set.toString(fn x => x, vars)*),
           fn() =>
             Assert.assertTrue(Set.equal(
               ftv(parseConstraint(tokenize constraint)),
               vars)))
        end
  
  val testCase = [
    makeTest("Int <: Int", []),
    makeTest("Int <: X", ["X"]),
    makeTest("Y <: X", ["X", "Y"]),
    makeTest("X -> Y <: Bool -> Z", ["X", "Y", "Z"]),
    makeTest("X -> {x: Int} <: Bool -> Z", ["X", "Z"]),
    makeTest("Char -> {x: X} <: Bool -> Bool", ["X"]),
    makeTest("Z -> {x: X} <: {l:U, v:V}", ["U", "V", "X", "Z"])];
in
  ConsoleTestRunner.runTestCase(testCase)
end