let
  open Assert
  open Parser
  open TokenizerFactory
  open Types
  open Constraints
  
  fun eqTest(d1, d2, eq) =
    (Distances.toString d1 ^ " " ^ (if eq then "=" else "!=") ^ " " ^ (Distances.toString d2),
     fn() => assertTrue(if eq then Distances.equals (d1, d2) else not (Distances.equals (d1, d2))))
  
  val t1 = parseType (tokenize "Int")
  val t2 = parseType (tokenize "Bool")
  val t3 = parseType (tokenize "Char")
in
  ConsoleTestRunner.runTestCase [eqTest(Distances.empty, Distances.empty, true)]
end