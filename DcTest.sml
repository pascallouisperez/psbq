let
  open Assert
  open Parser
  open TokenizerFactory
  open Types
  open Constraints
  
  fun makeTest(constraint : string, issat : bool) =
    let
      val c = parseConstraint(tokenize constraint)
    in if issat then ("d|-  " ^ constraint,
         fn() => assertTrue(dc c))
       else ("d|/- " ^ constraint,
         fn() => assertFalse(dc c))
    end
  
  fun upDTest(t1s : string, t2s : string, up : int) =
    let
      val t1 = parseType(tokenize t1s)
      val t2 = parseType(tokenize t2s)
    in
      ("d+(" ^ Types.toString_mono t1 ^ ", " ^ Types.toString_mono t2 ^ ") = " ^ Int.toString up,
       fn() => assertTrue(Constraints.upD(t1, t2) = up))
    end
  
  fun downDTest(t1s : string, t2s : string, down : int) =
    let
      val t1 = parseType(tokenize t1s)
      val t2 = parseType(tokenize t2s)
    in
      ("d-(" ^ Types.toString_mono t1 ^ ", " ^ Types.toString_mono t2 ^ ") = " ^ Int.toString down,
       fn() => assertTrue(Constraints.downD(t1, t2) = down))
    end
  
  val testCase = [
    upDTest("top", "top", 0),
    upDTest("Int", "top", 1),
    upDTest("top", "Int", 2),
    upDTest("Int", "Int", 0),
    upDTest("Int", "Char", 2),
    upDTest("Char", "Bool", 2),
    upDTest("Bool", "Char", 2),
    upDTest("Bool -> Int", "Bool -> Int", 0),
    upDTest("Int -> Int", "Int -> top", 1),
    upDTest("Char -> Int", "Int -> Int", 2),
    upDTest("{x:Char,c:Bool}", "{x:Char,c:Bool}", 0),
    upDTest("{l:Int}", "{}", 1),
    upDTest("{}", "{l:Int}", 2),
    upDTest("{m:Int}", "{l:Int}", 2),
    upDTest("{l:{l:Int}}", "{l:{}}", 1),
    
    downDTest("top", "top", 0),
    downDTest("Int", "top", 2),
    downDTest("top", "Int", 1),
    downDTest("Int", "Int", 0),
    downDTest("Char", "Bool", 3),
    downDTest("Bool", "Char", 3),
    downDTest("Bool -> Int", "Bool -> Int", 0),
    downDTest("{l:Int} -> top", "{l:Int} -> top", 0),
    downDTest("Int -> top", "Int -> Char", 1),
    downDTest("Int -> Int", "Int -> top", 2),
    downDTest("Char -> Int", "Int -> Int", 3),
    downDTest("{x:Char,c:Bool}", "{x:Char,c:Bool}", 0),
    downDTest("{l:Int}", "{}", 2),
    downDTest("{}", "{l:Int}", 1),
    downDTest("{m:Int}", "{l:Int}", 2),
    downDTest("{x:Int,y:Bool}", "{x:Int,z:Char}", 2),
    downDTest("{l:{l:Int}}", "{l:{}}", 2),
    downDTest("{l:{}}", "{l:{l:Int}}", 1),
    downDTest("Bool -> Int", "Bool", 3),
    downDTest("Bool -> Int", "Int", 3),
    
    makeTest("true", true),
    makeTest("false", false),
    makeTest("Int <: Char", false),
    makeTest("Bool <: Int", false),
    makeTest("Bool -> Int <: Bool -> Int", true),
    makeTest("Bool -> Int <: Char -> Int", false),
    makeTest("Bool -> Int <: Int -> Bool", false),
    makeTest("Char <: X", true),
    makeTest("X <: Bool", true),
    makeTest("Bool -> Int <: X", true),
    makeTest("X -> Int <: Y", true),
    makeTest("Char -> Y <: X", true),
    makeTest("Char -> Y <: Y", true),
    makeTest("{y: Int} <: {y: Int}", true),
    makeTest("{} <: {y: Int}", false),
    makeTest("{y: Int} <: {}", true),
    makeTest("{y: Int} <: {y: X}", true),
    makeTest("{y: Int} <: {x: Y}", true),
    makeTest("{y: Int} <: X -> Y", true),
    makeTest("{lab: Int} <: {lab: X -> X}", true),
    makeTest("(Int -> Int) -> Int <: Y -> X", true),
    makeTest("Int -> X -> X <: Int -> X", true),
    makeTest("{l:Int} <: {l:Char}", false),
    makeTest("X <: Int ^ {l:X} <: {l:Char}", false),
    makeTest("{l:Int} -> X <: Y -> Char ^ Y <: {l:X}", false),
    makeTest("{l:Bool} -> X <: Y -> Bool ^ Y <: {l:X}", true),
    makeTest("X <: Int ^ {a:X,b:Y} <: Z ^ Z <: {a:Int} ^ Z <: {b:{a:Bool}} ^ Y <: {a:X}", false),
    makeTest("{l:Int} <: X ^ X <: {l:Int,m:Int}", false),
    makeTest("X <: {l:Int} ^ X <: {m:Int} ^ {l:Int,p:Int} <: X", false),
    makeTest("{l:Y} <:  {l:Y,m:Y}", true)]
in
  ConsoleTestRunner.runTestCase(testCase)
  (*ConsoleTestRunner.runTestCase [makeTest("{l:Int} -> X <: Y -> Char ^ Y <: {l:X}", false)]*)
end