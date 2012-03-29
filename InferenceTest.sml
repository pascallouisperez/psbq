let
  open Types
  open Parser
  open TokenizerFactory
  open Inference
  
  fun makeTestP(expression : string, polytype : string) =
        (expression ^ " : " ^ polytype,
         fn() => Assert.assertTrue(polytype =
           toString_poly(infer(parse(spacelessTokenize expression)))))
  
  val testCase = [
    makeTestP("3", "\\0|true.Int"),
    makeTestP("true", "\\0|true.Bool"),
    makeTestP("'j'", "\\0|true.Char"),
    makeTestP("+", "\\0|true.Int -> Int -> Int"),
    makeTestP("-", "\\0|true.Int -> Int -> Int"),
    makeTestP("fn(x) => x", "\\X1|true.X1 -> X1"),
    makeTestP("fn(x) => x 3", "\\X1|true.X1 -> X1"),
    makeTestP("fn x => fn f => f x", "\\X1|true.X1 -> X1")];
    
    fn x => (fn y => x (y y)) (fn y => x (y y))
in
  (*ConsoleTestRunner.runTestCase(testCase)*)
  (*toString_poly(infer(parse(spacelessTokenize "(fn x => x) 7")))*)
  toString_poly(infer(parse(spacelessTokenize "#foo {foo=fn x => x,bar=8}")))
end