let
  open Constraints
  
  fun makeTest(input : string, c1 : Types.Constraint) = (input,
      fn() => Assert.assertTrue(let
        val c2 : Types.Constraint = Parser.parseConstraint(TokenizerFactory.tokenize input);
      in
        equals c1 c2
      end));
  
  val i = Types.BaseType(Types.Int);
  val c = Types.BaseType(Types.Char);
  val b = Types.BaseType(Types.Bool);
  
  val x = Types.VarType("X");
  val y = Types.VarType("Y");

  val i2i = Types.FunType(i, i);  
  val b2i = Types.FunType(b, i);
  val x2i = Types.FunType(x, i);
  val c2y = Types.FunType(c, y);
  val y2x = Types.FunType(y, x);
  val i2i_2i = Types.FunType(i2i, i);
  
  val testCase = [
    makeTest("true", Types.Boolean(true)),
    makeTest("true ^ true", Types.AndConstraint(Types.Boolean(true), Types.Boolean(true))),
    makeTest("Int <: Char", Types.SubtypingConstraint(i, c)),
    makeTest("Bool <: Int", Types.SubtypingConstraint(b, i)),
    makeTest("Bool -> Int <: Bool -> Int", Types.SubtypingConstraint(b2i, b2i)),
    makeTest("Char <: X", Types.SubtypingConstraint(c, x)),
    makeTest("X <: Bool", Types.SubtypingConstraint(x, b)),
    makeTest("Bool -> Int <: X", Types.SubtypingConstraint(b2i, x)),
    makeTest("X -> Int <: Y", Types.SubtypingConstraint(x2i, y)),
    makeTest("Char -> Y <: X", Types.SubtypingConstraint(c2y, x)),
    makeTest("(Int -> Int) -> Int <: Y -> X", Types.SubtypingConstraint(i2i_2i, y2x)),
    makeTest("Int <: X ^ Char <: X",
      Types.AndConstraint(
        Types.SubtypingConstraint(i, x),
        Types.SubtypingConstraint(c, x)))];
in
  ConsoleTestRunner.runTestCase(testCase)
end