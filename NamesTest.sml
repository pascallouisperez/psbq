let
  val gen = Names.getGenerator("test1");
  val scope1_X1 = #create gen "X";
  val scope1_Y1 = #create gen "Y";
  val scope1_Y2 = #create gen "Y";
  val scope1_X2 = #create gen "X";
  
  val t1 = #newScope gen ();
  val scope2_X1 = #create gen "X";
  val scope2_Y1 = #create gen "Y";
  val scope2_X2 = #create gen "X";
  val scope2_Y2 = #create gen "Y";
  
  val t2 = #previousScope gen ();
  val scope1_X3 = #create gen "X";
  
  val scope1_X1_gen2 = #create (Names.getGenerator("test2")) "X";
  
  val scope1_X1_gen3 = #create (Names.getGenerator("test1")) "X";
  
  fun eq(n1:Names.Name, n2:Names.Name) = n1 = n2;
  val testCase = [
        ("scope1: X1 = X2", fn() => Assert.assertEquals(eq, scope1_X1, scope1_X2)),
        ("scope1: X1 = X3", fn() => Assert.assertEquals(eq, scope1_X1, scope1_X3)),
        ("scope1: X1 = X1", fn() => Assert.assertEquals(eq, scope2_X1, scope2_X2)),
        ("scope1: Y1 = Y2", fn() => Assert.assertEquals(eq, scope1_Y1, scope1_Y2)),
        ("scope2: Y1 = Y2", fn() => Assert.assertEquals(eq, scope2_Y1, scope2_Y2)),
        ("scope1 X1 != scope2 X1", fn() => Assert.assertNotEquals(eq, scope1_X1, scope2_X2)),
        ("scope1 Y1 != scope2 Y1", fn() => Assert.assertNotEquals(eq, scope1_Y1, scope2_Y2)),
        ("different domains, X != X", fn() => Assert.assertNotEquals(eq, scope1_X1, scope1_X1_gen2)),
        ("same domain, X = X", fn() => Assert.assertEquals(eq, scope1_X1, scope1_X1_gen3))];
in
  ConsoleTestRunner.runTestCase(testCase)
end