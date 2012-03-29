signature INFERENCE = sig
  exception TypeError of string
  
  val infer : Expressions.Expression -> Types.PolyType;
  
  val inferp : BQEnv.environment * Expressions.Expression -> Types.PolyType;
  
  val inferm : BQEnv.environment * Expressions.Expression -> Types.Constraint * Types.MonoType;
end