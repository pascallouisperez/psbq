signature PARSER = sig
  exception ParseException of string
  
  val parse : TokenizerFactory.Tokenizer -> Expressions.Expression
  
  val parseConstraint : TokenizerFactory.Tokenizer -> Types.Constraint
  
  val parseType : TokenizerFactory.Tokenizer -> Types.MonoType
end