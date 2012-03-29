signature TOKENIZERFACTORY = sig
  datatype Token = SPACES
                 | DIGIT of int
                 | CHAR of char
                 | NUMBER of int
                 | NAME of string
                 | AND
                 | PLUS
                 | MINUS
                 | TRUE
                 | FALSE
                 | INTTYPE
                 | CHARTYPE
                 | BOOLTYPE
                 | TOPTYPE
                 | SINGLEQUOTE
                 | DOUBLEQUOTE
                 | POUND
                 | PERIOD
                 | COLON
                 | FN
                 | LET
                 | LPAREN
                 | RPAREN
                 | LACCO
                 | RACCO
                 | COMMA
                 | DARROW
                 | SARROW
                 | SUBTYPE
                 | VAL
                 | IN
                 | END
                 | EQUALS
                 | EOS
                 | EOF;
  
  type Tokenizer = {next : unit -> Token, previous : Token -> unit}
  
  exception Tokenization
  
  val tokenize : string -> Tokenizer
end