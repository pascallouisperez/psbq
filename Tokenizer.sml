structure TokenizerFactory :> TOKENIZERFACTORY = struct
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
  
  type Tokenizer = {next : unit -> Token, previous : Token -> unit};
  
  exception Tokenization;
  
  fun preTokenize(input) =
      let
        val characters = ref (String.explode input);
        
        fun nextChar() = if (List.length(!characters) > 0) then
            let
              val c = List.hd(!characters);
              val t = characters := List.drop(!characters, 1);
            in SOME(c) end else NONE;
        
        fun putChar(c) = characters := c :: !characters;
        
        fun try(s : string) : bool =
            let
              val s_chars = String.explode s;
              val input_chars = List.map (fn(c) => nextChar()) s_chars;
            in
              if (ListPair.all (fn(op1,c2) => (case (op1) of SOME(c1) => c1 = c2 | NONE => false))
                           (input_chars, s_chars)) then true
              else let
                val t = List.map (fn(op1) => case (op1) of SOME(c) => putChar(c) | NONE => ()) (List.rev input_chars);
              in false end end;
        fun eatSpaces() : Token = case (nextChar())
              of SOME(c) => if Char.isSpace c then eatSpaces ()
                            else let val t = putChar(c) in SPACES end
              | NONE => SPACES
        
        val backtrackedTokens : Token list ref = ref [];
      in
        {next = fn() => if (List.length(!backtrackedTokens)) > 0 then let
                  val nextToken = List.hd(!backtrackedTokens);
                  val t = backtrackedTokens := List.drop (!backtrackedTokens, 1);
                in
                  nextToken
                end
           else case (nextChar())
           of SOME(#"'") => SINGLEQUOTE
           | SOME(#"\"") => DOUBLEQUOTE
           | SOME(#"#") => POUND
           | SOME(#".") => PERIOD
           | SOME(#"(") => LPAREN
           | SOME(#")") => RPAREN
           | SOME(#"{") => LACCO
           | SOME(#"}") => RACCO
           | SOME(#",") => COMMA
           | SOME(#":") => COLON
           | SOME(#"+") => PLUS
           | SOME(#"<") => if try(":") then SUBTYPE else raise Tokenization
           | SOME(#"-") => if try(">") then SARROW else MINUS
           | SOME(#"^") => AND
           | SOME(#"=") => if try(">") then DARROW else EQUALS
           | SOME(#"a") => if try("nd") then AND else CHAR(#"a")
           | SOME(#"e") => if try("nd") then END else CHAR(#"e")
           | SOME(#"i") => if try("n") then IN else CHAR(#"i")
           | SOME(#"f") => if try("n") then FN else
                           if try("alse") then FALSE else
                           CHAR(#"f")
           | SOME(#"l") => if try("et") then LET else CHAR(#"l")
           | SOME(#"t") => if try("rue") then TRUE else
                           if try("op") then TOPTYPE else
                           CHAR(#"t")
           | SOME(#"v") => if try("al") then VAL else CHAR(#"v")
           | SOME(#"I") => if try("nt") then INTTYPE else CHAR(#"I")
           | SOME(#"C") => if try("har") then CHARTYPE else CHAR(#"C")
           | SOME(#"B") => if try("ool") then BOOLTYPE else CHAR(#"B")
           | NONE => EOF
           | SOME(c) => if Char.isSpace c then eatSpaces ()
               else if Char.isDigit c then DIGIT(Char.ord c - Char.ord #"0")
               else CHAR(c),
         previous = fn(token) => backtrackedTokens := token :: !backtrackedTokens}
      end	
  
  fun tokenize input =
    let
      val t = preTokenize input
      
      fun next(t : Tokenizer) = (case #next t ()
       of CHAR c => (case name(#next t (), Char.toString c)
          of (NONE, t1) => t1
           | (SOME t2, t1) =>
             let
               val u = #previous t t2
             in
               t1
             end)
        | DIGIT d => (case number(#next t (), d)
          of (NONE, t1) => t1
           | (SOME t2, t1) =>
             let
               val u = #previous t t2
             in
               t1
             end)
        | SPACES => next t
        | token => token)
      
      and name(CHAR c, s) = name(#next t (), s ^ (Char.toString c))
        | name(SPACES, s) = (NONE, NAME s)
        | name(token, s) = (SOME token, NAME s)
      
      and number(DIGIT d, n) = number(#next t (), d + 10 * n)
        | number(SPACES, n) = (NONE, NUMBER n)
        | number(token, n) = (SOME token, NUMBER n)
    in
      {next = fn() => next t, previous =  #previous t}
    end
end