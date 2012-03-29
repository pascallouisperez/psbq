structure Expressions = struct
    datatype AtomicConstant = Int of int
      | Bool of bool
      | Char of char
      | Plus
      | Minus;
    
    datatype Expression = Constant of AtomicConstant
      | Variable of Names.Name
      | Abstraction of Names.Name * Expression
      | Application of Expression * Expression
      | Selection of string * Expression
      | Record of (string * Expression) list
      | Let of Names.Name * Expression * Expression;
    
    fun toString_cvs([]) = ""
      | toString_cvs([x]) = x
      | toString_cvs(x :: xs) = x ^ ", " ^ toString_cvs(xs);
    
    fun toString(Constant(Int(i))) = Int.toString(i)
      | toString(Constant(Bool(b))) = Bool.toString(b)
      | toString(Constant(Char(c))) = "'" ^ Char.toString(c) ^ "'"
      | toString(Constant(Plus)) = "+"
      | toString(Constant(Minus)) = "-"
      | toString(Variable(n : Names.Name)) = Names.toString(n)
      | toString(Abstraction(n : Names.Name, e)) = "fn " ^ Names.toString(n) ^ " => " ^ toString(e)
      | toString(Application(e1 as Abstraction(_, _), e2 as Application(_, _))) =
          "(" ^ toString(e1) ^ ") (" ^ toString(e2) ^ ")"
      | toString(Application(e1 as Abstraction(_, _), e2 as Abstraction(_, _))) =
          "(" ^ toString(e1) ^ ") (" ^ toString(e2) ^ ")"
      | toString(Application(e1 as Application(_, _), e2 as Abstraction(_, _))) =
          "(" ^ toString(e1) ^ ") (" ^ toString(e2) ^ ")"
      | toString(Application(e1 as Application(_, _), e2 as Application(_, _))) =
          "(" ^ toString(e1) ^ ") (" ^ toString(e2) ^ ")"
      | toString(Application(e1 as Abstraction(_, _), e2)) = "(" ^ toString(e1) ^ ") " ^ toString(e2)
      | toString(Application(e1, e2 as Application(_, _))) = toString(e1) ^ " (" ^ toString(e2) ^ ")"
      | toString(Application(e1, e2)) = toString(e1) ^ " " ^ toString(e2)
      | toString(Record(l)) =
          "{" ^ toString_cvs (List.map (fn(le) => #1 le ^ "=" ^ toString(#2 le)) l) ^ "}"
      | toString(Selection(l : string, e as Application(_, _))) =
          "#" ^ l ^ " (" ^ toString(e) ^ ")"
      | toString(Selection(l, e)) = "#" ^ l ^ " " ^ toString(e)
      | toString(Let(n, e1 as Let(_, _, _), e2)) =
          "let " ^ Names.toString n ^  " = (" ^ toString e1 ^ ") in " ^ toString e2 ^ " end"
      | toString(Let(n, e1, e2)) =
          "let " ^ Names.toString n ^  " = " ^ toString e1 ^ " in " ^ toString e2 ^ " end"
end
