structure Bq = struct
  open Types
  open Parser
  open TokenizerFactory
  open Inference
  open BQEnv
  
  val pretty = fn m => fn a => if (String.size m = 0) then a else m
  
  fun i(expression : string) : unit =
    let
      val e = parse(tokenize expression)
      val tipe = Expressions.toString e ^ " : " ^ toString_poly(infer e)
        handle TypeError m => pretty m "type error"
             | NotInEnvironment n => "unbound variable " ^ Names.toString n
             | ParseException m => pretty m "parse error"
             | _ => "error"
    in
      print(tipe ^ "\n")
    end
  
  fun dc(constraint : string) : unit =
    let
      val c = parseConstraint(tokenize constraint)
    in
      if Constraints.dc c then
        print ("dc|-  " ^ (toString_constraint c) ^ "\n")
      else
        print ("dc|/- " ^ (toString_constraint c) ^ "\n")
    end
  
  fun ws(constraint : string) : unit =
    let
      val c = parseConstraint(tokenize constraint)
    in
      if Constraints.ws c then
        print ("ws|-  " ^ (toString_constraint c) ^ "\n")
      else
        print ("ws|/- " ^ (toString_constraint c) ^ "\n")
    end
end
