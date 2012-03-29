structure Inference :> INFERENCE = struct
  open BQEnv
  open Expressions
  open Constraints
  open Types
  
  exception TypeError of string
  
  fun infer(e : Expressions.Expression) : Types.PolyType = inferp(empty (), e)
  
  and inferp(gamma : BQEnv.environment, e : Expressions.Expression): Types.PolyType = (case e
   of Constant(c) =>
        let val t = case c
          of Expressions.Int _ => BaseType(Int)
           | Expressions.Bool _ => BaseType(Bool)
           | Expressions.Char _ => BaseType(Char)
           | Plus => FunType(BaseType(Int), FunType(BaseType(Int), BaseType(Int)))
           | Minus => FunType(BaseType(Int), FunType(BaseType(Int), BaseType(Int)))
       in
         (Set.empty, Boolean(true), t)
       end
    | Variable(n) => get gamma n
    | _ => let
          val (d, t) = inferm(empty (), e)
          val c = simplify d
        in
          if ws c then
            if dc c then
              (Set.difference(Set.union(Types.ftv t, Constraints.ftv c), BQEnv.ftv gamma), c, t)
            else
              raise TypeError (Types.toString_constraint c ^ " is not distance consistent")
          else
            raise TypeError (Types.toString_constraint c ^ " is not weakly satisfiable")
        end)
  
  and inferm(gamma : BQEnv.environment, e : Expressions.Expression): Types.Constraint * Types.MonoType = (case e
   of Variable(x) =>
        let
          val (xs, c, t) = get gamma x
          val xsList : Types.substitution = List.map (fn x => (x, VarType (fresh gamma))) (Set.listItems xs)
        in
          (Types.subst_constraint xsList c, Types.subst_mono xsList t)
        end
    | Constant(c) => let val (_, c, t) = inferp(gamma, e) in (c, t) end
    | Abstraction(x, e) =>
        let
          val X = (Set.empty, Boolean(true), VarType (fresh gamma))
          val (c, t) = inferm(bind gamma (x, X), e)
        in
          (c, FunType(#3 X, t))
        end
    | Application(e1, e2) =>
        let
          val (c1, t1) = inferm(gamma, e1)
          val (c2, t2) = inferm(gamma, e2)
        in
          case t1
            of FunType(t1a, t1r) => 
               (AndConstraint(AndConstraint(c1, c2),SubtypingConstraint(t2, t1a)), t1r)
             | _ =>
               let
                 val x1 = VarType (fresh gamma)
                 val x2 = VarType (fresh gamma)
               in
                 (AndConstraint(
                   AndConstraint(c1, c2),
                   AndConstraint(
                     SubtypingConstraint(t1, FunType(x1, x2)),
                     SubtypingConstraint(t2, x1))), x2)
               end
        end
    | Record(labeledExpressions) =>
        let
          val subDerivations : (string * Types.Constraint * Types.MonoType) list =
            List.map (fn(l, e) =>
              let
                val (c, t) = inferm(gamma, e)
              in
                (l, c, t)
              end) labeledExpressions
          val labeledTypes =
            List.foldl (fn((l, _, t), map) => StringMap.insert(map, l, t)) StringMap.empty subDerivations
          val constraint : Constraint =
            List.foldl (fn((_, c, _), partialConstraint) =>
              AndConstraint(c, partialConstraint)) (Boolean true) subDerivations
        in
          (constraint, RecordType labeledTypes)
        end
    | Selection(l, e) => (case inferm(gamma, e)
        of (c, RecordType map) => (case StringMap.find(map, l)
             of SOME t => (c, t)
              | NONE => raise TypeError ("cannot select label " ^ l))
         | (c, t) =>
             let
               val x = VarType (fresh gamma)
               val record = RecordType (StringMap.insert (StringMap.empty, l, x))
             in
               (AndConstraint(c, SubtypingConstraint(t, record)), x)
             end)
    | Let(x, e1, e2) =>
        let
          val s = inferp(gamma, e1)
        in
          inferm(bind gamma (x,s), e2)
        end)
end