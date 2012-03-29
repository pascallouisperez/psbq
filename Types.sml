structure Types = struct
  open Expressions
  open Names
  
  (* Types and Constraints *)
  datatype BType = Int | Char | Bool
  and MonoType = TopType
    | StarType
    | BaseType of BType
    | VarType of string
    | RecordType of MonoType StringMap.map
    | FunType of MonoType * MonoType
  and Constraint = Boolean of bool
    | SubtypingConstraint of MonoType * MonoType
    | EqualityConstraint of MonoType * MonoType
    | AndConstraint of Constraint * Constraint
    | ExistsConstraint of string list * Constraint;
  
  type PolyType = Set.set * Constraint * MonoType;
  
  type binding = string * MonoType;
  
  type substitution = binding list;
  
  exception IllegalArgument
  
  structure MonoTypeOrd : ORD_KEY = struct
    type ord_key = MonoType
    
    (* total ordering among 'same class' types
       ---------------------------------------
       Int < Bool < Char
       X < Y <=> String.compare
       T1 -> T2 < U1 -> U2 <=> (T1, T2) < (U1, U2) using lexicographc ordering
       {L1} < {L2} <=> |L1| < |L2| or
                       |L1| > |L2|
                       |L1| = |L2| and
                         labels in L1 are lexicographically smaller than L2's or
                         L1's types are lexicographically smaller thant L2's
       
       total ordering between types 'classes'
       --------------------------------------
       Top < Star < Base < Var < Fun < Rec
     *)
    fun compare(t1, t2) = case (t1, t2)
      of (TopType, TopType) => EQUAL
       | (TopType, _) => LESS
       | (StarType, StarType) => EQUAL
       | (StarType, TopType) => GREATER
       | (StarType, _) => LESS
       | (BaseType Int, BaseType Int) => EQUAL
       | (BaseType Int, BaseType _) => LESS
       | (BaseType Bool, BaseType Int) => GREATER
       | (BaseType Bool, BaseType Bool) => EQUAL
       | (BaseType Bool, BaseType _) => LESS
       | (BaseType Char, BaseType Char) => EQUAL
       | (BaseType _, BaseType _) => GREATER
       | (BaseType _, TopType) => GREATER
       | (BaseType _, StarType) => GREATER
       | (BaseType _, _) => LESS
       | (VarType s1, VarType s2) => String.compare (s1, s2)
       | (VarType _, TopType) => GREATER
       | (VarType _, StarType) => GREATER
       | (VarType _, BaseType _) => GREATER
       | (VarType _, _) => LESS
       | (FunType (t1, t2), FunType (u1, u2)) =>
           (case (compare (t1, u1), compare (t2, u2))
             of (EQUAL, c) => c
              | (c, _) => c)
       | (FunType _, TopType) => GREATER
       | (FunType _, StarType) => GREATER
       | (FunType _, BaseType _) => GREATER
       | (FunType _, VarType _) => GREATER
       | (FunType _, _) => LESS
       | (RecordType m1, RecordType m2) =>
           if Int.<(StringMap.numItems m1, StringMap.numItems m2) then LESS
           else if Int.>(StringMap.numItems m1, StringMap.numItems m2) then GREATER
           else
             let
               fun lexicographic comparator = fn (v1, v2, c) =>
                 case c of EQUAL => comparator (v1, v2) | _ => c
               val list1 = StringMap.listItemsi m1
               val list2 = StringMap.listItemsi m2
               val labels = ListPair.foldlEq
                 (lexicographic String.compare)
                 EQUAL
                 (List.map (fn(k,v)=>k) list1, List.map (fn(k,v)=>k) list2)
             in
               case labels
                 of EQUAL =>
                     ListPair.foldlEq
                     (lexicographic compare)
                     EQUAL
                     (List.map (fn(k,v)=>v) list1, List.map (fn(k,v)=>v) list2)
                  | _ => labels
             end
       | (RecordType _, _) => GREATER
  end
  
  structure MSet = BinarySetFn (MonoTypeOrd)
  
  fun equals(t1 : MonoType) = fn t2 : MonoType => case (t1, t2)
      of (TopType, TopType) => true
       | (StarType, StarType) => true
       | (BaseType b1, BaseType b2) => b1 = b2
       | (VarType s1, VarType s2) => s1 = s2
       | (RecordType map1, RecordType map2) =>
           let
             val intersection = StringMap.intersectWith
               (fn (t1, t2) => equals t1 t2)
               (map1, map2)
             val count = StringMap.numItems intersection
           in
             (StringMap.numItems map1 = count) andalso
             (StringMap.numItems map2 = count) andalso
             (StringMap.foldl (fn (b1, b2) => b1 andalso b2) true intersection)
           end
       | (FunType (t1a, t1r), FunType (t2a, t2r)) =>
           (equals t1a t2a) andalso (equals t1r t2r)
       | (_, _) => false
  
  (* substitution *)
  and subst_mono(s : substitution) = fn(t1 : MonoType) => (case t1
    of VarType(a) => (case List.find (fn(e) => a = #1 e) s
      of SOME t2 => #2 t2
      | NONE => t1)
    | FunType(t1a, t1r) => FunType(subst_mono s t1a, subst_mono s t1r)
    | _ => t1)
  and subst_constraint(s : substitution) = fn(c : Constraint) => (case c
   of SubtypingConstraint(t1, t2) => SubtypingConstraint(subst_mono s t1, subst_mono s t2)
    | EqualityConstraint(t1, t2) => EqualityConstraint(subst_mono s t1, subst_mono s t2)
    | AndConstraint(c1, c2) => AndConstraint(subst_constraint s c1, subst_constraint s c2)
    | _ => c)
  
  (* unification *)
  and unify_mono(VarType(a), t2 as VarType(b)) =
        if (a = b) then SOME [] else SOME [(a, t2)]
    | unify_mono(VarType(a), t2) =
        if Set.member((ftv t2), a) then NONE else SOME [(a, t2)]
    | unify_mono(t1, t2 as VarType(b)) = unify_mono(t2, t1)
    | unify_mono(FunType(t1a, t1r), FunType(t2a, t2r)) = (case unify_mono(t1a, t2a)
        of NONE => NONE
         | SOME s1 => (case unify_mono(subst_mono s1 t1r, subst_mono s1 t2r)
           of NONE => NONE
            | SOME s2 => SOME (s1 @ s2)))
    | unify_mono(RecordType map1, RecordType map2) =
        let
          val intersection = StringMap.intersectWith (fn x => x) (map1, map2)
          val iNum = StringMap.numItems intersection
          val map1Num = StringMap.numItems map1
          val map2Num = StringMap.numItems map2
        in
          if (Int.<=(iNum, map1Num) andalso (iNum = map2Num)) then
            StringMap.foldl
              (fn((t1, t2), s) =>
                case s of NONE => NONE | SOME s1 =>
                  case unify_mono (subst_mono s1 t1, subst_mono s1 t2)
                    of NONE => NONE
                     | SOME s2 => SOME (s1 @ s2))
              (SOME [])
              intersection
          else
            NONE
        end
    | unify_mono(t1, t2) =
        if equals t1 t2 then SOME [] else NONE
  
  (* ftv *)
  and ftv(VarType(v)) : Set.set = Set.singleton v
    | ftv(FunType(t1, t2)) : Set.set = Set.union(ftv t1, ftv t2)
    | ftv(RecordType map) =
        StringMap.foldl (fn(x, b) => Set.union(b, ftv x)) Set.empty map
    | ftv(_) : Set.set = Set.empty
  
  (* toString *)
  and toString_b(Int) = "Int"
    | toString_b(Char) = "Char"
    | toString_b(Bool) = "Bool"
  and toString_mono(TopType) = "top"
    | toString_mono(StarType) = "*"
    | toString_mono(BaseType(a)) = toString_b(a)
    | toString_mono(VarType(name)) = name
    | toString_mono(RecordType labels) = "{" ^ (toString_l (StringMap.listItemsi labels)) ^ "}"
    | toString_mono(FunType(d as FunType(_, _), r)) = "(" ^ toString_mono(d) ^ ") -> " ^ toString_mono(r)
    | toString_mono(FunType(d, r)) = toString_mono(d) ^ " -> " ^ toString_mono(r)
  and toString_poly(x, c, t) =
        "\\" ^ toString_t (Set.listItems x) ^
        "|" ^ (toString_constraint c) ^ "." ^ (toString_mono t)
  and toString_constraint(Boolean(b)) = Bool.toString(b)
    | toString_constraint(SubtypingConstraint(t1, t2)) = toString_mono(t1) ^ " <: " ^ toString_mono(t2)
    | toString_constraint(EqualityConstraint(t1, t2)) = toString_mono(t1) ^ " = " ^ toString_mono(t2)
    | toString_constraint(AndConstraint(c1 as ExistsConstraint(_, _), c2)) =
        "(" ^ toString_constraint(c1) ^ ") ^ " ^ toString_constraint(c2)
    | toString_constraint(AndConstraint(c1, c2 as ExistsConstraint(_, _))) =
        toString_constraint(c1) ^ " ^ (" ^ toString_constraint(c2) ^ ")"
    | toString_constraint(AndConstraint(c1, c2)) =
        toString_constraint(c1) ^ " ^ " ^ toString_constraint(c2)
    | toString_constraint(ExistsConstraint(ns, c)) =
        "E{not implemented}." ^ toString_constraint(c)
  
  and toString_l([]) = ""
    | toString_l([(label, tipe)]) = label ^ ":" ^ (toString_mono tipe)
    | toString_l((label, tipe) :: rest) = label ^ ":" ^ (toString_mono tipe) ^ "," ^ (toString_l rest)
  
  and toString_t([]) = "0"
    | toString_t([n]) = n
    | toString_t(n :: rest) = n ^ "," ^ (toString_t rest)
end;