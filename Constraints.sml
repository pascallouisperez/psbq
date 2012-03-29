structure Constraints :> CONSTRAINTS = struct
  open Types
  
  exception Argument
  
  type substitution = (string * MonoType) list
  
  fun equals c1 = fn c2 => case (c1, c2)
   of (AndConstraint(c1, c2), AndConstraint(d1, d2)) =>
        (equals c1 d1) andalso (equals c2 d2)
    | (SubtypingConstraint(t1, t2), SubtypingConstraint(u1, u2)) =>
        (Types.equals t1 u1) andalso (Types.equals t2 u2)
    | (EqualityConstraint(t1, t2), EqualityConstraint(u1, u2)) =>
        (Types.equals t1 u1) andalso (Types.equals t2 u2)
    | (Boolean b1, Boolean b2) =>
        if b1 then b2 else not b2
    | _ => false
  
  val filterTaut = fn c => case c
        of Boolean(b) => not b
         | SubtypingConstraint(t1, t2) => not (Types.equals t1 t2)
         | _ => true
  
  val splitSubtyping = fn c => case c
    of SubtypingConstraint(FunType(t1, t2), FunType(u1, u2)) =>
         [EqualityConstraint(t1, u1), SubtypingConstraint(t2, u2)]
     | SubtypingConstraint(t as _, u as BaseType(b)) =>
         [EqualityConstraint(t, u)]
     | _ => [c]
  
  fun simplify(c : Constraint) : Constraint =
    let
      val constraints = asList c
    in
      fromList (List.filter filterTaut (asList c))
      (*fromList (List.concat (List.map splitSubtyping (List.filter filterTrue (asList c))))*)
    end
  
  and fromList(l : Constraint list) : Constraint = case l
   of [] => Boolean true
    | x :: xs => List.foldr (fn(c1, c2) => AndConstraint(c1, c2)) x xs
  
  and asList(c : Constraint) : Constraint list = case c
   of AndConstraint(c1, c2) => (asList c1) @ (asList c2)
    | _ => [c]
  
  and ws(c : Constraint) : bool =
        case (unify (weak c))
          of SOME _ => true
           | NONE => false
  
  and dc(c : Constraint) : bool =
        let
          fun tuplize([], buf) = buf
            | tuplize(SubtypingConstraint(t1, t2) :: l, buf) =
                tuplize(l, (t1, t2) :: buf)
            | tuplize(EqualityConstraint(t1, t2) :: l, buf) =
                tuplize(l, (t1, t2) :: (t2, t1) :: buf)
            | tuplize(_ :: l, buf) = raise Argument
          
          val l = asList c
          
          fun dcAllRules ds = dcRec (dcFun (dcJoin (dcDual (dcSymm ds))))
          (*fun dcAllRules ds = dcJoin ds*)
          
          fun dcConverge (ds : Distances.distances) =
                let
                  val dsNew = dcAllRules ds
                in
                  if Distances.equals(dsNew, ds) then dsNew
                  else dcConverge dsNew
                end
          
          val tuples = tuplize(List.filter (fn c => case c of Boolean _ => false | _ => true) l, [])
          
          (* bootstrapping - DC-AX rule *)
          val distances = dcConverge (dcAx tuples)
          
          (*val distances = dcAx tuples
          val _ = print ((Distances.toString distances) ^ "\n\n")
          val distances = dcFun distances
          val _ = print ((Distances.toString distances) ^ "\n\n")
          val distances = dcJoin distances
          val _ = print ((Distances.toString distances) ^ "\n\n")
          val distances = dcRec distances
          val _ = print ((Distances.toString distances) ^ "\n\n")*)
        in
          if List.exists (fn x => case c of Boolean b => not b | _ => false) l then
            false
          else
            dcCheck distances
        end
  
  and dcAx (tuples : (MonoType * MonoType) list) : Distances.distances =
        List.foldl (fn((t1, t2), ds) => Distances.addUp ds (t1, t2, 1)) Distances.empty tuples
  
  and dcSymm (ds : Distances.distances) : Distances.distances =
        Distances.map
        {up = fn ds => fn (t1,t2,n) =>
               if n mod 2 = 1 then
                 Distances.updateDown ds (t2, t1, n)
               else
                 ds,
         down = fn ds => fn (t1,t2,n) =>
               if n mod 2 = 1 then
                 Distances.updateUp ds (t2, t1, n)
               else
                 ds} ds
  
  and dcDual (ds : Distances.distances) : Distances.distances =
        Distances.map
        {up = fn ds => fn (t1,t2,n) => Distances.updateDown ds (t1, t2, n + 1),
         down = fn ds => fn (t1,t2,n) => Distances.updateUp ds (t1, t2, n + 1)} ds
  
  and dcJoin (ds : Distances.distances) : Distances.distances =
        let
          fun combine (n, m) = if n = 0 then m else if m = 0 then n else m + n - 1
          fun join((t1, t2, t3), ds) =
            let
              val n_up = Distances.up ds (t1, t2)
              val m_up = if n_up mod 2 = 0 then Distances.down ds (t2, t3) else Distances.up ds (t2, t3)
              
              val n_down = Distances.down ds (t1, t2)
              val m_down = if n_down mod 2 = 0 then Distances.up ds (t2, t3) else Distances.down ds (t2, t3)
              
              val distances_up =
                    if n_up = 0 orelse m_up = 0 then ds
                    else Distances.updateUp ds (t1, t3, combine (n_up, m_up))
              val distances_down =
                    Distances.updateDown distances_up (t1, t3, combine (n_down, m_down))
            in
              distances_down
            end
          val add = fn(s, t1, t2, _) => MSet.add (MSet.add (s, t1), t2)
          val types = MSet.listItems (Distances.fold {up = add, down = add} MSet.empty ds)
          val triples : (MonoType * MonoType * MonoType) list =
            List.concat
              (List.concat
                (List.map (fn t1 =>
                  (List.map (fn t2 =>
                    (List.map (fn t3 =>
                      (t1, t2, t3)) types)) types)) types))
        in
          List.foldl join ds triples
        end
  
  and dcFun (ds : Distances.distances) : Distances.distances =
        Distances.map
        {up = fn ds => fn (t1,t2,n) => case (t1,t2)
          of (FunType (a1,r1), FunType (a2, r2)) =>
              if n = 1 then
                 Distances.updateUp
                   (Distances.updateDown
                     (Distances.updateUp ds (a1, a2, 0))
                   (a1, a2, 0))
                 (r1, r2, n)
              else
                ds
           | _ => ds,
         down = fn ds => fn (t1,t2,n) => case (t1,t2)
          of (FunType (a1,r1), FunType (a2, r2)) =>
              if n = 1 orelse n = 2 then
                Distances.updateDown
                  (Distances.updateDown
                    (Distances.updateUp ds (a1, a2, 0))
                  (a1, a2, 0))
                (r1, r2, n)
              else
                ds
           | _ => ds} ds
  
  and dcRec (ds : Distances.distances) : Distances.distances =
        Distances.map
        {up = fn ds => fn (t1,t2,n) => case (t1, t2)
          of (RecordType m_down, RecordType m_up) =>
            let
              val labels = StringMap.intersectWithi (fn(_, t1, t2) => (t1, t2)) (m_down, m_up)
            in
              StringMap.foldl (fn((t1, t2), tmpDs) => Distances.updateUp tmpDs (t1, t2, n)) ds labels
              (*if StringMap.numItems labels = (StringMap.numItems m_up) then
                StringMap.foldl (fn((t1, t2), tmpDs) => Distances.updateUp tmpDs (t1, t2, n)) ds labels
              else
                (* indicating an inconsistency *)
                Distances.updateUp ds (BaseType(Bool),BaseType(Int), 0)*)
            end
           | _ => ds,
         down = fn ds => fn (t1,t2,n) => case (t1, t2)
          of (RecordType m_down, RecordType m_up) =>
            let
              val labels = StringMap.intersectWithi (fn(_, t1, t2) => (t1, t2)) (m_down, m_up)
            in
              StringMap.foldl (fn((t1, t2), ds) => Distances.updateDown ds (t1, t2, n)) ds labels
              (*if StringMap.numItems labels = (StringMap.numItems m_down) then
                StringMap.foldl (fn((t1, t2), ds) => Distances.updateDown ds (t1, t2, n)) ds labels
              else
                (* indicating an inconsistency *)
                Distances.updateUp ds (BaseType(Bool),BaseType(Int), 0)*)
            end
           | _ => ds} ds
  
  and dcCheck (ds : Distances.distances) : bool =
        let
          fun f d = fn(b, t1, t2, distance) =>
            if b andalso Set.isEmpty (Types.ftv t1) andalso Set.isEmpty (Types.ftv t2) then
              Int.<=(d(t1, t2), distance)
            else
              b
        in
          Distances.fold {up = f upD, down = f downD} true ds
        end
  
  and groundTypes l = List.foldl ground MSet.empty l
  
  and decomposeTypes l = List.foldl ground (MSet.addList (MSet.empty, l)) l
  
  and ground(t, s) =
        if Set.isEmpty (Types.ftv t) then MSet.add (s, t)
        else case t
          of FunType(t1, t2) => ground (t2, ground (t1, s))
           | RecordType map => StringMap.foldl ground s map
           | _ => s
  
  and upD(TopType, TopType) = 0
    | upD(_, TopType) = 1
    | upD(BaseType b1, BaseType b2) = if b1 = b2 then 0 else 2
    | upD(FunType (t1, t2), FunType (u1, u2)) =
        if Types.equals t1 u1 then upD(t2, u2) else 2
    | upD(t1 as RecordType m1, t2 as RecordType m2) =
        if Types.equals t1 t2 then 0
        else
          let
            val labels = StringMap.intersectWithi (fn(_, t1, t2) => (t1, t2)) (m1, m2)
          in
            if StringMap.numItems labels = (StringMap.numItems m2) then
              StringMap.foldl (fn((t1, t2), d) => Int.max(d, upD(t1, t2))) 1 labels
            else
              2
          end
    | upD(_, _) = 2
  
  and downD(TopType, TopType) = 0
    | downD(TopType, _) = 1
    | downD(_, TopType) = 2
    | downD(BaseType b1, BaseType b2) = if b1 = b2 then 0 else 3
    | downD(FunType (t1, t2), FunType (u1, u2)) =
        if Types.equals t1 u1 then downD(t2, u2) else 3
    | downD(t1 as RecordType m1, t2 as RecordType m2) =
        if Types.equals t1 t2 then 0
        else
          let
            val labels = StringMap.intersectWithi (fn(_, t1, t2) => (t1, t2)) (m1, m2)
          in
            if StringMap.numItems labels = (StringMap.numItems m1) then
              StringMap.foldl (fn((t1, t2), d) => Int.max(d, downD(t1, t2))) 1 labels
            else
              if StringMap.isEmpty labels then 2
              else Int.max(2, StringMap.foldl (fn((t1, t2), d) => Int.max(d, downD(t1, t2))) 0 labels)
          end
    | downD(_, _) = 3
  
  and ftv(SubtypingConstraint(t1, t2)) : Set.set = Set.union(Types.ftv t1, Types.ftv t2)
    | ftv(EqualityConstraint(t1, t2)) : Set.set = Set.union(Types.ftv t1, Types.ftv t2)
    | ftv(AndConstraint(c1, c2)) : Set.set =
        Set.union(ftv c1, ftv c2)
    | ftv(ExistsConstraint(vars, c)) : Set.set =
        List.foldl (fn(x, curr) => Set.delete(curr, x)) (ftv c) vars
    | ftv(_) = Set.empty
  
  and unify(c : Constraint) : substitution option = case c
   of EqualityConstraint(t1, t2) => unify_mono(t1, t2)
    (*| AndConstraint(c1, c2) => (case (unify c1)
      of SOME s1 => (case (unify (subst_constraint s1 c2))
        of SOME s2 => SOME (s1 @ s2)
         | NONE => NONE)
       | NONE => NONE)*)
    | AndConstraint(c1, c2) => (case (unify c1, unify c2)
        of (SOME s1, SOME s2) => merge (s1, s2)
         | _ => NONE)
    | Boolean b => if b then SOME [] else NONE
    | _ => NONE
  
  and merge (s1 : substitution, s2 : substitution) : substitution option =
    let
      val names1 = List.map (fn (name, tipe) => name) s1
      val names2 = List.map (fn (name, tipe) => name) s2
      val (_, namesOnlyIn1) =
        List.partition (fn name1 => List.exists (fn name2 => name1 = name2) names2) names1
      val allNames = namesOnlyIn1 @ names2
    in
      List.foldl
        (fn ((name, otype), olist) => case (otype, olist)
          of (_, NONE) => NONE
           | (NONE, _) => NONE
           | (SOME t, SOME l) => SOME ((name, t) :: l))
        (SOME [])
        (List.map
          (fn name => case (List.find (fn (name1, _) => name = name1) s1, List.find (fn (name2, _) => name = name2) s2)
            of (SOME (_, t1), SOME (_, t2)) => (name, merge_type (t1, t2))
             | (SOME (_, t1), NONE) => (name, SOME t1)
             | (NONE, SOME (_, t2)) => (name, SOME t2)
             | _ => (name, NONE)) (* by construction should never happen *)
          allNames)
    end
  
  and merge_type(t1 : MonoType, t2 : MonoType) : MonoType option =
    case (t1, t2)
      of (VarType n1, VarType n2) => if n1 = n2 then SOME (VarType n1) else NONE
       | (VarType n1, t2) => SOME t2
       | (StarType, StarType) => SOME StarType
       | (FunType(a1, b1), FunType(a2, b2)) =>
           (case (merge_type (a1, a2), merge_type(b1, b2))
             of (SOME a, SOME b) => SOME (FunType(a, b))
              | _ => NONE)
       | (RecordType m1, RecordType m2) =>
           let
             val mergedmaps : MonoType option StringMap.map = StringMap.unionWith
               (fn (t1, t2) => case (t1, t2)
                 of (SOME t1, SOME t2) => merge_type (t1, t2)
                  | (SOME t1, NONE) => SOME t1
                  | (NONE, SOME t2) => SOME t2
                  | (NONE, NONE) => NONE)
               (StringMap.map (fn t => SOME t) m1, StringMap.map (fn t => SOME t) m2)
             val omap : MonoType StringMap.map option = StringMap.foldli
               (fn (label, otype, omap) => case (otype, omap)
                 of (_, NONE) => NONE
                  | (NONE, _) => NONE
                  | (SOME t, SOME m) => SOME (StringMap.insert (m, label, t)))
               (SOME StringMap.empty)
               mergedmaps
           in
             case omap of SOME m => SOME (RecordType m) | NONE => NONE
           end
       | _ => NONE
  
  and weak(SubtypingConstraint(t1, t2)) : Constraint =
        EqualityConstraint (starify t1, starify t2)
	| weak(AndConstraint(c1, c2)) =
	    AndConstraint (weak c1, weak c2)
    | weak(c as _) = c
  
  and starify(TopType) = StarType
    | starify(BaseType(_)) = StarType
    | starify(StarType) = StarType
    | starify(t as VarType(_)) = t
    | starify(FunType(t1, t2)) = FunType(starify t1, starify t2)
    | starify(RecordType labeledTypes) =
        RecordType (StringMap.map starify labeledTypes)
end