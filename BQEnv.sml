structure BQEnv :> BQENV = struct
  open Names
  open Types
  
  structure NameMap = RedBlackMapFn
    (struct
       type ord_key = Name
       val compare = Names.compare
     end)
  
  type environment = (PolyType NameMap.map) * (int ref) * NameGenerator;
  
  exception NotInEnvironment of Name;
  
  fun empty() : environment = (NameMap.empty, ref 0, getGenerator "e")
  
  fun bind(gamma : environment) =
        fn(n : Name, s : PolyType) =>
          (NameMap.insert(#1 gamma, n, s), #2 gamma, #3 gamma) : environment
  
  and get(gamma : environment) =
        fn n : Name => case NameMap.find(#1 gamma, n)
          of SOME t => t
           | NONE => raise (NotInEnvironment n)
  
  and fresh(gamma : environment) : string =
        let val whatever = #2 gamma := !(#2 gamma) + 1 in
          "X" ^ Int.toString(!(#2 gamma))
        end
  
  fun ftv(gamma : environment) : Set.set =
        NameMap.foldl
          (fn x => Set.union x)
          Set.empty
          (NameMap.map (fn t : PolyType =>
            Set.difference(
              Set.union(
                Types.ftv (#3 t),
                Constraints.ftv (#2 t)), #1 t)) (#1 gamma))
end