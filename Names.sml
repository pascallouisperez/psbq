signature NAMES = sig
  (* #1 : domain
     #2 : scope number
     #3 : id number
     #4 : name *)
  type Name = string * int * int * string;
  
  type NameGenerator =
         {newScope : unit -> unit,
          previousScope : unit -> unit,
          create : string -> Name};
  
  exception IncomparableDomains;

  val getGenerator : string -> NameGenerator;
  
  val toString : Name -> string;

  val < : (Name * Name) -> bool;

  val <= : (Name * Name) -> bool;

  val > : (Name * Name) -> bool;

  val >= : (Name * Name) -> bool;
  
  val compare : (Name * Name) -> order
end

structure Names :> NAMES = struct
  type Name = string * int * int * string;
  
  type NameGenerator =
         {newScope : unit -> unit,
          previousScope : unit -> unit,
          create : string -> Name};
  
  exception IncomparableDomains;
  
  val generators : (string * NameGenerator) list ref = ref nil;
  
  fun getGenerator(domain) = case (List.find
        (fn(t:string * NameGenerator) => #1 t = domain) (!generators))
        of SOME(t:string * NameGenerator) => #2 t
        | NONE =>
          let
            val scopes = ref [ref 0];
            val names : Name list ref list ref = ref [ref []];
            
            fun nextId() =
                let val t = List.hd(!scopes) := !(List.hd(!scopes)) + 1; in
                  !(List.hd(!scopes))
                end;
            
            fun newScopeImpl() =
                let val t = scopes := ref 0 :: !scopes; in
                  names := ref [] :: !names
                end;
            
            fun previousScopeImpl() = if (List.length(!scopes) > 1)
                then let val t = scopes := List.drop(!scopes, 1); in
                  names := List.drop(!names, 1)
                end
                  else raise Empty;

            fun createImpl(n : string) : Name =
                case (List.find (fn(name: Name) => #4 name = n) (!(List.hd(!names))))
                  of SOME(name) => name
                  | NONE =>
                    let
                      val name = (domain, List.length(!scopes), nextId(), n);
                      val t = List.hd(!names) := name :: !(List.hd(!names));
                    in name end;
          in
            {newScope = newScopeImpl,
             previousScope = previousScopeImpl,
             create = createImpl}
          end;
  
  fun toString(n : Name) = #4 n
  (*fun toString(n : Name) = "(" ^ Int.toString (#2 n) ^ "," ^ Int.toString (#3 n) ^ "," ^ #4 n ^ ")"*)
  
  fun compare(c, n1:Name, n2:Name) =
      if (#1 n1 = #1 n2) then
        if (#2 n1 = #2 n2) then c(#3 n1, #3 n2) else c(#2 n1, #2 n2)
      else raise IncomparableDomains;

  infix 3 < fun op <(n1:Name, n2:Name) = compare(Int.<, n1, n2);

  infix 3 <= fun op <=(n1:Name, n2:Name) = compare(Int.<=, n1, n2);

  infix 3 > fun op >(n1:Name, n2:Name) = compare(Int.>, n1, n2);

  infix 3 >= fun op >=(n1:Name, n2:Name) = compare(Int.>=, n1, n2);
  
  fun compare(n1, n2) =
        if (n1 < n2) then
          LESS
        else
          if (n1 = n2) then
            EQUAL
          else
            GREATER
end