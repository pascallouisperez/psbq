signature CONSTRAINTS = sig
  type substitution
  
  exception Argument
  
  val equals : Types.Constraint -> Types.Constraint -> bool
  
  val simplify : Types.Constraint -> Types.Constraint
  
  val asList : Types.Constraint -> Types.Constraint list
  
  val fromList : Types.Constraint list -> Types.Constraint
  
  (* weak satifiability *)
  val ws : Types.Constraint -> bool
  
  (* distance consistency *)
  val dc : Types.Constraint -> bool
  
  (* distances *)
  val upD : Types.MonoType * Types.MonoType -> int
  val downD : Types.MonoType * Types.MonoType -> int
  
  val unify : Types.Constraint -> substitution option
  
  val ftv : Types.Constraint -> Set.set
  
  val groundTypes : Types.MonoType list -> Types.MSet.set
  
  val decomposeTypes : Types.MonoType list -> Types.MSet.set
end