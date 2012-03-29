signature BQENV = sig
  type environment;
  
  exception NotInEnvironment of Names.Name
  
  val empty : unit -> environment
  
  val bind : environment -> (Names.Name * Types.PolyType) -> environment
  
  val get : environment -> Names.Name -> Types.PolyType
  
  val fresh : environment -> string
  
  val ftv : environment -> Set.set
end