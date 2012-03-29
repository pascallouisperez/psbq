signature DISTANCES = sig
  type distances
  
  val empty : distances
  
  val map : {up:distances -> Types.MonoType * Types.MonoType * int -> distances,
             down:distances -> Types.MonoType * Types.MonoType * int -> distances}
              -> distances
              -> distances
  
  val fold : {up:'a * Types.MonoType * Types.MonoType * int -> 'a,
             down:'a * Types.MonoType * Types.MonoType * int -> 'a}
             -> 'a
             -> distances
             -> 'a
  
  val up : distances -> Types.MonoType * Types.MonoType -> int
  
  val down : distances -> Types.MonoType * Types.MonoType -> int
  
  val addUp : distances -> Types.MonoType * Types.MonoType * int -> distances
  
  val addDown : distances -> Types.MonoType * Types.MonoType * int -> distances
  
  val updateUp : distances -> Types.MonoType * Types.MonoType * int -> distances
  
  val updateDown : distances -> Types.MonoType * Types.MonoType * int -> distances
  
  val equals : distances * distances -> bool
  
  val domain : distances -> (Types.MonoType * Types.MonoType) list
  
  val toString : distances -> string
end