structure Distances :> DISTANCES = struct
  open Types
  
  structure MMap = BinaryMapFn (MonoTypeOrd)
  
  type distances = (int MMap.map MMap.map) * (int MMap.map MMap.map)
  
  val max = 3
  
  val empty = (MMap.empty, MMap.empty)
  
  fun get m1 (t1, t2) = case MMap.find (m1, t1)
    of SOME m2 => MMap.find (m2, t2)
     | _ => NONE
  
  fun normalize opt = case opt of SOME d => d | NONE => max
  
  fun set m1 (t1, t2, d) =
    let
      val m2 = case MMap.find (m1, t1) of SOME m2 => m2 | _ => MMap.empty
    in
      MMap.insert (m1, t1, (MMap.insert (m2, t2, d)))
    end
  
  fun range d = if Int.<=(0,d) andalso Int.<=(d,max) then d else Int.max(0, Int.min(3, d))
  
  fun getUp (ds : distances) (t1, t2) : int option = get (#1 ds) (t1, t2)
  
  fun getDown (ds : distances) (t1, t2) : int option = get (#2 ds) (t1, t2)
  
  fun up (ds : distances) (t1, t2) = normalize (getUp ds (t1, t2))
  
  fun down (ds : distances) (t1, t2) = normalize (getDown ds (t1, t2))
  
  fun addUp (ds : distances) (t1, t2, d) = (set (#1 ds) (t1, t2, range d), #2 ds)
  
  fun addDown (ds : distances) (t1, t2, d) = (#1 ds, set (#2 ds) (t1, t2, range d))
  
  fun updateUp (ds : distances) (t1, t2, d) = addUp ds (t1,t2,range (Int.min(d,up ds (t1, t2))))
  
  fun updateDown (ds : distances) (t1, t2, d) = addDown ds (t1,t2,range (Int.min(d,down ds (t1, t2))))
  
  fun fold {up = u, down = d} initial (ds : distances) =
        let
          fun folder f initial m1 = MMap.foldli
            (fn(t1, m2, tmp1) =>
              MMap.foldli (fn(t2, d, tmp2) => f (tmp2, t1, t2, d)) tmp1 m2)
            initial
            m1
        in
         folder d (folder u initial (#1 ds)) (#2 ds)
        end
  
  fun map {up = u, down = d} (ds : distances) =
        fold {up = fn(ds, t1, t2, dP) => u ds (t1, t2, dP),
              down = fn(ds, t1, t2, dM) => d ds (t1, t2, dM)} ds ds
  
  fun domain (ds : distances) =
        let
          val add = fn(s, t1, t2, _) => MSet.add (MSet.add (s, t1), t2)
          val types = MSet.listItems (fold {up = add, down = add} MSet.empty ds)
        in
          List.concat
            (List.map (fn t1 =>
               (List.map (fn t2 => (t1, t2)) types)) types)
        end
  
  fun equals(ds1 : distances, ds2 : distances) =
        let
          fun mapEq (test : 'a * 'a -> bool) (m1 : 'a MMap.map, m2 : 'a MMap.map) =
            if MMap.numItems m1 = (MMap.numItems m2) then
              MMap.foldli (fn(k,v1,result) => result andalso case MMap.find (m2, k)
                of SOME v2 => test (v1, v2)
                 | NONE => false) true m1
            else false
          
          val mapMapEquality = mapEq (mapEq (fn(d1:int, d2:int) => d1 = d2))
          
          (*fun check f ds = fn(b, t1, t2, d) => b andalso f ds (t1, t2) = d*)
        in
          (mapMapEquality (#1 ds1, #1 ds2)) andalso (mapMapEquality (#2 ds1, #2 ds2))
          (*(fold {up = check up ds1, down = check down ds1} true ds2)*)
          (*(fold {up = check up ds1, down = check down ds1} true ds2) andalso
          (fold {up = check up ds2, down = check down ds2} true ds1)*)
        end
  
  fun toString ds =
    let
      fun spacer s = if String.size s = 0 then s else ","
    in
      (fn s => if String.size s = 0 then "(empty)" else s) (fold {up = fn(s, t1, t2, d) =>
          s ^ (spacer s) ^ "d+(" ^ Types.toString_mono t1 ^ ","
          ^ Types.toString_mono t2 ^ ")=" ^ Int.toString d,
        down = fn(s, t1, t2, d) =>
          s ^ (spacer s) ^ "d-(" ^ Types.toString_mono t1 ^ ","
          ^ Types.toString_mono t2 ^ ")=" ^ Int.toString d} "" ds)
    end
end