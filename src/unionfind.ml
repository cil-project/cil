(*
 * Union-Find Functor (no side effects, purely applicative)
 *
 * (used by physical type equality to keep equivalence classes of types)
 *) 

module type UF = 
  sig
    type t
    type elt 
    val empty : t
    val check_equal : t -> elt -> elt -> bool 
    val make_equal : t -> elt -> elt -> t
    val eq_classes : t -> elt list list
  end

module Make(S : Set.S) =
  struct
    type t = S.t list
    type elt = S.elt 
    let empty = []
    let check_equal uf e1 e2 =
      List.fold_left (fun acc elt -> acc ||
        (S.mem e1 elt && S.mem e2 elt)) false uf
    let make_equal (uf : S.t list) e1 e2 =
      let uninvolved = ref [] in
      let s1 = ref (S.singleton e1) in
      let s2 = ref (S.singleton e2) in 
      List.iter (fun (s : S.t) ->
        if S.mem e1 s then 
          s1 := s
        else if S.mem e2 s then
          s2 := s
        else 
          uninvolved := s :: !uninvolved
      ) uf ;
      let merged_set = S.union !s1 !s2 in
      let final_list_of_sets = merged_set :: !uninvolved in
      (final_list_of_sets : S.t list)
    let eq_classes uf = 
      List.map (fun eqclass -> S.elements eqclass) uf
  end


    
  
