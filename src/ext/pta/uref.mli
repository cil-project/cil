type 'a uref
  
(** Union-find with union by rank and path compression 
  
  This is an implementation of Tarjan's union-find data structure using 
  generics. The interface is analagous to standard references, with the
  addition of a union operation which makes two references indistinguishable.
  
*)
  
val uref: 'a -> 'a uref 
  (** Create a new uref *)
  
val equal: 'a uref * 'a uref -> bool
  (** Test whether two urefs share the same equivalence class *)
  
val deref: 'a uref -> 'a
  (** Extract the contents of this reference *)
  
val update: 'a uref * 'a -> unit
  (** Update the value stored in this reference *)
  
val unify: ('a * 'a -> 'a) -> 'a uref * 'a uref -> unit
  (** [unify f (p,q)] unifies references [p] and [q], making them 
    indistinguishable. The contents of the reference are the result of
    [f] *)
    
val union: 'a uref * 'a uref -> unit
  (** [unify (p,q)] unifies references [p] and [q], making them
    indistinguishable. The contents of the reference are the contents of
    one of the first or second arguments (unspecified) *)
