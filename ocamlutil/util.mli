(** A bunch of generally useful functions *)

val list_append: ('a list) -> ('a list) -> ('a list) (** tail-recursive append *)

val list_map : ('a -> 'b) -> 'a list -> 'b list (** More efficient map *)

val memoize: ('a, 'b) Hashtbl.t ->
            'a ->
            ('a -> 'b) -> 'b


(** Get the value of an option.  Raises Failure if None *)
val valOf : 'a option -> 'a

(** This has the semantics of (=) on OCaml 3.07 and earlier.  It can
   handle cyclic values as long as a structure in the cycle has a unique
   name or id in some field that occurs before any fields that have cyclic
   pointers. *)
val equals: 'a -> 'a -> bool
