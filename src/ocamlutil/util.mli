(** A bunch of generally useful functions *)

val list_append: ('a list) -> ('a list) -> ('a list) (** tail-recursive append *)

val list_map : ('a -> 'b) -> 'a list -> 'b list (** More efficient map *)

val memoize: ('a, 'b) Hashtbl.t ->
            'a ->
            ('a -> 'b) -> 'b


(** Get the value of an option.  Raises Failure if None *)
val valOf : 'a option -> 'a

(* list_map lifted on option type *)
val list_map_opt : ('a -> 'b) -> 'a list option -> 'b list option

(** This has the semantics of (=) on OCaml 3.07 and earlier.  It can
   handle cyclic values as long as a structure in the cycle has a unique
   name or id in some field that occurs before any fields that have cyclic
   pointers. *)
val equals: 'a -> 'a -> bool

(** [make_counter ()] returns two functions: the first one increments a
 * counter (initialized to 0) and returns its new value; the second one
 * resets the counter to 0. *)
val make_counter : unit -> (unit -> int) * (unit -> unit)

(** Same as {!Arg.parse} except it skips every unknown option, and
 * disables [-help] and [--help]. *)
val parse_argv_skip_unknown : (Arg.key * Arg.spec * Arg.doc) list ->
  Arg.anon_fun -> unit
