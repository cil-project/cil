(** Print extra debugging info *)
val debug : bool ref

(** Debug constraints (print all constraints) *)
val debug_constraints : bool ref

(** Debug smart alias queries *)
val debug_aliases : bool ref

val smart_aliases : bool ref

(** Print out the top level constraints *)
val print_constraints : bool ref

(** Make the analysis monomorphic *)
val analyze_mono : bool ref

(** Disable subtyping *)
val no_sub : bool ref

(** Make the flow step a no-op *)
val no_flow : bool ref 

(** Show the progress of the flow step *)
val show_progress : bool ref 

(** Analyze a file *)
val analyze_file : Cil.file -> unit

(** Print the type of each lvalue in the program *)
val print_types : unit -> unit

(** Compute points to sets. If true is passed, print the sets. *)
val compute_results : bool -> unit

(** Compute alias relationships. If true is passed, print all alias pairs. *)
val compute_aliases : bool -> unit

(** Compute alias frequncy *)
val compute_alias_frequency : unit -> unit
