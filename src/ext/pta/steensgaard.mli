type lvalue
type tau
val debug : bool ref
val debug_constraints : bool ref
val print_constraints : bool ref
val no_flow : bool ref
val no_sub : bool ref
val analyze_mono : bool ref
val solve_online : bool ref
val solve_constraints : unit -> unit
val rvalue : lvalue -> tau
val deref : tau -> lvalue
val join : tau -> tau -> tau
val join_inits : tau list -> tau
val address : lvalue -> tau
val instantiate : lvalue -> int -> lvalue
val assign : lvalue -> tau -> unit
val apply : tau -> tau list -> tau
val make_function :  string -> lvalue list -> tau -> tau
val make_lvalue : bool -> string -> lvalue
val bottom : unit -> tau
val return : tau -> tau -> unit
val make_fresh : string -> tau
val points_to : lvalue -> string list    
val string_of_lvalue : lvalue -> string
val global_lvalue : lvalue -> bool
val alias_query : bool -> lvalue list -> int * int
