

type cfgInfo = {
    start   : int;          
    size    : int;    
    successors    : int list array; (* the successor function *)
    predecessors  : int list array; (* the prdecessor function *)
    blocks: block array;
    nrRegs: int
  } 

and block = reg list * statement list
  
and statement = reg list * reg list

and reg = int


and idomInfo = int array  (* immediate dominator *)

and dfInfo = (int list) array (* dominance frontier *)

val compute_idom: cfgInfo -> idomInfo
val dominance_frontier: cfgInfo -> dfInfo
val add_ssa_info: cfgInfo -> unit
