
type lvalue = unit
type tau = unit

let debug_constraints : bool ref = ref false

let print_constraints : bool ref = ref false

let analyze_mono : bool ref = ref false

let solve_online : bool ref = ref false

let solve_constraints () = ()

let rvalue x = x

let deref x = x

let join x y = x

let join_inits x = ()

let address x = ()

let instantiate x y = ()

let assign x y = ()

let apply x y = ()

let make_function x y z = ()

let make_lvalue x y = ()

let bottom x = ()

let return x y = ()

let make_fresh x = ()

let points_to x = []

