(* Keep here the globally-visible flags *)
let doCheck= ref false   (* Whether to check CIL *)

let logCalls = ref false (* Whether to produce a log with all the function 
                          * calls made *)

(* A tryFinally function *)
let tryFinally 
    (main: 'a -> 'b) (* The function to run *)
    (final: 'b option -> unit)  (* Something to run at the end *)
    (arg: 'a) : 'b = 
  try
    let res = main arg in
    final (Some res);    (* sm: I think it is a mistake for this 'final' call *)
    res                  (* to be in the scope of the 'try' ... *)
  with e -> begin
    final None;
    raise e
  end

