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
    final (Some res);
    res
  with e -> begin
    final None;
    raise e
  end

