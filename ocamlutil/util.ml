(** Utility functions for Coolaid *)
module E = Errormsg

exception GotSignal of int

let withTimeout (secs: float) (* Seconds for timeout *)
                (handler: int -> 'b) (* What to do if we have a timeout. The 
                                        * argument passed is the signal number 
                                        * received. *)
                (f: 'a -> 'b) (* The function to run *)
                (arg: 'a) (* And its argument *)
   : 'b = 
  let oldHandler = 
    Sys.signal Sys.sigalrm 
      (Sys.Signal_handle 
         (fun i -> 
           ignore (E.log "Got signal %d\n" i);
           raise (GotSignal i)))
  in
  let reset_sigalrm () = 
    ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_value = 0.0;
                                              Unix.it_interval = 0.0;});
    Sys.set_signal Sys.sigalrm oldHandler;
  in
  ignore (Unix.setitimer Unix.ITIMER_REAL 
              { Unix.it_value    = secs;
                Unix.it_interval = 0.0;});
  (* ignore (Unix.alarm 2); *)
  try
    let res = f arg in 
    reset_sigalrm ();
    res
  with exc -> begin
    reset_sigalrm ();
    ignore (E.log "Got an exception\n");
    match exc with 
      GotSignal i -> 
        handler i
    | _ -> raise exc
  end
