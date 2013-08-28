(** Whether to use the performance counters (on Pentium only) *)
type timerModeEnum =
  | Disabled      (** Do not collect timing information *)
  | SoftwareTimer (** Use OCaml's [Unix.time] for timing information *)

let timerMode = ref Disabled

(* Flag for counting number of calls *)
let countCalls = ref false

                                        (* A hierarchy of timings *)

type t = { name : string;
           mutable time : float; (* In seconds *)
	   mutable ncalls : int;
           mutable sub  : t list}

                                        (* Create the top level *)
let top = { name = "TOTAL";
            time = 0.0;
	    ncalls = 0;
            sub  = []; }

                                        (* The stack of current path through 
                                         * the hierarchy. The first is the 
                                         * leaf. *)
let current : t list ref = ref [top]

exception NoPerfCount
let reset (mode: timerModeEnum) : unit = 
  top.sub <- [];
  timerMode := mode



let print chn msg = 
  (* Total up *)
  top.time <- List.fold_left (fun sum f -> sum +. f.time) 0.0 top.sub;
  let rec prTree ind node = 
	(Printf.fprintf chn "%s%-25s      %6.3f s" 
        (String.make ind ' ') node.name node.time);
    begin
      if node.ncalls <= 0 then
	output_string chn "\n" 
      else if node.ncalls = 1 then
	output_string chn "  (1 call)\n"
      else
	(Printf.fprintf chn "  (%d calls)\n" node.ncalls)
    end;
    List.iter (prTree (ind + 2)) (List.rev node.sub)
  in
  Printf.fprintf chn "%s" msg; 
  List.iter (prTree 0) [ top ];
  Printf.fprintf chn "Timing used\n";
  let gc = Gc.quick_stat () in 
  let printM (w: float) : string = 
    let coeff = float_of_int (Sys.word_size / 8) in
    Printf.sprintf "%.2fMB" (w *. coeff /. 1000000.0)
  in
  Printf.fprintf chn 
    "Memory statistics: total=%s, max=%s, minor=%s, major=%s, promoted=%s\n    minor collections=%d  major collections=%d compactions=%d\n"
    (printM (gc.Gc.minor_words +. gc.Gc.major_words 
               -. gc.Gc.promoted_words))
    (printM (float_of_int gc.Gc.top_heap_words))
    (printM gc.Gc.minor_words)
    (printM gc.Gc.major_words)
    (printM gc.Gc.promoted_words)
    gc.Gc.minor_collections
    gc.Gc.major_collections
    gc.Gc.compactions;
    
  ()
        
  

(* Get the current time, in seconds *)
let get_current_time () : float = 
  (Unix.times ()).Unix.tms_utime

let repeattime limit str f arg = 
                                        (* Find the right stat *)
  let stat : t = 
    let curr = match !current with h :: _ -> h | [] -> assert false in
    let rec loop = function
        h :: _ when h.name = str -> h
      | _ :: rest -> loop rest
      | [] -> 
          let nw = {name = str; time = 0.0; ncalls = 0; sub = []} in
          curr.sub <- nw :: curr.sub;
          nw
    in
    loop curr.sub
  in
  let oldcurrent = !current in
  current := stat :: oldcurrent;
  let start = get_current_time () in
  let rec repeatf count = 
    let finish diff =
      (* count each call to repeattime once *)
      if !countCalls then stat.ncalls <- stat.ncalls + 1;
      stat.time <- stat.time +. (diff /. float(count));
      current := oldcurrent;                (* Pop the current stat *)
      ()
    in
    let res   =
      try f arg
      with e ->
	let diff = get_current_time () -. start in
	finish diff;
	raise e
    in
    let diff = get_current_time () -. start in
    if diff < limit then
      repeatf (count + 1)
    else begin
      finish diff;
      res                                   (* Return the function result *)
    end
  in
  repeatf 1


let time str f arg =
  if !timerMode = Disabled then
    f arg
  else
    repeattime 0.0 str f arg
    

let lastTime = ref 0.0
let timethis (f: 'a -> 'b) (arg: 'a) : 'b = 
  let start = get_current_time () in
  let res = f arg in 
  lastTime := get_current_time () -. start; 
  res
  
(** Return the cumulative time of all calls to {!Stats.time} and
  {!Stats.repeattime} with the given label. *)
(* Usually there will be only one occurence in the tree, but summing them all
   makes more sense than choosing one arbitrarily *)
let lookupTime (label:string) : float =
  let time : float ref = ref 0.0 in
  let rec search (x:t) : unit = 
    if x.name = label then time := !time +. x.time;
    List.iter search x.sub
  in
  search top;
  !time











