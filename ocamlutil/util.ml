(** Utility functions for Coolaid *)
module E = Errormsg
module H = Hashtbl

open Pretty

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

(** Print a hash table *)
let docHash (one: 'a -> 'b -> doc) () (h: ('a, 'b) H.t) = 
  let theDoc = ref nil in
  (H.fold 
     (fun key data acc -> acc ++ one key data)
     h
     align) ++ unalign
    


let hash_to_list (h: ('a, 'b) H.t) : ('a * 'b) list =
  H.fold
    (fun key data acc -> (key, data) :: acc)
    h
    []

let keys (h: ('a, 'b) H.t) : 'a list =
  H.fold
    (fun key data acc -> key :: acc)
    h
    []

let hash_copy_into (hfrom: ('a, 'b) H.t) (hto: ('a, 'b) H.t) : unit = 
  H.clear hto;
  H.iter (H.add hto) hfrom

let anticompare a b = compare b a
;;


let rec list_drop (n : int) (xs : 'a list) : 'a list =
  if n < 0 then invalid_arg "Util.list_drop";
  if n = 0 then 
    xs
  else begin 
    match xs with
    | [] -> invalid_arg "Util.list_drop"
    | y::ys -> list_drop (n-1) ys
  end


let rec list_span (p : 'a -> bool) (xs : 'a list) : 'a list * 'a list = 
  begin match xs with
  | [] -> ([],[])
  | x::xs' -> 
      if p x then
        let (ys,zs) = list_span p xs' in (x::ys,zs)
      else ([],xs)
  end
;;

let rec list_rev_append revxs ys =
  begin match revxs with
  | [] -> ys
  | x::xs -> list_rev_append xs (x::ys)
  end
;;
let list_insert_by (cmp : 'a -> 'a -> int) 
    (x : 'a) (xs : 'a list) : 'a list =
  let rec helper revhs ts =
    begin match ts with
    | [] -> List.rev (x::revhs)
    | t::ts' -> 
        if cmp x t >= 0 then helper (t::revhs) ts'
        else list_rev_append (x::revhs) ts
    end
  in
  helper [] xs
;;

let list_head_default (d : 'a) (xs : 'a list) : 'a =
  begin match xs with
  | [] -> d
  | x::_ -> x
  end
;;

let rec list_iter3 f xs ys zs =
  begin match xs, ys, zs with
  | [], [], [] -> ()
  | x::xs, y::ys, z::zs -> f x y z; list_iter3 f xs ys zs
  | _ -> invalid_arg "Util.list_iter3"
  end
;;
  
let rec get_some_option_list (xs : 'a option list) : 'a list =
  begin match xs with
  | [] -> []
  | None::xs  -> get_some_option_list xs
  | Some x::xs -> x :: get_some_option_list xs
  end
;;

let list_iteri (f: int -> 'a -> unit) (l: 'a list) : unit = 
  let rec loop (i: int) (l: 'a list) : unit = 
    match l with 
      [] -> ()
    | h :: t -> f i h; loop (i + 1) t
  in
  loop 0 l

let list_mapi (f: int -> 'a -> 'b) (l: 'a list) : 'b list = 
  let rec loop (i: int) (l: 'a list) : 'b list = 
    match l with 
      [] -> []
    | h :: t -> 
	let headres = f i h in
	headres :: loop (i + 1) t
  in
  loop 0 l

let list_fold_lefti (f: 'acc -> int -> 'a -> 'acc) (start: 'acc) 
                   (l: 'a list) : 'acc = 
  let rec loop (i, acc) l = 
    match l with
      [] -> acc
    | h :: t -> loop (i + 1, f acc i h) t
  in
  loop (0, start) l


let list_init (len : int) (init_fun : int -> 'a) : 'a list =
  let rec loop n acc =
    if n < 0 then acc
    else loop (n-1) ((init_fun n)::acc)
  in
  loop (len - 1) []
;;


(** Generates the range of integers starting with a and ending with b *)
let int_range_list (a: int) (b: int) = 
  list_init (b - a + 1) (fun i -> a + i)


(** Some handling of registers *)
type 'a growArrayFill =
    Elem of 'a
  | Susp of (int -> 'a)

type 'a growArray = {
            gaFill: 'a growArrayFill;
            (** Stuff to use to fill in the array as it grows *)
    mutable gaData: 'a array;
  } 

let growTheArray (ga: 'a growArray) (toidx: int) (why: string) : unit = 
  let len = Array.length ga.gaData in
  if toidx >= len then begin
(*
    ignore (E.log "growing an array to idx=%d (%s)\n" toidx why);
*)
    let data' = begin match ga.gaFill with
      Elem x ->

	let data'' = Array.create (toidx + 1) x in
	Array.blit ga.gaData 0 data'' 0 len;
	data''
    | Susp f -> Array.init (toidx + 1)
	  (fun i -> if i < len then ga.gaData.(i) else f i)
    end
    in
    ga.gaData <- data'
  end

let getReg (ga: 'a growArray) (r: int) : 'a = 
  growTheArray ga r "get";
  ga.gaData.(r)

let setReg (ga: 'a growArray) (r: int) (what: 'a) : unit = 
  growTheArray ga r "set";
  ga.gaData.(r) <- what

let newGrowArray (initsz: int) (fill: 'a growArrayFill) : 'a growArray = 
  { gaFill = fill;
    gaData = begin match fill with
      Elem x -> Array.create initsz x
    | Susp f -> Array.init initsz f
    end; }

let copyGrowArray (ga: 'a growArray) : 'a growArray = 
  { ga with gaData = Array.copy ga.gaData } 

let hasPrefix (prefix: string) (what: string) : bool = 
  let pl = String.length prefix in
  try String.sub what 0 pl = prefix 
  with Invalid_argument _ -> false



let restoreRef (r: 'a ref) : (unit -> unit) = 
  let old = !r in
  (fun () -> r := old)

let restoreHash (h: ('a, 'b) H.t) : (unit -> unit) = 
  let old = H.copy h in
  (fun () -> 
    hash_copy_into old h)

let restoreArray (a: 'a array) : (unit -> unit) = 
  let old = Array.copy a in
  (fun () -> Array.blit old 0 a 0 (Array.length a))

let runThunks (l: (unit -> unit) list) : (unit -> unit) = 
  fun () -> List.iter (fun f -> f ()) l



(* Memoize *)
let memoize (h: ('a, 'b) Hashtbl.t) 
            (arg: 'a) 
            (f: 'a -> 'b) : 'b = 
  try
    Hashtbl.find h arg
  with Not_found -> begin
    let res = f arg in
    Hashtbl.add h arg res;
    res
  end

(* Just another name for memoize *)
let findOrAdd h arg f = memoize h arg f

(* A tryFinally function *)
let tryFinally 
    (main: 'a -> 'b) (* The function to run *)
    (final: 'b option -> unit)  (* Something to run at the end *)
    (arg: 'a) : 'b = 
  try
    let res: 'b = main arg in
    final (Some res);
    res
  with e -> begin
    final None;
    raise e
  end

