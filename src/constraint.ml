(* Aman's constraint representation and solver
   Intended for use for array bounds checking elimination
*)

let debug = false
let showsolution = true

(******************************************************************************)
(* Types *)

type limit = 
  | Int of int
  | Sym of string
  | NegInfinity
  | Infinity

type t =
  | LT of  limit * limit
  | LTE of limit * limit

type set = t list

let set_contains = List.mem
let set_fold_right = List.fold_right
let set_fold_left = List.fold_left



(******************************************************************************)
(* Functions *)

let mk a op b =
  match op with
  | "<" -> LT (a,b)
  | ">" -> LT (b,a)
  | "<=" -> LTE (a,b)
  | ">=" -> LTE (b,a)
  | _ -> failwith "Don't know how to make this constraint"

let lim_tostring = function
  | Int n -> string_of_int n
  | Sym  s -> (* "'" ^ *) s
  | NegInfinity -> "-INF"
  | Infinity -> "INF"

let tostring = function 
  | LT (a,b) -> (lim_tostring a) ^ " < " ^ (lim_tostring b)
  | LTE (a,b) -> (lim_tostring a) ^ " <= " ^ (lim_tostring b)


let pr = Printf.printf;

(******************************************************************************)
(* Solvers *)

(* Fringe:
   We maintain a memoized set of theorems that are being worked on or
   are already proved or disproved
*)
type fringe_t = {fval : t; mutable fsolution : bool option}
let fringe : fringe_t list ref = ref []

let fringe_contains (x : t) = 
  List.exists (fun a -> a.fval = x) !fringe

let fringe_find (x : t) =
  List.find (fun a -> a.fval = x) !fringe

let fringe_add (x : t) : fringe_t = 
  let fr = {fval=x; fsolution=None} in
  fringe := fr :: !fringe;
  fr

(* The constraints and all the limit values for a given run of the solver *)
let sconstraints : set ref = ref []
let slimits : limit list ref = ref []

(* Initialize the fringe and the values for one run of the solver *)
let initSolver constraints = 
  fringe := [];
  sconstraints := constraints;
  slimits := [Infinity;NegInfinity];
  let add_to_slimits lim =
    if not (List.memq lim !slimits) then
      match lim with
      | (Sym _) as lim -> slimits := lim :: !slimits
      | (Int _) as lim -> slimits := lim :: !slimits
      | _ -> ()
  in					   
  List.iter (function 
    | LT (a,b) -> add_to_slimits a; add_to_slimits b;
    | LTE (a,b) -> add_to_slimits a; add_to_slimits b;)
    constraints


(* Try to prove a less-than relation *)
let rec rsolveLT (ca : limit) (cb : limit) : bool =
  let lt = (LT (ca,cb)) in
  if fringe_contains lt then match fringe_find lt with
  | {fsolution = None} as fr -> fr.fsolution <- Some false; false
  | {fsolution = Some sol}   -> sol
  else 
    let fr = fringe_add lt in
    let sol =
      match ca, cb with
      | NegInfinity, _ -> cb != NegInfinity
      |	_, NegInfinity -> false
      |	_, Infinity -> ca != Infinity
      |	Infinity, _ -> false
      |	Int ia, Int ib -> ia < ib
      |	_,_ ->
	  set_contains lt !sconstraints ||
	  List.fold_left
	    (fun acc lim -> 
	      acc ||
	      ((rsolveLT ca lim) && (rsolveLTE lim cb)) ||
	      ((rsolveLTE ca lim) && (rsolveLT lim cb)))
	    false
	    !slimits
    in
    fr.fsolution <- Some sol;
    if debug then Printf.printf "Solve %s = %b\n"  (tostring lt) sol;
    sol
    

(* Try to prove an equal-to relation *)
and rsolveEQ (ca : limit) (cb : limit) : bool =
  (rsolveLTE ca cb &&
  rsolveLTE cb ca)

(* Try to prove a less-than-or-equal relation *)
and rsolveLTE (ca : limit) (cb : limit) : bool =
  let lt = (LTE (ca,cb)) in
  if fringe_contains lt then match fringe_find lt with
  | {fsolution = None} as fr -> fr.fsolution <- Some false; false
  | {fsolution = Some sol}   -> sol
  else 
    let fr = fringe_add lt in
    let sol =
      match ca, cb with
      | NegInfinity, _ -> cb != NegInfinity
      |	_, NegInfinity -> false
      |	_, Infinity -> ca != Infinity
      |	Infinity, _ -> false
      |	Int ia, Int ib -> ia <= ib
      |	_,_ ->
	  set_contains lt !sconstraints ||
	  rsolveLT ca cb ||
	  ca = cb ||
	  List.fold_left
	    (fun acc lim -> 
	      acc ||
	      ((rsolveEQ ca lim) && (rsolveLTE lim cb)) ||
	      ((rsolveEQ cb lim) && (rsolveLTE ca lim)))
	    false
	    !slimits
    in
    fr.fsolution <- Some sol;
    if debug then Printf.printf "Solve %s = %b\n"  (tostring lt) sol;
    sol
  


(* Invoke the solver. Try to prove a theorem with the given constraints *)
let solve constraints theorem =
  initSolver constraints;
  let ret =
    match theorem with
    | LT  (a,b) -> rsolveLT  a b
    | LTE (a,b) -> rsolveLTE a b
  in
  if debug || showsolution then begin 
    Printf.printf "(solve) %s \t= %b\n" (tostring theorem) ret;
    flush stdout
  end;
  ret

(* Solve a == b *)
let solveEQ constraints a b =
  initSolver constraints;
  let ret =
    (rsolveLTE a b) && (rsolveLTE b a)
  in
  if debug || showsolution then begin
    Printf.printf "(solve) %s == %s \t= %b\n" (lim_tostring a) (lim_tostring b) ret;
    flush stdout
  end;
  ret
	  


(******************************************************************************)
(* Test *)

let test () =
  let constraints = [
    mk (Sym "b") "<" (Int 10);
    mk (Sym "b") ">" (Int 8);
    mk (Sym "a") ">" (Int 3);
    mk (Sym "a") "<" (Int 7);
    mk (Sym "x") "<=" (Sym "a");
    mk (Sym "x") ">=" (Sym "a");
    mk (Sym "x") "<=" (Sym "y");
    mk (Sym "x") ">=" (Sym "y");
  ] in
  pr "Constraints:\n";
  List.iter (fun c -> pr "%s\n" (tostring c)) constraints;
  flush stdout;
  ignore (solve constraints (mk (Sym "a") "<" (Int 8)));
  ignore (solve constraints (mk (Sym "b") ">=" (Int 8)));
  ignore (solve constraints (mk (Sym "a") "<" (Sym "b")));
  ignore (solve constraints (mk (Sym "a") "<=" (Sym "b")));
  ignore (solve constraints (mk (Sym "a") ">" (Sym "b")));
  ignore (solve constraints (mk (Sym "a") ">=" (Sym "b")));
  ignore (solveEQ constraints (Sym "a") (Sym "y"));
  pr "s/a/y/g\n";
  ignore (solve constraints (mk (Sym "y") "<" (Int 8)));
  ignore (solve constraints (mk (Sym "b") ">=" (Int 8)));
  ignore (solve constraints (mk (Sym "y") "<" (Sym "b")));
  ignore (solve constraints (mk (Sym "y") "<=" (Sym "b")));
  ignore (solve constraints (mk (Sym "y") ">" (Sym "b")));
  ignore (solve constraints (mk (Sym "y") ">=" (Sym "b")));
  ignore (solveEQ constraints (Sym "y") (Sym "y"));
  ()
    

;; test () ;; 
