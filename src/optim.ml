(* Written by S. P. Rahul *)

(* Optimizes the placement of boxing checks *)

open Pretty
open Cil
module E=Errormsg

let debug = false

(*-----------------------------------------------------------------*)
let sCount = ref 0   (* to generate consecutive sid's *)
let numNodes = ref 0 (* number of nodes in the CFG *)
let nodeList : stmt list ref = ref [] (* All the nodes in a flat list *) (* ab: Added to change dfs from quadratic to linear *)
let nodes : stmt array ref = ref [| |] (* ordered nodes of the CFG *)
let markedNodes : stmt list ref = ref [] (* Used in DFS for recording visited nodes *) (* ab: not used anymore *)
let dfsMarkWhite = -1 (* Color-marking the vertices during dfs, a la CLR *)
let dfsMarkGray  = -2
let dfsMarkBlack =  0 (* or any greater number *)

let gen : exp list array ref = ref [| |] (* array of exp list, representing GEN sets *)
let kill : bool array ref = ref [| |] (* array of bool, representing KILL sets *)
                     (* see comment in nullChecksOptim why they are bool's *)
let inNode : exp list array ref = ref[| |] (* dataflow info at the entry of a node. Array of flowVal *)
let outNode : exp list array ref = ref[| |] (* dataflow info at the exit of a node. Array of flowVal *)
let allVals : exp list ref = ref [] (* list of all flow values. To be used as TOP in dataflow analysis *)

(* Some statistics that we can use to brag about later *)
let stats_total      = ref 0 (* Total CHECKs seen *) 
let stats_nc_removed = ref 0 (* removed by null check remover *)
let stats_removed    = ref 0 (* removed by the redundancy eliminator *)

(*------------------------------------------------------------*)
(* Notes regarding CFG computation:
   1) Initially only succs and preds are computed. sid's are filled in
      later, in whatever order is suitable (e.g. for forward problems, reverse
      depth-first postorder).
   2) If a stmt (return, break or continue) has no successors, then
      function return must follow.
      No predecessors means it is the start of the function
   3) We use the fact that initially all the succs and preds are assigned []
*)

(* Fill in the CFG info for the stmts in a block
   next = succ of the last stmt in this block
   break = succ of any Break in this block
   cont  = succ of any Continue in this block
   None means the succ is the function return. It does not mean the break/cont
   is invalid. We assume the validity has already been checked.
*)
(* At the end of CFG computation, 
   - numNodes = total number of CFG nodes 
   - length(nodeList) = numNodes
*)
let rec cfgBlock (blk: block) (next:stmt option) (break:stmt option) (cont:stmt option) =
  match blk with
    [] -> ();
  | [s] -> cfgStmt s next break cont
  | hd::tl ->
      cfgStmt hd (Some (List.hd tl))  break cont;
      cfgBlock tl next break cont

(* Fill in the CFG info for a stmt
   Meaning of next, break, cont should be clear from earlier comment
*)
and cfgStmt (s: stmt) (next:stmt option) (break:stmt option) (cont:stmt option) =
  numNodes := !numNodes + 1;
  nodeList := s :: !nodeList; (* Future traversals can be made in linear time. e.g.  *)
  match s.skind with
    Instr _  ->
      (match next with
	None -> ()
      | Some n -> s.succs <- [n]; n.preds <- s::n.preds)
  | Return _  -> ()
  | Goto (p,_) ->
      s.succs <- [!p]; (!p).preds <- s::(!p).preds
  | Break _ ->
      (match break with
        None -> ()
      | Some b -> s.succs <- [b]; b.preds <- s::b.preds)
  | Continue _ ->
      (match cont with
        None -> ()
      | Some c -> s.succs <- [c]; c.preds <- s::c.preds)
  | If (_, blk1, blk2, _) ->
      (* The succs of If is [true branch;false branch] *)
      (match blk2 with
        [] -> (match next with
          None -> ()
        | Some n -> s.succs <- n::s.succs; n.preds <- s::n.preds)
      | (hd::tl) -> s.succs <- hd::s.succs; hd.preds <- s::hd.preds);
      (match blk1 with
        [] -> (match next with
          None -> ()
        | Some n -> s.succs <- n::s.succs; n.preds <- s::n.preds)
      | (hd::tl) -> s.succs <- hd::s.succs; hd.preds <- s::hd.preds);
      cfgBlock blk1 next break cont;
      cfgBlock blk2 next break cont
  | Switch(_,blk,l,_) ->
      s.succs <- l; (* in order *)
      List.iter (function br -> br.preds <- s::br.preds) l;
      cfgBlock blk next next cont
  | Loop(blk,_) ->
      if (blk <> []) then begin
        s.succs <- [List.hd blk];
        (List.hd blk).preds <- s::(List.hd blk).preds
      end;
      cfgBlock blk (Some s) next (Some s)
      (* Since all loops have terminating condition true, we don't put
         any direct successor to stmt following the loop *)


let rec printCfgBlock ?(showGruesomeDetails = true) blk =
  List.iter (function s ->
    printCfgStmt ~showGruesomeDetails:showGruesomeDetails s)
    blk;
  

and printCfgStmt ?(showGruesomeDetails = true) s =
  ignore (printf "-------------------------------------------------\n");
  ignore (printf "Id: %d\n" s.sid);
  ignore (printf "Succs: ");
  List.iter (function s -> ignore (printf "%d " s.sid)) s.succs;
  ignore (printf "\n");
  ignore (printf "Preds: ");
  List.iter (function s -> ignore (printf "%d " s.sid)) s.preds;
  ignore (printf "\n");
  if showGruesomeDetails then begin
    ignore (printf "Gen:\n ");
    List.iter (function e -> ignore (printf "%a\n" d_exp e)) (!gen).(s.sid);
    ignore (printf "\n");
    ignore (printf "Kill: ");
    if (!kill).(s.sid) then ignore (printf "true\n") else ignore(printf "false\n");
    ignore (printf "In:\n ");
    List.iter (function e -> ignore (printf "%a\n" d_exp e)) (!inNode).(s.sid);
    ignore (printf "\n");
    ignore (printf "Out:\n ");
    List.iter (function e -> ignore (printf "%a\n" d_exp e)) (!outNode).(s.sid);
    ignore (printf "\n");
  end;
  match s.skind  with
  | If (test, blk1, blk2, _) ->
      ignore (printf "Cond: %a\n" d_plainexp test);
      printCfgBlock blk1; printCfgBlock blk2
  | Switch(test,blk,_,_) ->
      ignore (printf "Switch: %a\n" d_exp test);
      printCfgBlock blk
  | Loop(blk,_) ->
      ignore (printf "Loop\n");
      printCfgBlock blk
  | _ -> ignore (printf "%a\n" d_stmt s)

(* Assign sid based on reverse depth-first postorder *)
let rec orderBlock blk =
  if (blk <> []) then begin
    dfs (List.hd  blk)
  end
  else ()
(*
and dfs s =
  if not (List.memq s !markedNodes) then begin
    markedNodes := s::!markedNodes;
    List.iter dfs s.succs;
    s.sid <- !sCount;
    !nodes.(!sCount) <- s; ( * add to ordered array * )
    sCount := !sCount - 1;
  end
*)

and dfsMarkAll (color : int) = 
  List.iter (fun s -> s.sid <- color) !nodeList
  
and dfs (start:stmt) =
  assert ((List.length !nodeList) == !numNodes);
  dfsMarkAll dfsMarkWhite;
  dfsLoop start

and dfsLoop (s:stmt) =
  if s.sid == dfsMarkWhite then begin
    s.sid <- dfsMarkGray;
    List.iter dfsLoop s.succs;
    assert (!sCount >= dfsMarkBlack);
    s.sid <- !sCount;
    !nodes.(!sCount) <- s; (* add to ordered array *)
    sCount := !sCount - 1;
  end



(*-----------------------------------------------------------------*)
(* The String module should've contained this basic function *)
let starts_with (s : string) (prefix : string) : bool =
  let len_s = String.length s in
  let len_p = String.length prefix in
  let rec strcmp pos =
    pos >= len_p ||
    (s.[pos] == prefix.[pos] && strcmp (pos+1))
  in
  len_p <= len_s && strcmp 0


(* Decide whether a call is a CHECK_something *)
let isCheckCall_str s = starts_with s "CHECK_"
let isCheckCall_instr : instr -> bool = function
    Call (_,Lval(Var x,_),args,l) when isCheckCall_str x.vname -> true
  | _ -> false    
    

(*-----------------------------------------------------------------*)
(* Print all the CHECK_NULL arguments (without a cast if there is one).
   For debugging purposes *)

let rec printChecksBlock blk =
  List.iter printChecksStmt blk

and printChecksStmt s =
  match s.skind with
    Instr l -> List.iter printChecksInstr l
  | Return _ | Continue _ | Break _ | Goto _ -> ()
  | If (e,blk1,blk2,_) -> printChecksBlock blk1; printChecksBlock blk2;
  | Switch (e,blk,_,_) -> printChecksBlock blk
  | Loop (blk,_) -> printChecksBlock blk

and printChecksInstr i =
  match i with
    Call (_,Lval(Var x,_),args,l) when x.vname = "CHECK_NULL" ->
       (* args must be a list of one element -- the exp which we have
          to ensure is non-null *)
      let arg = List.hd args in
       (* remove the cast if it has one *)
      (match arg with
        CastE(_,e) -> ignore (printf "%a\n\n%a\n" d_exp e d_plainexp e)
      | _ -> ignore (printf "%a\n\n%a\n" d_exp arg d_plainexp arg));
      ignore (printf "------------------------------------\n")
  | _ -> ()

(*-----------------------------------------------------------------*)

(* Find the union of two sets (represented by lists) *)
let rec union l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl ->
      if (List.mem hd l2) then union tl l2
      else hd::union tl l2

and intersect l1 l2 =
  match l1 with
    [] -> []
  | hd::tl ->
      if (List.mem hd l2) then hd::intersect tl l2
      else intersect tl l2

and intersectAll l =
  match l with
    [] -> []
  | [s] -> s
  | hd::tl -> intersect hd (intersectAll tl)

(*-----------------------------------------------------------------*)
(* For accumulating the GEN and KILL of a sequence of instructions *)
let instrGen = ref []
let instrKill = ref false

(* Reads "nodes" and side-effects "gen","kill" and "allVals" *)
let rec createGenKill () =
  allVals := [];
  Array.iteri createGenKillForNode !nodes

and createGenKillForNode i s =
  instrGen := []; instrKill := false;
  match s.skind with
    Instr l ->  createGenKillForInstrList i l (* TODO *)
  | _ -> (!gen).(i) <- []; (!kill).(i) <- false

and createGenKillForInstrList i (l:instr list) =
  (match l with
    [] -> (!gen).(i) <- !instrGen; (!kill).(i) <- !instrKill
  | hd::tl -> let (g,k) = createGenKillForInstr hd in
    if (k) then begin
      instrGen := []; instrKill := true;
    end else begin
      instrGen := union !instrGen g
    end;
    createGenKillForInstrList i tl);
  (!gen).(i) <- !instrGen;
  (!kill).(i) <- !instrKill;
  allVals := union !allVals !instrGen (* update list of possible flow values *)


and createGenKillForInstr i =
  match i with
    Asm _ -> ([],false)
  | Set _ -> ([],true) (* invalidate everything *)
  | Call(_,Lval(Var x,_),args, l) when x.vname = "CHECK_NULL" ->
      (* args is a list of one element *)
      let arg = List.hd args in
      (match arg with
        CastE(_,e) -> ([e],false)
      | _ -> ([arg],false))
  | Call(_,Lval(Var x,_),args, l) (* Don't invalidate set if function is
                                  * something we know behaves well *)
    when  isCheckCall_str x.vname  -> ([],false)
  | Call _ -> ([],true) (* Otherwise invalidate everything *)


(*-----------------------------------------------------------------*)
(* Remove redundant CHECK_NULLs from a list of instr. Essentially a
   basic-block optimization *)
(* nnl (for NotNullList) is a list of expressions guaranteed to be not null
   on entry to l *)
let rec optimInstr (l: instr list) nnl : instr list =
  match l with
    [] -> []
  | first::tl ->
      match first with
        Asm _ -> first::optimInstr tl nnl
      | Set _ -> first::optimInstr tl [] (* Kill everything *)
      | Call(_,Lval(Var x,_),args,l) when x.vname = "CHECK_NULL" ->
      (* args is a list of one element *)
          let arg = List.hd args in
          let checkExp = (match arg with CastE(_,e) -> e | _ -> arg) in
	  stats_total := !stats_total + 1;
          if (List.mem checkExp nnl) then begin
	    stats_nc_removed := !stats_nc_removed + 1;
	    optimInstr tl nnl (* remove redundant check*)
	  end
          else first::optimInstr tl (checkExp::nnl) (* add to the list of valid non-null exp *)
      | Call(_,Lval(Var x,_),args,l)  (* Don't invalidate for well-behaved functions *)
        when ((String.length x.vname) > 6 &&
              (String.sub x.vname 0 6)="CHECK_") -> 
		stats_total := !stats_total + 1;
		first::optimInstr tl nnl
      | Call _ -> (* invalidate everything *)
          first::optimInstr tl []

(*-----------------------------------------------------------------*)
let nullChecksOptim f =
  (* Notes regarding CHECK_NULL optimization:
     1) Argument of CHECK_NULL is an exp. We maintain a list of exp's
        known to be non-null and flow it over the CFG.
     2) In absence of aliasing info, any assignment kills *all* exps's
        known to be non-null. Therefore GEN is a list, but KILL is
        a bool.
  *)
  (* TODO: 1) Use better data structures. Lists are inefficient.
           2) Use node-ordering information combined with worklist
           3) Even basic aliasing information would be useful:
              (a) Program variables cannot alias one another
              (b) Aliased locations must have the same type
           4) Take the predicate in If statements into account
  *)

  if debug then  begin
    ignore (printf "----------------------------------------------\n");
    ignore (printf "Arguments (with casts removed) of all CHECK_NULL arguments\n");
    printChecksBlock f.sbody;
  end;

  (* Order nodes by reverse depth-first postorder *)
  sCount := !numNodes-1;
  nodes := Array.create !numNodes dummyStmt; (* allocate space *)
  orderBlock f.sbody; (* assign sid *)

  (* Create gen and kill for all nodes *)
  gen := Array.create !numNodes [];  (* allocate space *)
  kill := Array.create !numNodes false; (* allocate space *)
  createGenKill ();

  (* Now carry out the actual data flow *)

  (* Set IN = [] for the start node. All other IN's and OUT's are TOP *)
  inNode := Array.create !numNodes !allVals;
  (!inNode).(0) <- [];
  outNode := Array.create !numNodes !allVals;


  let changed = ref true in  (* to detect if fix-point has been reached *)
  while !changed do
    changed := false;
    for i = 0 to (!numNodes-1) do
      let old = (!outNode).(i) in
      let tmpIn = intersectAll (List.map (function s -> (!outNode).(s.sid)) (!nodes).(i).preds) in
      (!inNode).(i) <- tmpIn;
      if (!kill).(i) then
        (!outNode).(i) <- (!gen).(i)
      else
        (!outNode).(i) <- union (!gen).(i) tmpIn;
      if old <> (!outNode).(i) then changed := true
    done
  done;

  if debug then printCfgBlock f.sbody;
  if debug then begin
    ignore (printf "-------------------------------\n");
    ignore (printf "All dataflow values\n");
    List.iter (function e -> ignore (printf "%a\n" d_exp e)) !allVals
  end;

  (* Optimize every block based on the IN and OUT sets *)

  (* For CHECK_NULL optimization, we only have to remove the redundant
     CHECK_NULLS. Therefore, we only optimize stmt with skind Instr (...)
  *)

  Array.iter (function s ->
    match s.skind with
      Instr l -> s.skind <- Instr(optimInstr l (!inNode).(s.sid))
    | _ -> ())
    !nodes;

  f




(*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*)
(*  ,,---.     	       |	      	    | 			     	      *)
(*  ||    |            |	      	    | 			     	      *)
(*  ||    | .---.  .---|  |   |	 .---. 	.---|  .---.  .---.  .---.  .   ,     *)
(*  ||__./  |---'  |   |  |   |	 |   | 	|   |  |   |  |	  |  |	    |   |     *)
(*  ||   \  '---'  `---'  '---'	 '   '  `---'  `---'- '	  '  '---'  `---|     *)
(* 	       	       	       	       	 			    .---+-    *)
(* 	    (Just a visual marker)     	 			    '---'     *)
(* 	       	       	       	       	 			       	      *)


(******************************************************************************)
(* A flag to totally disable this part of the optimizer *)
let useRedundancyElimination = true

(* Turn debugging on. This also produces some commentary in the #line dir'ves *)
let amandebug = try (Sys.getenv "REDDBG") = "1" with _ -> false

(* Dont actually remove the redundant CHECKs. Just keep them marked *)
let amanDontRemove = try (Sys.getenv "REDDONTREMOVE") = "1" with _ -> false

(* Be extremely non-conservative about assignments and func-calls *)
let amanNonConservative = 
  try (Sys.getenv "REDNONCONSERVATIVE") = "1" with _ -> false

(* Skip the global init which is often very long and slows down my 
   super-quadratic algo *)
let amanSkipGlobInit =
  try (Sys.getenv "REDSKIPGLOBINIT") = "1" with _ -> false


(******************************************************************************)
(* Lattice values *)
type latticeType =
    Unknown 
  | NotNeeded 
  | Live of varinfo   (* Its sufficient to just have one temp. variable store
			 the live value of the CHECK, for non-void CHECK 
			 functions. 
			 The assumption here is that these temp. variables are
			 not assigned any other value later 
			 Another assumption is that the return value of CHECKs 
			 is only stored in named unoffsetted variables
		      *)			 
  | Needed 

(* I dont want to rely on the numbers that the compiler assigns to the types. *)
type lattice     = {latWeight: int ; 
		      latType: latticeType}

(* Lattice values start from "unknown" and keep creeping towards "needed" *)
let wtUNKNOWN   = 0
let wtNOTNEEDED = 10
let wtLIVE      = 20
let wrNEEDED    = 30

(* Constructors *)
let latUnknown         = {latWeight = wtUNKNOWN;   latType = Unknown}
let latCheckNotNeeded  = {latWeight = wtNOTNEEDED; latType = NotNeeded}
let latLive v          = {latWeight = wtLIVE;      latType = Live v}
let latCheckNeeded     = {latWeight = wrNEEDED;    latType = Needed}

(* Instead of relying on the polymorphic min/max, I want to define the relations
   precisely myself.
   Both these functions return v1 if the arguments are equal.

   We handle the comparision of Live specially. *)

let lat_max v1 v2 =
  if v1.latWeight = wtLIVE && v2.latWeight = wtLIVE 
  then (if v1 = v2 then v1 else latCheckNeeded)
  else if v1.latWeight >= v2.latWeight then v1 else v2

let lat_min v1 v2 = 
  if v1.latWeight = wtLIVE && v2.latWeight = wtLIVE 
  then (if v1 = v2 then v1 else latUnknown)
  else if v1.latWeight <= v2.latWeight then v1 else v2


(* Just some stuff good for debugging *)
let getLatValDescription = function
    Unknown ->  "?"
  | NotNeeded -> "X"
  | Live var -> "X-" ^ var.vname
  | Needed  -> "N"

(* Just some stuff good for debugging *)
let printLatVal n = Printf.printf " %s" (getLatValDescription n)



(******************************************************************************)
(* Misc. utility functions *)

(* Produce debugging output with a REDDBG: prefix *)
let pr s = flush stdout;Printf.printf  "REDDBG: "; Printf.printf s

let doMarshal v : unit =
  pr "************ MARSHALLING TO op.m *********************\n";
  let chn = open_out_bin "op.m" in
  Marshal.to_channel chn v [Marshal.No_sharing];
  (*
  Marshal.to_channel chn 
    (match !checkInstrs.(i) with Call (_,_,ee,_) -> ee | _ -> [])
    [];
  *)
  close_out chn

(* Used in peephole *)
exception PeepholeRemovable

(* Returns the name of a function in a Call *)
let getCallName = function
    Call (_,Lval (Var f,_),_,_) -> f.vname
  | Call _ -> "<some-function>"
  | _ -> "<non-call>"

(* Returns the outermost offset of an offset *)
let rec getOutermostOffset (off : offset) =
  match off with
    NoOffset
  | Field (_, NoOffset)
  | Index (_, NoOffset) -> off
  | Field (_, off) -> getOutermostOffset off
  | Index (_, off) -> getOutermostOffset off

let getOutermostOffsetLval (_,lvoff) = getOutermostOffset lvoff
  

(* Helper function that strips the outermost offset of an lval
   e.g. a[3].z[3] -> a[3].z -> a[3] -> a -> invalid_arg!
*)
let stripOffset (lv : lval) : lval =
  let rec butlastOffset (off : offset) =
    match off with
      NoOffset -> invalid_arg "butlastOffset (in stripOffset) should not be called on NoOffset"
    |	Index (_, NoOffset)
    |	Field (_, NoOffset) -> NoOffset
    |	Index (e, off2) -> Index (e, butlastOffset off2)
    |	Field (f, off2) -> Field (f, butlastOffset off2)
  in
  match lv with (mv, off) -> (mv, butlastOffset off)

(* Returns -1 if not found *)
let find_in_array (array : 'a array) (element : 'a) : int =  
  let ubound = Array.length array in
  let rec findloop i =
    if i >= ubound then -1 else
    if array.(i) == element then i else findloop (i + 1)
  in
  findloop 0

(* Find a substring *)
let strstr str qry : bool =
  let ql = String.length qry in
  let sl = String.length str in
  let rec loop pos qpos =
    qpos >= ql ||
    (str.[pos] == qry.[qpos] && (loop (pos+1) (qpos+1))) ||
    (pos < (sl-ql) && (loop (pos+1) 0)) 
  in
  ql <= sl && loop 0 0
     
(* compareWithoutCasts:
   Strip the casts and compare the expressions
*)
let rec compareWithoutCasts e1 e2 : bool =
  match e1,e2 with
    CastE (_, e1), _ -> compareWithoutCasts e1 e2
  | _, CastE (_, e2) -> compareWithoutCasts e1 e2
  | _,_ -> e1 = e2


(* Simplify exps in some way *)
let rec simplifyExp e : exp =
  match e with
    Const _ | SizeOf _ | AlignOf _ -> e
  | SizeOfE e -> simplifyExp e
  | AlignOfE e -> simplifyExp e
  | BinOp (op,e1,e2,t) -> BinOp (op, simplifyExp e1, simplifyExp e2, t)
  | UnOp (op,e,t) -> UnOp (op,simplifyExp e,t)
  | Question (e1,e2,e3) -> Question (simplifyExp e1,simplifyExp e2,simplifyExp e3)
  | CastE (_,e) -> simplifyExp e
  | Lval lv -> simplifyLval lv
  | StartOf lv -> simplifyLval lv
  | AddrOf (Mem e, NoOffset) -> 
      BinOp (IndexPI,simplifyExp e,zero,typeOf e)
  | AddrOf ((_,off) as lv) -> 
      (match getOutermostOffset off with 
	Index (i,NoOffset) -> 
	  let lvs = Lval (stripOffset lv) in
	  simplifyExp (BinOp (IndexPI, lvs, i, typeOf lvs))
      | _ -> e)

and simplifyLval lv : exp =
  match (getOutermostOffsetLval lv) with
    Index (i,NoOffset) ->
      let lv = Lval (stripOffset lv) in
      BinOp (IndexPI,lv,i,typeOf lv)
  | _ -> Lval lv


(* Compare exp's for equality *)
let rec expEqual e1 e2 : bool=
  e1 == e2 ||
  match e1,e2 with
    Const a, Const b -> a = b
  | SizeOf a, SizeOf b -> a = b
  | SizeOfE e1, SizeOfE e2 -> expEqual e1 e2
  | AlignOf a, AlignOf b -> a = b
  | AlignOfE e1, AlignOfE e2 -> expEqual e1 e2
  | BinOp (op1,ea1,ea2,t1), BinOp (op2,eb1,eb2,t2) -> 
      op1=op2 && (expEqual ea1 eb1) && (expEqual ea2 eb2) && t1=t2
  | UnOp (op1,ea1,t1), UnOp (op2,eb1,t2) -> 
      op1=op2 && (expEqual ea1 eb1) && t1=t2
  | Question (ea1,ea2,ea3), Question (eb1,eb2,eb3) ->
      (expEqual ea1 eb1) && (expEqual ea2 eb2) && (expEqual ea3 eb3)
  | CastE (t1,e1), CastE (t2,e2) ->t1=t2 && (expEqual e1 e2)
  | CastE (_, e1), _ -> expEqual e2 e1
  | Lval a, Lval b -> lvalEqual a b
  | StartOf _, Lval _ -> expEqual e2 e1
  | AddrOf _, Lval _ -> expEqual e2 e1
  | Lval _, StartOf lv -> expEqual e1 (Lval lv)
  | Lval _, AddrOf (Mem e, NoOffset) -> 
      expEqual e1 e
  | Lval _, AddrOf ((bb,bo) as b) ->
      (match getOutermostOffset bo with 
	Index (zero,NoOffset) -> expEqual e1 (Lval (stripOffset b))
      | _ -> false)
  | AddrOf lv1, AddrOf lv2 -> lvalEqual lv1 lv2
  | AddrOf lv1, StartOf lv2 ->expEqual e1 (Lval lv2)  (* ?? *)
  | StartOf lv1, AddrOf lv2 ->expEqual e2 e1
  | StartOf lv1, StartOf lv2 ->lvalEqual lv1 lv2 
  | _,_ -> e1=e2

(* Compare lvals *)
and lvalEqual lv1 lv2 : bool=
  (* ignore (Pretty.printf "off: %a ==? %a@!" d_lval lv1 d_lval lv2);*)
  lv1 == lv2 ||
  match lv1, lv2 with
    (Var v1, off1), (Var v2, off2) -> v1 = v2 && (offsetEqual off1 off2)
  | (Mem e1, off1), (Mem e2, off2) ->
      (expEqual e1 e2) && (offsetEqual off1 off2)
  | (Mem e1, NoOffset), (Var _, off2) -> 
      (match getOutermostOffset off2 with
	Index (zero, NoOffset) -> expEqual e1 (Lval (stripOffset lv2))
      |	_ -> false)
  | (Var _,_), (Mem _,_) -> lvalEqual lv2 lv1
  | _,_ -> lv1=lv2

and offsetEqual off1 off2 : bool =
  match off1, off2 with
    NoOffset, NoOffset -> true
  | Index (i1,off1), Index (i2,off2) -> (expEqual i1 i2) && (offsetEqual off1 off2)
  | Field (f1,off1), Field (f2,off2) -> f1.fname = f2.fname && (offsetEqual off1 off2)
  | _ -> false


(* Compare exp's for lt / gt / = *)
let compareExp e1 e2 : int option =
  let b =
  if (expEqual e1 e2) then Some 0
  else match e1,e2 with
    Const (CInt32 (a,_,_)), Const (CInt32 (b,_,_)) -> 
      Some (if a < b then -1 else if a=b then 0 else 1)
  | _ -> None
  in
  if amandebug then begin
    pr "";
    ignore (Pretty.printf "compare %a , %a = %s\n" d_exp e1 d_exp e2 (match b with None -> "None" | Some n -> string_of_int n));
  end;
  b

(******************************************************************************)

(* See comments in eliminateRedundancy *)
let checkInstrs : instr array ref = ref [||]
let checkBeingProcessed : instr ref = ref dummyInstr
let checkProcessed : bool array ref = ref [||]
let checkFlags : bool array ref = ref [||]
let checkLvals : lval list ref = ref []
let checkHasAliasedVar : bool ref = ref false
(******************************************************************************)
(* eliminateRedundancy:
   This is the entry-point to the redundancy eliminator.
*)
let rec eliminateRedundancy (f : fundec) : fundec = 
  if useRedundancyElimination then begin

    (* Number the nodes in some way. Doesn't matter for now *)
    let _ = numberNodes () in

     (*if amandebug then  printCfgBlock ~showGruesomeDetails:false f.sbody;*)
     (*flush stdout;*)

    (* The cin values corresponding to the nodes array *)
    let cin = Array.make !numNodes latUnknown in   

    (* The cout values as above *)
    let cout = Array.make !numNodes latUnknown in  
    
    (* An array of all the CHECK instructions. They're all Call's *)
    (* Right now, there is no good way to lookup the index of a known instr *)
    checkInstrs := Array.of_list (filterChecks !nodeList);

    (* The number of CHECK instructions *)
    let countCheckInstrs = Array.length !checkInstrs in
    
    if amandebug then pr "------- Function %s contains %d nodes and %d CHECKS\n" f.svar.vname !numNodes countCheckInstrs;

    (* Show variables *)
    if amandebug then begin
      List.iter (fun v -> pr "Var %d: %s vaddrof:%b\n" v.vid v.vname v.vaddrof ) 
	(f.sformals @ f.slocals);
    end;

    (* When processing a CHECK, I find all the other redundant CHECKs and flag them.
       i.e. All the simultaneously flagged elements are identical, and good 
       candidates for elimination *)
    checkFlags := Array.make countCheckInstrs false;

    (* Am I done? This should end up as an array of true's *)
    checkProcessed := Array.make countCheckInstrs false;

    (* stats_total := !stats_total + countCheckInstrs; *)

    (* Process each CHECK Call  *)
    for i=0 to (countCheckInstrs - 1) do      
      if not (!checkProcessed.(i)) then begin (* Skip the already-processed ones *)
	!checkProcessed.(i) <- true;

	(* Let other functions know the current CHECK *)
	checkBeingProcessed := !checkInstrs.(i);
	
	if amandebug then begin 
	  pr "";	  
	  ignore (printf "Processing %d: %a\n" i d_instr !checkInstrs.(i));

	  (* Some temporary stuff for inspection by a ocamlmktop manufactured toplvl *)
	  
          if i = 4 && false then begin
	    match !checkInstrs.(i) with 
	      Call (_,_,ee,_) -> doMarshal ee 
	    | _ -> ()
          end
	  
	end;

	(* Get a list of all the Lvals that are used in the arguments to
	   the CHECK being processed *)
	checkLvals := getCheckedLvals !checkInstrs.(i);

	(* Look for an aliased variable somewhere in these lvals
	   If one exists, then we'll have to be more conservative *)	  
	checkHasAliasedVar := List.fold_right 
	    (fun lv acc -> acc || (lvalHasAliasedVar lv))
	    !checkLvals
	    false;

	if amandebug then begin
	  pr "Lvals: ";
	  List.iter (fun lv -> ignore (printf "%a " d_lval lv)) !checkLvals;
	  if !checkHasAliasedVar then ignore (printf " (checkHasAliasedVar)");
	  ignore (printf "\n");
	end;

        (* Flag the similar CHECKs. Skip ahead if there is nothing redundant *)
	if (markSimilar i) > 1 then begin
          (* Reset cin, cout *)
	  Array.iteri (fun i _ -> cin.(i) <- latUnknown ; cout.(i) <- latUnknown) cin;

	  (* Mark all the similar ones as "processed" *)
	  Array.iteri (fun i a -> !checkProcessed.(i) <- !checkProcessed.(i) || a) !checkFlags;
	  
	  (* Set the cin for the entry point. We do need a check at this point *)
	  (match f.sbody with
	    h :: t -> cin.(h.sid) <- latCheckNeeded; 
	      if amandebug then pr "Entry node = %d\n" h.sid
	  | _ -> ());
	  
	  (* Find cin,cout, till fixed-point *)
	  let reachedFixedPoint = ref false in
	  let nIter = ref 0 in
	  
	  (* .. with utter disrespect to functional programming ... *)
	  while not !reachedFixedPoint do 
	    nIter := !nIter + 1;
	    reachedFixedPoint := true;

	    (* Update the cin and cout for each node and see if nothing changes *)
	    for i=0 to (!numNodes - 1) do
	      let newCin = evalCin !nodes.(i) cout cin.(i) in
	      if newCin <> cin.(i) then reachedFixedPoint := false;
	      cin.(i) <- newCin;
	      let newCout = evalCout !nodes.(i) (lat_max cin.(i) cout.(i))  in
	      if newCout <> cout.(i) then reachedFixedPoint := false;
	      cout.(i) <- newCout;	    
	    done; (* for *)

	    (* Show that we're actually doing some real work in here *)
	    if amandebug then begin
	      pr "Cin  = [";
	      Array.iter (fun n -> printLatVal n.latType) cin;
	      Printf.printf " ]\n";
	      pr "Cout = [";
	      Array.iter (fun n -> printLatVal n.latType) cout;
	      Printf.printf " ]\n";
	    end;

	  done; (* while *)
	  
	  if amandebug then pr "Reached fixed point in %d iterations\n" !nIter;

     	  (* Now, actually remove the redundant CHECKs *)
	  Array.iteri
	    (fun i nd ->
	      match nd.skind with
	      Instr instList -> 
		nd.skind <- Instr (removeRedundancies cin.(i) instList i)
	      |	_ -> ())
	    !nodes;
	  
	end (* if markSimilar .. else ... *)
	
      end (* if not checkProcessed *)
      else
	if amandebug then begin
	  pr "";
	  ignore (printf "Skipping %d: %a\n" i d_instr !checkInstrs.(i));
	end
    done; (* for i ... *)

    (* Do peephole optimization *)
    if amandebug then pr "Trying peephole ... \n";
    Array.iteri
      (fun i nd -> 
	match nd.skind with
	  Instr instList ->
	    nd.skind <- Instr (removePeephole instList)
	| _ -> ())
      !nodes;

  end; (* if useRedundancyElimination ... *)
  f (* Return the same thing, mutated *)



(* removeRedundancies:
   Use the cin information to eliminate redundant CHECKs.
   As there is no cin/cout info stored inside the basic blocks (Instr),
   they have to be computed from the cin of the node.

   cin_start = the cin value at the beginning of the Instr
   instList = the instruction list comprising the Instr
   checkInstrs = the array of check instructions (to lookup the index of a CHECK)
   checkFlags  = the array to see if the CHECK is a valid candidate for elim.n
   node_number = index of the Instr in the nodes array ... just for debugging
*)
and removeRedundancies (cin_start : lattice) instList node_number =
  (* The loop. 
     cin_local = current cin value. 
     instList = remaining instr's
     The processed instr's are accumulated in filteredList  *)
  let rec removeRedundanciesLoop (cin_local : lattice) instList filteredList =
    match instList with
      [] -> List.rev filteredList (* filteredList happens to be inverted *)
    | h :: t -> 
	(* Is this a candidate for elimination ? *)
	let index = find_in_array !checkInstrs h in
	if index >= 0 && !checkFlags.(index) then begin	
	  let h2 = (* A debugging hack ... let the index be shown in the .c file in the line number info *)
	    match h with 
	      Call (p1,(Lval(Var f,p2)) ,p3,loc) when amandebug -> 
	      (* THIS LINE MUST BE REMOVED. Its not standard ocaml and may not compile in the future *)
		Call (p1,Lval (Var f, p2),p3, 
		      {loc with file = loc.file ^ "_$" ^ 
			(string_of_int node_number) ^ "_" ^
			(string_of_int index) ^  (getLatValDescription cin_local.latType)})
	    | _ -> h
	  in
	  match cin_local.latType with (*------------------------------------------------------------*)
	    Live var -> (* The value of this CHECK is already live in some var 
			   We keep the cin value as it is *)
	      stats_removed := !stats_removed + 1;
	      if amandebug then 
		pr "%s %d removed! (replaced by live var %s)\n" (getCallName h) index var.vname;	  
	      if amanDontRemove 
	      then removeRedundanciesLoop cin_local t (h2 :: filteredList) 
	      else
		(match h with 
		  Call (Some destlv,_,_,loc) -> (* replace with an assignment: var2 = var *) 
		    removeRedundanciesLoop cin_local t 
		      ((Set (destlv, 
			     Lval (Var var, NoOffset), loc)) :: filteredList)
		| _ -> raise (Failure "non-Call in checkInstrs array"))

	  | NotNeeded -> (* This CHECK has already been performed, and the result is live 
			    We keep the cin value unchanged *)
	      stats_removed := !stats_removed + 1;
	      if amandebug then pr "%s %d removed!\n" (getCallName h) index;
	      if amanDontRemove 
	      then removeRedundanciesLoop cin_local t (h2 :: filteredList)	      
	      else removeRedundanciesLoop cin_local t (filteredList) ;	      
	
	  | Unknown | Needed -> (* This CHECK can't be removed.
				   We have to recompute cin. *)
	      if amandebug then 
		pr "%s %d kept (lattice = %s)\n" (getCallName h2) 
		  index (getLatValDescription cin_local.latType);

	      removeRedundanciesLoop (evalCoutInstr cin_local h2) t (h2 :: filteredList)
	end
	else (* This instr is not a candidate for elimination.
		It could be a Set, Call or Asm... so recompute the new cin.
		Re-use the current cin as far as possible *)
	  removeRedundanciesLoop (evalCoutInstr cin_local h) t (h :: filteredList)
	  (* removeRedundanciesLoop (lat_max cin_local (minLatticeValue h)) t (h :: filteredList) *)
  in
  removeRedundanciesLoop cin_start instList []

(* removePeephole:
   Uses peephole optimization to remove CHECKs.
*)
and removePeephole instList =
  (List.fold_right 
     (fun inst acc -> 
       try 
	 (peepholeReplace inst) :: acc 
       with PeepholeRemovable ->
	 stats_removed := !stats_removed + 1;
	 if amandebug then begin 
	   pr "";
	   ignore (Pretty.printf "Removed by peephole: %a@!" d_instr inst);
	 end;
	 acc)
     instList [])


(* peepholeReplace:
   Returns a peephole-optimized replacement for a given CHECK.
   Raises PeepholeRemovable if this check can be removed
*)
and peepholeReplace (chk : instr) : instr =
  match chk with
    Call (_, Lval(Var{vname="CHECK_POSITIVE"},_), [Const (CInt32 (c,_,_))],_) 
    when c >= Int32.zero -> raise PeepholeRemovable

  | Call (_, Lval(Var{vname="CHECK_LBOUND"},_), [a;b],_) 
    when expEqual a b -> raise PeepholeRemovable
  | Call (_, Lval(Var{vname="CHECK_LBOUND"},_),
	  [CastE (_,StartOf (a, aoff)); CastE (_, AddrOf(b,Index(zero,boff))) ] ,_) 
    when a=b && aoff=boff -> raise PeepholeRemovable

  | Call (_, Lval(Var{vname="CHECK_FATSTACKPOINTER"},_), [a;b],_)
      when (compareWithoutCasts a b) -> raise PeepholeRemovable

  | Call (_, Lval(Var{vname="CHECK_BOUNDS"},_), [lo;hi;mid;_],_) 
      when intervalContainsExp lo hi mid -> raise PeepholeRemovable

  | _ -> chk


(* intervalContainsExp:
   Returns whether mid is in the closed interval [lo,hi]
*)
and intervalContainsExp (lo:exp) (hi:exp) (mid:exp) : bool =
  let lo = simplifyExp lo  in
  let hi = simplifyExp hi  in
  let mid= simplifyExp mid in
  match lo,hi,mid with
    Lval lv, (* For the new simplifyExp function *)
    BinOp (IndexPI, lvv, c,_),
    BinOp (IndexPI, lvi, c2, _)
    when (expEqual lo lvv) && (expEqual lo lvi) ->      
      ((compareExp c2 zero) >= Some 0) &&
      ((compareExp c  c2)   > Some 0)
  | StartOf lv, (* this case is probably unused *)
    BinOp (IndexPI, StartOf lvv, c,_),
    AddrOf ((_,lvioff) as lvi)
    when lv = lvv && lv = (stripOffset lvi) ->
      (match getOutermostOffset lvioff with
	Index (c2, NoOffset) ->
	  ((compareExp c2 zero) >= Some 0) &&
	  ((compareExp c  c2)   > Some 0)
      |	_ -> false);      
  | _ -> 
      false


(* minLatticeValue:
   Compute the least possible lattice value for an instr
   This instr is assumed to be not flagged. Flagged CHECK Calls must be 
   processed specially.
   This is a very subtle and important function
*)
and minLatticeValue (inst : instr) : lattice =
  let isLvalClobbering lval : bool =
    ((List.mem lval !checkLvals) || (* Directly modifying the argument to the CHECK *)
    (!checkHasAliasedVar &&         (* Possibly, indirectly modifying *)
     (match lval with 
       (Mem _,_) -> true
     | (_, off) -> 
	 match (getOutermostOffset off) with
	   Index _ -> false (* ab: I changed it to false ... I think array set operations cant change values of other variables *)
	 | _ -> false)))
  in  
  if amanNonConservative then latCheckNotNeeded
  else
    match inst with
      Set (lval,exp,_) -> 
	if isLvalClobbering lval
	then latCheckNeeded
	else latCheckNotNeeded
    | Call (Some lval,Lval(Var f,_),_,_) when isLvalClobbering lval ->
	latCheckNeeded
    | Call (_,Lval(Var f,_),args,_) ->
	if (isCheckCall_str f.vname) 
	then  latCheckNotNeeded (* CHECK's are non-clobbering *)
	else  latCheckNeeded    (* Other functions may clobber *)
    | Call _ -> latCheckNeeded 
    | Asm  _ -> latCheckNeeded


(* evalCin:
   Evaluates the cin for a given CFG node.
   cin = max (cout_predecessors) 
   If two pred's have Live values in different variables, we choose Needed
   as a safe upper-bound (see the lat_max function) *)
and evalCin (nd : stmt) cout at_least =
  List.fold_right 
    (fun a b -> lat_max b cout.(a.sid)) 
    nd.preds at_least


(* evalCout:
   Evaluates the cout for a given CFG node.
   The calculation is based on the cin at the beginning of the same node and we 
   have to iterate through all the instr's in the node.
   The cout is reset to NotNeeded, or Live right after a check that is being
   processed (flagged in checkFlags) *)
and evalCout (nd : stmt) cin_start : lattice = (* TODO: Look at the args of the check *)
  (*
  let rec evalCoutLoop (cin_local : lattice) (instList : instr list)  = 
    match instList with
      [] -> cin_local
    | h :: t -> 
	evalCoutLoop (evalCoutInstr cin_local h) t
  in
  *)
  match nd.skind with
    Instr instList -> 
      List.fold_left evalCoutInstr cin_start instList
      (* evalCoutLoop cin_start instList *)
  | _ -> cin_start	

(* evalCoutInstr:
   Evaluate the cout of an instr based on its cin. See evalCout for info
*)
and evalCoutInstr (cin : lattice) (inst : instr) : lattice =
  let index = find_in_array !checkInstrs inst in
  if index >= 0 && !checkFlags.(index) 
  then match !checkInstrs.(index) with
    Call (Some (Var var, NoOffset),_,_,_) ->
      (* If some variable already holds a live value, use the same *)
      if cin.latWeight =  wtLIVE then cin else (latLive var)
  | Call (None,_,_,_) -> latCheckNotNeeded
  | _ -> failwith "non-Call in checkInstrs array"
  else lat_max cin (minLatticeValue inst)


(* checkIncludes:
   Is check b implied by check a?
*)
and checkImplies (a : instr) (b : instr) : bool =
  match getCallName a, getCallName b with
    "CHECK_UBOUND_OR_NULL", "CHECK_UBOUND_OR_NULL" ->
      false
  | _ -> false


(* numberNodes:
   Use the nodeList and fill in the nodes array.
   For any node n, nodes.(n.sid) == n *)
and numberNodes () =
  let rec numberNodesRec (i : int) = function
      [] -> ()
    | node :: rest -> 
	node.sid <- i; 
	!nodes.(i) <- node;
	
	numberNodesRec (i - 1) rest
  in 
  nodes := Array.make !numNodes dummyStmt;
  numberNodesRec (!numNodes - 1) !nodeList 


(* filterChecks:
   Process a list of stmt's, 
   find the CHECK Calls in the Instr's inside them
   and return an array of all such Calls found *)
and filterChecks (stmts : stmt list) : instr list =
  let rec filterChecksLoop (stmts : stmt list) (instrs : instr list) (acc : instr list) =
  match instrs , stmts with (* Does this actually allocate a pair ? *)
    [], [] -> acc
  | [], {skind=Instr i} :: t -> filterChecksLoop t i acc
  | [], _ :: t -> filterChecksLoop t [] acc
  | h :: t, _  -> filterChecksLoop stmts t (if (isCheckCall_instr h) then h::acc else acc)
  in
  filterChecksLoop stmts [] []

(* markSimilar:
   Set the values in the flags array to true if the corresponding check is similar to
   checkInstrs.(index) and a candidate for being redundant *)
and markSimilar (index : int) : int =
  let count = ref 0 in
  Array.iteri 
    (fun i chk -> !checkFlags.(i) <- isSimilar !checkInstrs.(index) chk;
      if !checkFlags.(i) then count := !count + 1;
      if (i != index) && (!checkFlags.(i)) 
      then (if amandebug then pr "%d == %d\n" index i))
    !checkInstrs; (* Perhaps , we only need to iter thru index - (length-1) *)
  !count

(* isSimilar:
   markSimilar relies on this function to decide whether two CHECKs are simular
   i.e. whether they do and return the same thing 

   Two CHECKs are similar if they have the same function being called and
   1. All arguments are identical, or
   2. If its a CHECK_FETCHLENGTH and the second arguments are identical
*)
and isSimilar (a : instr) (b : instr) : bool =
  match a,b with
    Call (_,Lval(Var ax,_),argsa,_) , Call (_,Lval(Var bx,_),argsb,_) ->
      ax.vname = bx.vname &&
      ((argsa = argsb) || 
       (ax.vname = "CHECK_FETCHEND" && (List.tl argsa) = (List.tl argsb)) ||
       (ax.vname = "CHECK_FETCHLENGTH" && (List.tl argsa) = (List.tl argsb)))   
  | _,_ -> false


(* getCheckedLvals:
   Look at the arguments of a CHECK Call and returns a list of lvals
   that this CHECK depends on.
   Parent lvals are also returned with their offsets stripped off.
   e.g. a.b[3] -> [a.b[3] ; a.b  ; a]
   As long as the contents of these lvals dont change, the CHECK results remain
   valid.
   The returned list may contain duplicates
*)
and getCheckedLvals (check : instr) : lval list =
  let rec getLvalsFromExp (e : exp) : lval list =
    match e with 
      Const _
    | SizeOf _ 
    | SizeOfE _
    | AlignOf _
    | AlignOfE _ -> []
    | UnOp (_,e1,_) -> getLvalsFromExp e1
    | BinOp (_,e1,e2,_) -> (getLvalsFromExp e1) @ (getLvalsFromExp e2)
    | Question (e1,e2,e3) -> 
	(getLvalsFromExp e1) @ (getLvalsFromExp e2) @ (getLvalsFromExp e3)
    | CastE (_,e1) -> getLvalsFromExp e1
    | AddrOf ((_,off) as lv) -> 
	let off = getOutermostOffset off in begin
	  match off with 
	    NoOffset -> []
	  | Index (i, _) -> 
	      (getLvalsFromExp i) @ getLvalsFromExp (Lval (stripOffset lv)) 
	  | Field _ -> 
	      getLvalsFromExp (Lval (stripOffset lv))
	end
    | StartOf lv -> getLvalsFromExp (Lval lv)
    | Lval ((Mem e1, NoOffset) as lv) -> lv :: (getLvalsFromExp e1)
    | Lval ((Var v, NoOffset) as lv) -> [lv]
    | Lval ((_, Field _) as lv) -> 
	lv :: (getLvalsFromExp (Lval (stripOffset lv)))
    | Lval ((_, Index (i,_)) as lv) -> 
	lv :: (getLvalsFromExp i) @ (getLvalsFromExp (Lval (stripOffset lv)))
  in  
  match check with
    Call (_,Lval(Var checkName,_),args,_) -> 
      let someargs =
	if checkName.vname = "CHECK_FETCHLENGTH" || checkName.vname = "CHECK_FETCHEND" 
	then List.tl args
	else args
      in
      List.fold_right (fun e acc -> (getLvalsFromExp e) @ acc) someargs []
  | _ -> raise (Failure "non-Call in checkInstrs array")


(* Find an aliased variable in the lval
*)
and lvalHasAliasedVar (lv : lval) : bool =
  match lv with
    (Var v, NoOffset) -> v.vaddrof
  | (Mem e, NoOffset) -> expHasAliasedVar e
  | (vm, Field (_, off)) -> lvalHasAliasedVar (vm, off)
  | (vm, Index (e, off)) -> (expHasAliasedVar e) || (lvalHasAliasedVar (vm, off))
  

and expHasAliasedVar (e : exp) : bool = 
  match e with
    Const _ 
  | SizeOf _
  | SizeOfE _
  | AlignOf _
  | AlignOfE _ -> false
  | UnOp (_,e1,_) -> expHasAliasedVar e1
  | BinOp (_,e1,e2,_) -> (expHasAliasedVar e1) || (expHasAliasedVar e2)
  | Question (e1,e2,e3) -> 
      ((expHasAliasedVar e1) || (expHasAliasedVar e2) || (expHasAliasedVar e3))
  | CastE (_,e1) -> expHasAliasedVar e1
  | AddrOf lv -> lvalHasAliasedVar lv
  | StartOf lv -> lvalHasAliasedVar lv
  | Lval lv -> lvalHasAliasedVar lv
    

(*----------------------------------------------------------------------------*)

let rec eliminateAllChecks (f : fundec) (checkName : string) : fundec =
    Array.iter (function s ->
      if amandebug then pr "Removing all %s checks\n" checkName;
      match s.skind with
 	Instr l when checkName="" -> s.skind <- Instr (removeAllChecks l)
      |	Instr l -> s.skind <- Instr (removeCheck checkName l)
      | _ -> ())
      !nodes;
    f  

and removeAllChecks (i : instr list) : instr list =
  let rec removeAllChecksLoop (acc : instr list) : instr list -> instr list = function
    [] -> acc
  | hd :: rest ->
      match hd with
      | Call(_,Lval(Var x,_),args,l) when isCheckCall_str x.vname -> removeAllChecksLoop acc rest	  
      |	_ -> removeAllChecksLoop (hd :: acc) rest
  in List.rev (removeAllChecksLoop [] i)

and removeCheck (checkName : string) (i : instr list) : instr list =
  let rec removeCheckLoop (checkName : string) (acc : instr list) : instr list -> instr list = function
    [] -> acc
  | hd :: rest ->
      match hd with
      | Call(_,Lval(Var x,_),args,l) when (checkName.[0] != 'C') && (strstr x.vname checkName) ->
	  removeCheckLoop checkName acc rest	  
      | Call(_,Lval(Var x,_),args,l) when x.vname = checkName -> 
	  removeCheckLoop checkName acc rest	  
      |	_ -> removeCheckLoop checkName (hd :: acc) rest
  in List.rev (removeCheckLoop checkName [] i)



let getStatistics () : string =
  let t = !stats_total in
  let r = !stats_removed in
  let n = !stats_nc_removed in
  let (//) a b = (float_of_int a) /. (float_of_int b) in
  Printf.sprintf 
    "\n  Total CHECKs \t\t %d\n  Null-check-remover \t %d removed\n  Redundancy eliminator  %d removed\n  Summary \t\t %d (%.2f%%) removed, %d (%.2f%%) kept\n"
    t n r (n+r) (100*(n+r)//t) (t-n-r) (100*(t-n-r) // t)
   

(*  ,,---.     	       |                    |                          	      *)
(*  ||    |            |	      	    | 			     	      *)
(*  ||    | ,---.  ,---|  |   |	 ,---. 	,---|  ,---.  ,---.  ,---.  .   ,     *)
(*  ||___,' |---'  |   |  |   |	 |   | 	|   |  |   |  |	  |  |	    |   |     *)
(*  ||   \  '---'  `---'  '---'	 '   '  `---'  `---'- '	  '  '---'  `---|     *)
(* 	       	       	       	       	 			    .---+-    *)
(* 	    (Just a visual marker)     	 			    '---'     *)
(*vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv*)






(*------------------------------------------------------------*)
(* Carry out a series of optimizations on a function definition *)

let optimFun (f : fundec) =
  if debug then begin
    ignore (printf "===================================\n");
    ignore (printf "Analyzing %s\n" f.svar.vname);
  end;

  (* Fill in the CFG information (succs and pred only)*)
  numNodes := 0;
  nodeList := [];
  cfgBlock f.sbody None None None;


  let optimizedF = ref f in
  (* Carry out the optimizations, one at a time *)

  (* Remove redundant CHECK_NULL *)
  optimizedF := nullChecksOptim !optimizedF;

  (* Remove other redundant checks *)
  optimizedF := eliminateRedundancy !optimizedF;

  (* Remove all checks ... useful to see which tests make a difference *)
  (try let rm = Sys.getenv "REDREMOVEALL" in
  if rm = "1" then
    optimizedF := eliminateAllChecks !optimizedF ""
  else
    optimizedF := eliminateAllChecks !optimizedF rm
  with _ -> ());

  (* Return the final optimized version *)
  !optimizedF

(*------------------------------------------------------------*)
let optimFile file =
  if debug
  then begin
    ignore(printf "\n-------------------------------------------------\n");
    ignore(printf "OPTIM MODULE STARTS\n\n")
  end;

  (* Replace every function definition by its optimized version*)
  file.globals <- List.map
      (function
          GFun(f,l) -> GFun(optimFun f, l)
        | _ as other -> other)
      file.globals;

  (* ab: ... and the global initializer *)
  (match file.globinit with
    Some f when not amanSkipGlobInit -> 
      file.globinit <- Some (optimFun f)
  | _ -> ());
  
  if debug
  then begin
    ignore(printf "\n\nOPTIM MODULE ENDS\n");
    ignore(printf "--------------------------------------------------\n")
  end;

  file
(*------------------------------------------------------------*)
