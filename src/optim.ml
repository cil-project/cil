(* Written by S. P. Rahul *)

(* Optimizes the placement of boxing checks *)

open Pretty
open Cil
module E=Errormsg

let debug = false

(*-----------------------------------------------------------------*)
let dummyStmt = {labels = []; sid = -1; skind = Break {line = -1; file = ""};
                  succs= []; preds = []} (* a place holder so that we can create
                                         an array of stmts *)
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
          if (List.mem checkExp nnl) then optimInstr tl nnl (* remove redundant check*)
          else first::optimInstr tl (checkExp::nnl) (* add to the list of valid non-null exp *)
      | Call(_,Lval(Var x,_),args,l)  (* Don't invalidate for well-behaved functions *)
        when ((String.length x.vname) > 6 &&
              (String.sub x.vname 0 6)="CHECK_") -> first::optimInstr tl nnl
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

let useRedundancyElimination = true

(* Lattice values *)
let latUnknown        = 0
let latCheckNotNeeded = 1
let latCheckNeeded    = 2

let amandebug = false
let amanDontRemove = amandebug && true

let stats_removed = ref 0
let stats_kept    = ref 0

let pr s = Printf.printf  "REDDBG: "; Printf.printf s

let rec eliminateRedundancy (f : fundec) : fundec =      (* Using let rec because I couldn't find let*  *)
  if useRedundancyElimination then begin
    let _ = numberNodes () in
    if amandebug then  printCfgBlock ~showGruesomeDetails:false f.sbody;
    flush stdout;
    let cin = Array.make !numNodes latUnknown in
    let cout = Array.make !numNodes latUnknown in
    let checkInstrs : instr array  = filterChecks !nodeList in 
    let countCheckInstrs = Array.length checkInstrs in
    if amandebug then pr "------- Function %s contains %d nodes and %d CHECKS\n" f.svar.vname !numNodes countCheckInstrs;
    let checkFlags : bool  array = Array.make countCheckInstrs false in
    let checkProcessed : bool array = Array.make countCheckInstrs false in
    for i=0 to (countCheckInstrs - 1) do
      if not (checkProcessed.(i)) then begin
	checkProcessed.(i) <- true;
	if amandebug then pr "Processing %d\n" i;
	markSimilar checkFlags checkInstrs i;
	Array.iteri (fun i a -> checkProcessed.(i) <- checkProcessed.(i) || a) checkFlags;

	(* Reset cin, cout *)
	Array.iteri (fun i _ -> cin.(i) <- latUnknown ; cout.(i) <- latUnknown) cin;

	(* Set the entry point cin *)
	(match f.sbody with
	  h :: t -> cin.(h.sid) <- latCheckNeeded; 
	    if amandebug then pr "Entry node = %d\n" h.sid
	| _ -> ());

	(* Find cin,cout, till fixed-point *)
	let reachedFixedPoint = ref false in
	let nIter = ref 0 in

	while not !reachedFixedPoint do
	  nIter := !nIter + 1;
	  reachedFixedPoint := true;
	  for i=0 to (!numNodes - 1) do
	    let newCin = evalCin !nodes.(i) cout cin.(i) in
	    if newCin != cin.(i) then reachedFixedPoint := false;
	    cin.(i) <- newCin;
	    let newCout = evalCout !nodes.(i) (max cin.(i) cout.(i)) checkInstrs checkFlags in
	    if newCout != cout.(i) then reachedFixedPoint := false;
	    cout.(i) <- newCout;	    
	  done
	done;

	if amandebug then begin
	  pr "Reached fixed point in %d iterations\n" !nIter;
	  pr "Cin  = [";
	  Array.iter (fun n -> Printf.printf " %d" n) cin;
	  Printf.printf " ]\n";
	  pr "Cout = [";
	  Array.iter (fun n -> Printf.printf " %d" n) cout;
	  Printf.printf " ]\n";
	end;

	(* Remove redundant CHECKs *)
	for i=0 to (!numNodes - 1) do
	  match !nodes.(i).skind with
	    Instr instList -> !nodes.(i).skind <- Instr (removeRedundancies cin.(i) instList checkInstrs checkFlags i)
	  | _ -> ()
	done
	
      end
      else
	if amandebug then pr "Skipping %d\n" i
    done;
    ()
  end;
  f

and removeRedundancies at_least instList checkInstrs checkFlags node_number =
  let rec removeRedundanciesRec at_least instList filteredList =
    match instList with
      [] -> List.rev filteredList
    | h :: t -> 
	let index = find_in_array checkInstrs h in
	if index >= 0 && checkFlags.(index) then begin	
	  let h2 = (* A debugging hack ... let the index be shown in the .c file in the line number info *)
	    match h with 
	      Call (p1,(Lval(Var f,p2)) ,p3,loc) when amandebug -> 
	      (* THIS LINE MUST BE REMOVED. Its not standard ocaml and may not compile in the future *)
		Call (p1,Lval (Var f, p2),p3, 
		      {loc with file = loc.file ^ "_$" ^ 
			(string_of_int node_number) ^ "_" ^
			(string_of_int index) ^  [| " ?" ; " XXXX" ; " @" |].(at_least) })
	    | _ -> h
	  in
	  if at_least == latCheckNotNeeded then begin
	    if amandebug then pr "CHECK %d removed!\n" index;	  
	    stats_removed := !stats_removed + 1;
	    if amanDontRemove then
	      removeRedundanciesRec latCheckNotNeeded t (h2 :: filteredList) 
	    else
	      removeRedundanciesRec latCheckNotNeeded t (filteredList) 
	  end
	  else begin
	    if amandebug then pr "CHECK %d kept (lattice = %d)\n" index at_least;
	    stats_kept := !stats_kept + 1;
	    removeRedundanciesRec latCheckNotNeeded t (h2 :: filteredList) ;
	  end
	end
	else 
	  removeRedundanciesRec (max at_least (minLatticeValue h)) t (h :: filteredList) 
  in
  removeRedundanciesRec at_least instList []

and minLatticeValue (inst : instr) : int =
  match inst with
    Set (lval,exp,_) -> latCheckNeeded
  | Call (None,Lval(Var f,_),args,_) -> 	
      if (isCheckCall_str f.vname) 
      then  latCheckNotNeeded
      else  latCheckNeeded
  | Call (Some (var, varp),Lval(Var f,_),args,_) -> 
      if (isCheckCall_str f.vname) 
      then  latCheckNotNeeded
      else  latCheckNeeded
  | Call (None,func,args,_) ->  latCheckNeeded 
  | Call (Some (var, varp),func,args,_) ->  latCheckNeeded
  | Asm _ -> latCheckNeeded
 
and evalCin (nd : stmt) cout at_least =
  List.fold_right (fun a b -> max (cout.(a.sid)) b) nd.preds at_least

and evalCout (nd : stmt) at_least checkInstrs checkFlags = (* TODO: Look at the args of the check *)
  let rec evalCoutLoop (at_least : int) (instList : instr list)  = match instList with
      [] -> at_least
    | h :: t -> 
	let index = find_in_array checkInstrs h in
	if index >= 0 && checkFlags.(index) then evalCoutLoop (max at_least latCheckNotNeeded) t
	else evalCoutLoop (max at_least (minLatticeValue h)) t
  in
  match nd.skind with
    Instr instList -> evalCoutLoop at_least instList
  | _ -> at_least	

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


and filterChecks (stmts : stmt list) : instr array =
  let rec filterChecksLoop (stmts : stmt list) (instrs : instr list) (acc : instr list) =
  match instrs , stmts with (* Does this actually allocate a pair ? *)
    [], [] -> acc
  | [], {skind=Instr i} :: t -> filterChecksLoop t i acc
  | [], _ :: t -> filterChecksLoop t [] acc
  | h :: t, _  -> filterChecksLoop stmts t (if (isCheckCall_instr h) then h::acc else acc)
  in
  Array.of_list (filterChecksLoop stmts [] [])

and markSimilar (flags : bool array) (checkInstrs : instr array) (index : int) =
  Array.iteri 
    (fun i chk -> flags.(i) <- isSimilar checkInstrs.(index) chk;
      if (i != index) && (flags.(i)) then if amandebug then pr "%d == %d\n" index i)
    checkInstrs (* Perhaps , we only need to iter thru index - (length-1) *)

and isSimilar (a : instr) (b : instr) : bool =
  match a,b with
    Call (_,Lval(Var ax,_),argsa,_) , Call (_,Lval(Var bx,_),argsb,_) ->
      ax.vname = bx.vname &&
      argsa = argsb
  | _,_ -> false

(* Returns -1 if not found ... letting the exception bubble through caused a segfault ?!?! *)
and find_in_array (array : 'a array) (element : 'a) : int =  
  let ubound = Array.length array in
  let rec findloop i =
    if i >= ubound then -1 else
    if array.(i) == element then i else findloop (i + 1)
  in
  findloop 0

(*----------------------------------------------------------------------------*)

let rec eliminateAllChecks (f : fundec) (checkName : string) : fundec =
    Array.iter (function s ->
      match s.skind with
(* 	Instr l -> s.skind <- Instr (removeAllChecks l)*)
	Instr l -> s.skind <- Instr (removeCheck checkName l)
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
      | Call(_,Lval(Var x,_),args,l) when x.vname = checkName -> removeCheckLoop checkName acc rest	  
      |	_ -> removeCheckLoop checkName (hd :: acc) rest
  in List.rev (removeCheckLoop checkName [] i)



let getStatistics () : string =
  Printf.sprintf "CHECK Redundancy- %d kept, %d removed" !stats_kept !stats_removed
   

(*  ,,---.     	       |	      	    | 			     	      *)
(*  ||    |            |	      	    | 			     	      *)
(*  ||    | .---.  .---|  |   |	 .---. 	.---|  .---.  .---.  .---.  .   ,     *)
(*  ||__./  |---'  |   |  |   |	 |   | 	|   |  |   |  |	  |  |	    |   |     *)
(*  ||   \  '---'  `---'  '---'	 '   '  `---'  `---'- '	  '  '---'  `---|     *)
(* 	       	       	       	       	 			    .---+-    *)
(* 	    (Just a visual marker)     	 			    '---'     *)
(*vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv*)






(*------------------------------------------------------------*)
(* Carry out a series of optimizations on a function definition *)

let optimFun f =
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
  (* optimizedF := eliminateAllChecks !optimizedF "CHECK_BOUNDS" *)

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

  if debug
  then begin
    ignore(printf "\n\nOPTIM MODULE ENDS\n");
    ignore(printf "--------------------------------------------------\n")
  end;

  file
(*------------------------------------------------------------*)
