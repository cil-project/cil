(* Written by S. P. Rahul *)

(* Optimizes the placement of boxing checks *)

open Pretty
open Cil
module E=Errormsg

let debug = false

(*-----------------------------------------------------------------*)
let dummyStmt = {labels = []; sid = -1; skind = Break {line = -1;col = -1;file = ""};
                  succs= []; preds = []} (* a place holder so that we can create
                                         an array of stmts *)
let sCount = ref 0   (* to generate consecutive sid's *)
let numNodes = ref 0 (* number of nodes in the CFG *)
let nodes = ref [| |] (* ordered nodes of the CFG *)
let markedNodes = ref [] (* Used in DFS for recording visited nodes *)
let gen = ref [| |] (* array of exp list, representing GEN sets *)
let kill = ref [| |] (* array of bool, representing KILL sets *) 
                     (* see comment in nullChecksOptim why they are bool's *)
let inNode = ref[| |] (* dataflow info at the entry of a node. Array of flowVal *)
let outNode = ref[| |] (* dataflow info at the exit of a node. Array of flowVal *)
let allVals = ref [] (* list of all flow values. To be used as TOP in dataflow analysis *)
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
(* At the end of CFG computation, numNodes = total number of CFG nodes *)
let rec cfgBlock blk (next:stmt option) (break:stmt option) (cont:stmt option) =
  match blk with
    [] -> ();
  | [s] -> cfgStmt s next break cont 
  | hd::tl -> 
      cfgStmt hd (Some (List.hd tl))  break cont;
      cfgBlock tl next break cont

(* Fill in the CFG info for a stmt
   Meaning of next, break, cont should be clear from earlier comment
*)
and cfgStmt s (next:stmt option) (break:stmt option) (cont:stmt option) =
  numNodes := !numNodes + 1;
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
        

let rec printCfgBlock blk =
  List.iter (function s ->
    printCfgStmt s)
    blk
    
and printCfgStmt s =
  ignore (printf "-------------------------------------------------\n");
  ignore (printf "Id: %d\n" s.sid);
  ignore (printf "Succs: ");
  List.iter (function s -> ignore (printf "%d " s.sid)) s.succs; 
  ignore (printf "\n");
  ignore (printf "Preds: ");
  List.iter (function s -> ignore (printf "%d " s.sid)) s.preds; 
  ignore (printf "\n");
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

and dfs s = 
  if not (List.memq s !markedNodes) then begin
    markedNodes := s::!markedNodes;
    List.iter dfs s.succs;
    s.sid <- !sCount;
    !nodes.(!sCount) <- s; (* add to ordered array *)
    sCount := !sCount - 1;
  end

(*-----------------------------------------------------------------*)
(* Print all the CHECK_NULL arguments (without a cast if there is one).
   For debugging purposes *)

let rec printChecksBlock blk = 
  List.iter printChecksStmt blk

and printChecksStmt s =
  match s.skind with 
    Instr l -> List.iter (function (i,_) -> printChecksInstr i) l
  | Return _ | Continue _ | Break _ | Goto _ -> ()
  | If (e,blk1,blk2,_) -> printChecksBlock blk1; printChecksBlock blk2;
  | Switch (e,blk,_,_) -> printChecksBlock blk
  | Loop (blk,_) -> printChecksBlock blk

and printChecksInstr i =
  match i with 
    Call (_,Lval(Var x,_),args) when x.vname = "CHECK_NULL" ->
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
    Instr l ->  createGenKillForInstrList i (List.map (function (ins,l) -> ins) l) (* TODO *)
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
  | Call(_,Lval(Var x,_),args) when x.vname = "CHECK_NULL" ->
      (* args is a list of one element *)
      let arg = List.hd args in
      (match arg with
        CastE(_,e) -> ([e],false)
      | _ -> ([arg],false))
  | Call(_,Lval(Var x,_),args) (* Don't invalidate set if function is 
                                  * something we know behaves well *)
    when ((String.length x.vname) > 6 && 
          (String.sub x.vname 0 6)="CHECK_") -> ([],false)
  | Call _ -> ([],true) (* Otherwise invalidate everything *)


(*-----------------------------------------------------------------*)
(* Remove redundant CHECK_NULLs from a list of instr. Essentially a
   basic-block optimization *)
(* nnl (for NotNullList) is a list of expressions guaranteed to be not null
   on entry to l *)
let rec optimInstr (l: (instr * location) list) nnl = 
  match l with
    [] -> []
  | ((first,firstLoc) as hd)::tl -> 
      match first with 
        Asm _ -> hd::optimInstr tl nnl
      | Set _ -> hd::optimInstr tl [] (* Kill everything *)
      | Call(_,Lval(Var x,_),args) when x.vname = "CHECK_NULL" ->
      (* args is a list of one element *)
          let arg = List.hd args in
          let checkExp = (match arg with CastE(_,e) -> e | _ -> arg) in
          if (List.mem checkExp nnl) then optimInstr tl nnl (* remove redundant check*)
          else hd::optimInstr tl (checkExp::nnl) (* add to the list of valid non-null exp *)
      | Call(_,Lval(Var x,_),args)  (* Don't invalidate for well-behaved functions *)
        when ((String.length x.vname) > 6 && 
              (String.sub x.vname 0 6)="CHECK_") -> hd::optimInstr tl nnl
      | Call _ -> (* invalidate everything *)
          hd::optimInstr tl []
  
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
  
  (* Order nodes by revers depth-first postorder *)
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

(*------------------------------------------------------------*)
(* Carry out a series of optimizations on a function definition *)

let optimFun f =
  if debug then begin
    ignore (printf "===================================\n");
    ignore (printf "Analyzing %s\n" f.svar.vname);
  end;

  (* Fill in the CFG information (succs and pred only)*)
  numNodes := 0;
  cfgBlock f.sbody None None None;


  let optimizedF = ref f in
  (* Carry out the optimizations, one at a time *)

  (* Remove redundant CHECK_NULL *)
  optimizedF := nullChecksOptim !optimizedF;
  
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
