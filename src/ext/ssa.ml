module B=Bitmap
open Cil
open Pretty
    
(* Globalsread, Globalswritten should be closed under call graph *)

module StringOrder = 
  struct
    type t = string
    let compare s1 s2 = 
      if s1 = s2 then 0 else
      if s1 < s2 then -1 else 1
  end
  
module StringSet = Set.Make (StringOrder)


type cfgInfo = {
    start   : int;          
    size    : int;    
    blocks: cfgBlock array; (** Dominating blocks must come first *)
    successors: int list array; (* block indices *)
    predecessors: int list array;   
    mutable nrRegs: int;
    mutable regToVarinfo: varinfo array; (** Map register IDs to varinfo *)
  } 

(** A block corresponds to a statement *)
and cfgBlock = { 
    bstmt: Cil.stmt;

    (* We abstract the statement as a list of def/use instructions *)
    instrlist: instruction list;
    mutable livevars: (reg * int) list;  
    (** For each variable ID that is live at the start of the block, the 
     * block whose definition reaches this point. If that block is the same 
     * as the current one, then the variable is a phi variable *)
  }
  
and instruction = (reg list * reg list) 
  (* lhs variables, variables on rhs.  *)


and reg = int

type idomInfo = int array  (* immediate dominator *)

and dfInfo = (int list) array  (* dominance frontier *)

(* Muchnick's Domin_Fast, 7.16 *)

let compute_idom (flowgraph: cfgInfo): idomInfo = 
  let start = flowgraph.start in
  let size  = flowgraph.size in
  let successors = flowgraph.successors in
  let predecessors = flowgraph.predecessors in 
  let n0 = size in (* a new node (not in the flowgraph) *)
  let idom = Array.make size (-1) in (* Make an array of immediate dominators  *)
  let nnodes  = size + 1 in
  let nodeSet = B.init nnodes (fun i -> true) in
  
  let ndfs     = Array.create nnodes 0 in (* mapping from depth-first 
                                         * number to nodes. DForder 
                                         * starts at 1, with 0 used as 
                                         * an invalid entry  *)
  let parent   = Array.create nnodes 0 in (* the parent in depth-first 
                                         * spanning tree  *) 

      (* A semidominator of w is the node v with the minimal DForder such 
       * that there is a path from v to w containing only nodes with the 
       * DForder larger than w.  *)
  let sdno     = Array.create nnodes 0 in (* depth-first number of 
                                         * semidominator  *)
  
                                        (* The set of nodes whose 
                                           * semidominator is ndfs(i) *)
  let bucket   = Array.init nnodes (fun _ -> B.cloneEmpty nodeSet) in
  
       (* The functions link and eval maintain a forest within the 
          * depth-first spanning tree. Ancestor is n0 is the node is a root in 
          * the forest. Label(v) is the node in the ancestor chain with the 
          * smallest depth-first number of its semidominator. Child and Size 
          * are used to keep the trees in the forest balanced  *)
  let ancestor = Array.create nnodes 0 in 
  let label    = Array.create nnodes 0 in
  let child    = Array.create nnodes 0 in
  let size     = Array.create nnodes 0 in
  
  
  let n = ref 0 in                  (* depth-first scan and numbering. 
                                       * Initialize data structures. *)
  ancestor.(n0) <- n0;
  label.(n0)    <- n0;
  let rec depthFirstSearchDom v =
    incr n;
  sdno.(v) <- !n;
    ndfs.(!n) <- v; label.(v) <- v;
    ancestor.(v) <- n0;             (* All nodes are roots initially *)
    child.(v) <- n0; size.(v) <- 1;
    List.iter 
      (fun w ->
	if sdno.(w) = 0 then begin 
          parent.(w) <- v; depthFirstSearchDom w 
	end)
      successors.(v);
  in
       (* Determine the ancestor of v whose semidominator has the the minimal 
          * DFnumber. In the process, compress the paths in the forest.  *)
  let eval v =
    let rec compress v =
      if ancestor.(ancestor.(v)) <> n0 then
	begin
	  compress ancestor.(v);
	  if sdno.(label.(ancestor.(v))) < sdno.(label.(v)) then
	    label.(v) <- label.(ancestor.(v));
	  ancestor.(v) <- ancestor.(ancestor.(v))
      end
    in
    if ancestor.(v) = n0 then label.(v)
    else begin
      compress v;
      if sdno.(label.(ancestor.(v))) >= sdno.(label.(v)) then
	label.(v)
      else label.(ancestor.(v))
    end
  in
  
  let link v w =
    let s = ref w in
    while sdno.(label.(w)) < sdno.(label.(child.(!s))) do
      if size.(!s) + size.(child.(child.(!s))) >= 2* size.(child.(!s)) then
	(ancestor.(child.(!s)) <- !s;
	 child.(!s) <- child.(child.(!s)))
      else
	(size.(child.(!s)) <- size.(!s);
	 ancestor.(!s) <- child.(!s); s := child.(!s));
    done;
    label.(!s) <- label.(w);
    size.(v) <- size.(v) + size.(w);
    if size.(v) < 2 * size.(w) then begin
      let tmp = !s in
      s :=  child.(v);
      child.(v) <- tmp;
    end;
    while !s <> n0 do
      ancestor.(!s) <- v;
      s := child.(!s);
    done;
  in
      (* Start now *)
  depthFirstSearchDom start;
  for i = !n downto 2 do
    let w = ndfs.(i) in
    List.iter (fun v ->
      let u = eval v in
      if sdno.(u) < sdno.(w) then sdno.(w) <- sdno.(u);)
      predecessors.(w);
    B.set bucket.(ndfs.(sdno.(w))) w true;
    link parent.(w) w;
    while not (B.empty bucket.(parent.(w))) do
      let v =
	match B.toList bucket.(parent.(w)) with
	  x :: _ -> x
	| [] -> ignore(print_string "Error in dominfast");0 in
      B.set bucket.(parent.(w)) v false;
      let u = eval v in
      idom.(v) <- if sdno.(u) < sdno.(v) then u else parent.(w);
    done;
  done;
  
  for i=2 to !n do
    let w = ndfs.(i) in
    if idom.(w) <> ndfs.(sdno.(w)) then begin 
      let newDom = idom.(idom.(w)) in
      idom.(w) <- newDom;
    end
  done;
  idom
    
    
    
    
    
let dominance_frontier (flowgraph: cfgInfo) : dfInfo = 
  let idom = compute_idom flowgraph in
  let size = flowgraph.size in
  let children = Array.create size [] in 
  for i = 0 to size - 1 do
    if (idom.(i) != -1) then children.(idom.(i)) <- i :: children.(idom.(i));
  done; 

  let size  = flowgraph.size in
  let start  = flowgraph.start in
  let successors = flowgraph.successors in
  let predecessors = flowgraph.predecessors in
  
  let nodeSet = B.init size (fun i -> true) in (* and a set with those elements *)
  
  let df = Array.create size [] in
                                        (* Compute the dominance frontier  *)
  
  let bottom = Array.make size true in  (* bottom of the dominator tree *)
  for i = 0 to size - 1 do if (i != start) then bottom.(idom.(i)) <- false; done;
  let processed = Array.make size false in (* to record the nodes added to work_list *) 
  let workList = ref ([]) in (* to iterate in a bottom-up traversal of the dominator tree *)
  for i = 0 to size - 1 do 
    if (bottom.(i)) then workList := i :: !workList; 
  done;
  while (!workList != []) do
    let x = List.hd !workList in
    let update y = if idom.(y) <> x then df.(x) <- y::df.(x) in
                                        (* compute local component *)
    
(* We use whichPred instead of whichSucc because ultimately this info is 
 * needed by control dependence dag which is constructed from REVERSE 
 * dominance frontier *)
    List.iter (fun succ -> update succ) successors.(x);
                                        (* add on up component *)
    List.iter (fun z -> List.iter (fun y  -> update y) df.(z)) children.(x);
    processed.(x) <- true;
    workList := List.tl !workList;
    if (x != start) then begin
      let i = idom.(x) in
      if (List.for_all (fun child -> processed.(child)) children.(i)) then workList := i :: !workList; 
    end;
  done;
  df
    
    
(* Computes for each register, the set of nodes that need a phi definition 
 * for the register *)
    
let add_phi_functions_info (flowgraph: cfgInfo) : unit = 
  let df = dominance_frontier flowgraph in
  let size  = flowgraph.size in
  let nrRegs = flowgraph.nrRegs in 
  

  let defs = Array.init size (fun i -> B.init nrRegs (fun j -> false)) in 
  for i = 0 to size-1 do 
    List.iter 
      (fun (lhs,rhs) ->
        List.iter (fun (r: reg) -> B.set defs.(i) r true) lhs;
      ) 
      flowgraph.blocks.(i).instrlist
  done;
  let iterCount = ref 0 in
  let hasAlready = Array.create size 0 in 
  let work = Array.create size 0 in 
  let w = ref ([]) in 
  let dfPlus = Array.init nrRegs (
    fun i -> 
      let defIn = B.make size in
      for j = 0 to size - 1 do 
	if B.get defs.(j) i then B.set defIn j true
      done;
      let res = ref [] in 
      incr iterCount;
      B.iter (fun x -> work.(x) <- !iterCount; w := x :: !w;) defIn;
      while (!w != []) do 
	let x = List.hd !w in
	w := List.tl !w;
	List.iter (fun y -> 
	  if (hasAlready.(y) < !iterCount) then begin
	    res := y :: !res;
	    hasAlready.(y) <- !iterCount;
	    if (work.(y) < !iterCount) then begin
	      work.(y) <- !iterCount;
	      w := y :: !w;
	    end;
	  end;
		  ) df.(x)
      done;
      (* res := List.filter (fun blkId -> B.get liveIn.(blkId) i) !res; *)
      !res
   ) in
  let result = Array.create size ([]) in
  for i = 0 to nrRegs - 1 do
    List.iter (fun node -> result.(node) <- i::result.(node);) dfPlus.(i) 
  done;
(* result contains for each node, the list of variables that need phi 
 * definition *)
  for i = 0 to size-1 do
    flowgraph.blocks.(i).livevars <- 
      List.map (fun r -> (r, i)) result.(i);
  done
    
  
    
(* add dominating definitions info *)
    
let add_dom_def_info (f: cfgInfo): unit = 
  let blocks = f.blocks in
  let start = f.start in
  let size = f.size in
  let nrRegs = f.nrRegs in

  let idom = compute_idom f in
  let children = Array.create size [] in 
  for i = 0 to size - 1 do
    if (idom.(i) != -1) then children.(idom.(i)) <- i :: children.(idom.(i));
  done; 
  
  (* For each variable, maintain a stack of blocks that define it. When you 
   * process a block, the top of the stack is the closest dominator that 
   * defines the variable *)
  let s = Array.make nrRegs ([start]) in 
  
  let rec search (x: int): unit = (* x is a graph node *)
    (* Push the current block for the phi variables *)
    List.iter 
      (fun ((r: reg), dr) -> 
        if x = dr then s.(r) <- x::s.(r)) 
      blocks.(x).livevars;

    (* Clear livevars *)
    blocks.(x).livevars <- [];
    
    (* Compute livevars *)
    for i = 0 to nrRegs-1 do
      match s.(i) with 
      | [] -> assert false
      | fst :: _ -> 
          blocks.(x).livevars <- (i, fst) :: blocks.(x).livevars
    done;

    (* Update s for the children *)
    List.iter 
      (fun (lhs,rhs) ->
	List.iter (fun (lreg: reg) -> s.(lreg) <- x::s.(lreg) ) lhs; 
      ) 
      blocks.(x).instrlist;
    
        
    (* Go and do the children *)
    List.iter search children.(x);
    
    (* Then we pop x, whenever it is on top of a stack *)
    Array.iteri (fun i istack -> 
      match istack with 
        x' :: rest when x' = x -> s.(i) <- rest
      | _ -> ()) s;
  in
  search(start)
  
    
let add_ssa_info (f: cfgInfo): unit = 
  add_phi_functions_info f;
  add_dom_def_info f;
