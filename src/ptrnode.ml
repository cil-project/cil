

(* Implements nodes in a graph representing the pointer locations in a 
 * program *)
open Cil
open Pretty

module H = Hashtbl
module E = Errormsg

(* A place where a pointer type can occur *)
type place = 
    PGlob of string  (* A global variable or a global function *)
  | PType of string  (* A global typedef *)
  | PStatic of string * string (* A static variable or function. First is  
                                * the filename in which it occurs *)
  | PLocal of string * string * string (* A local varialbe. The name of the 
                                        * file, the function and the name of 
                                        * the local itself *)
  | POffset of int * string             (* An offset node, give the host node 
                                         * id and a field name *)
  | PField of fieldinfo                 (* A field of a composite type *)

  | PAnon of int                        (* Anonymous. This one must use a 
                                         * fresh int every time. Use 
                                         * anonPlace() to create one of these 
                                         * *)

let anonId = ref (-1) 
let anonPlace () : place = 
  incr anonId;
  PAnon !anonId

(* Each node corresponds to a place in the program where a qualifier for a 
 * pointer type could occur. As a special case we also add qualifiers for 
 * variables in expectation that their address might be taken *)
type node = 
    {         id: int;                  (* A program-wide unique identifier *)
              where: place * int;       (* A way to identify where is this 
                                         * coming from. We use this to make 
                                         * sure we do not create duplicate 
                                         * nodes. The integer is an index 
                                         * within a place, such as if the 
                                         * type of a global contains several 
                                         * pointer types (nested) *)

              btype: typ;               (* The base type of this pointer *)
      mutable attr: attribute list;     (* The attributes of this pointer 
                                         * type *)

      mutable onStack: bool;            (* Whether might contain stack 
                                         * addresses *)
      mutable updated: bool;            (* Whether it is used in a write 
                                         * operation *)
      mutable posarith: bool;           (* Whether this is used as an array 
                                         * base address in a [e] operation or 
                                         * obviously positive things are 
                                         * added to it. We assume that the 
                                         * programmer uses the notation [e] 
                                         * to indicate positive indices e. *)
      mutable arith: bool;              (* Whenever things are added to this 
                                         * pointer, but we don't know that 
                                         * they are positive *)
      mutable null: bool;               (* The number 0 might be stored in 
                                         * this pointer  *)
      mutable intcast: bool;            (* Some integer other than 0 is 
                                         * stored in this pointer *)

      mutable succ: edge list;          (* All edges with "from" = this node *)
      mutable pred: edge list;          (* All edges with "to" = this node *)

      mutable pointsto: node list;      (* A list of nodes whose types are 
                                         * pointed to by this pointer type. 
                                         * This is needed because we cannot 
                                         * have wild pointers to memory 
                                         * containing safe pointers. *)


      (* The rest are the computed results of constraint resolution *)
      mutable kind: pointerkind;
      mutable why_kind : whykind;
      
      mutable mark: bool;               (* For mark-and-sweep GC of nodes. 
                                         * Most of the time is false *)
    }       
   

and pointerkind = 
    Safe
  | FSeq
  | BSeq
  | String
  | Index
  | Wild
  | Unknown

and whykind = (* why did we give it this kind? *)
    BadCast of typ * typ
  | SpreadFromEdge of node 
  | SpreadPointsTo of node
  | BoolFlag
  | Default
  | UserSpec

and edge = 
    { mutable efrom:    node;
      mutable eto:      node;
      mutable ekind:    edgekind;
      mutable ecallid:   int;(* Normnally -1. Except if this edge is added 
                              * because of a call to a function (either 
                              * passing arguments or getting a result) then 
                              * we put a program-unique callid, to make it 
                              * possible later to do push-down verification *)

      (* It would be nice to add some reason why this edge was added, to 
       * explain later to the programmer.  *)
    } 
      

and edgekind = 
    ECast                    (* T_from ref q_from <= T_to ref q_to *)
  | ESafe                    (* q_to = if q_from = wild then wild else safe *)
  | EIndex                   (* q_to = if q_from = wild then wild else index *)


(* Print the graph *)
let d_place () = function
    PGlob s -> dprintf "Glob(%s)" s
  | PType s -> dprintf "Type(%s)" s
  | PStatic (f, s) -> dprintf "Static(%s.%s)" f s
  | PLocal (f, func, s) -> dprintf "Local(%s.%s.%s)" f func s
  | POffset (nid, fld) -> dprintf "Offset(%d, %s)" nid fld
  | PField(fi) -> dprintf "Field(%s)" fi.fname
  | PAnon id -> dprintf "Anon(%d)" id

let d_placeidx () (p, idx) = 
  dprintf "%a.%d" d_place p idx

let d_pointerkind () = function
    Safe -> text "safe"
  | FSeq -> text "fseq" 
  | BSeq -> text "bseq"
  | String -> text "string" 
  | Index -> text "index"
  | Wild -> text "wild" 
  | Unknown -> text "unknown" 

let d_ekind () = function
    ECast -> text "Cast"
  | ESafe -> text "Safe"
  | EIndex -> text "Index"

let d_whykind () = function
    BadCast(t1,t2) -> dprintf "cast(%a<= %a)" d_type t1 d_type t2
  | BoolFlag -> text "from_flag"
  | SpreadFromEdge(n) -> dprintf "spread_from_edge(%d)" n.id
  | SpreadPointsTo(n) -> dprintf "spread_points_to(%d)" n.id
  | Default -> text "by_default"
  | UserSpec -> text "user_spec"

let d_node n = 
  dprintf "%d : %a (%s%s%s%s%s%s) (@[%a@])@! K=%a/%a T=%a@!  S=@[%a@]@!  P=@[%a@]@!" 
    n.id d_placeidx n.where
    (if n.onStack then "stack," else "")
    (if n.updated then "upd," else "")
    (if n.posarith then "posarith," else "")
    (if n.arith then "arith," else "")
    (if n.null  then "null," else "")
    (if n.intcast  then "int," else "")
    (docList (chr ',' ++ break)
       (fun n -> num n.id)) n.pointsto
    d_pointerkind n.kind
    d_whykind n.why_kind
    d_type n.btype
    (docList (chr ',' ++ break)
       (fun e -> dprintf "%d:%a%a" e.eto.id
           d_ekind e.ekind
           insert (if e.ecallid >= 0 then dprintf "(%d)" e.ecallid else nil)))
    n.succ
    (docList (chr ',' ++ break)
       (fun e -> dprintf "%d:%a%a" e.efrom.id
           d_ekind e.ekind
           insert (if e.ecallid >= 0 then dprintf "(%d)" e.ecallid else nil)))
    n.pred
    

(* A mapping from place , index to ids. This will help us avoid creating 
 * duplicate nodes *)
let placeId: (place * int, node) H.t = H.create 1111

(* A mapping from ids to nodes. Rarely we need to find a node based on its 
 * index. *)
let idNode: (int, node) H.t = H.create 1111

(* Next identifier *)
let nextId = ref (-1)

let initialize () = 
  H.clear placeId;
  H.clear idNode;
  nextId := -1


let printGraph (c: out_channel) = 
  (* Get the nodes ordered by ID *)
  let all : node list ref = ref [] in
  H.iter (fun id n -> all := n :: !all) idNode;
  let allsorted = 
    List.sort (fun n1 n2 -> compare n1.id n2.id) !all in
  printShortTypes := true;
  List.iter (fun n -> fprint c 80 (d_node n)) allsorted;
  printShortTypes := false
       
(* Add a new points-to to the node *)
let addPointsTo n n' = 
  n.pointsto <- n' :: n.pointsto

let nodeOfAttrlist al = 
  match filterAttributes "_ptrnode" al with
    [] -> None
  | [ACons(_, [AInt n])] -> begin
      try Some (H.find idNode n)
      with Not_found -> E.s (E.bug "Cannot find node with id = %d\n" n)
  end
  | _ -> E.s (E.bug "nodeOfAttrlist")


let kindOfAttrlist al = 
  let rec loop = function
      [] -> Unknown, Default
    | a :: al -> begin
        match a with
          AId "safe" -> Safe, UserSpec
        | AId "index" -> Index, UserSpec
        | AId "seq" -> FSeq, UserSpec
        | AId "wild" -> Wild, UserSpec
        | ACons("_ptrnode", [AInt n]) -> begin
            (* See if there is a UserSpec *)
            match loop al with
              x, UserSpec -> x, UserSpec
            | _ -> begin
                let nd = 
                  try H.find idNode n
                  with Not_found -> 
                    E.s (E.bug "Cannot find node with id = %d\n" n)
                in
                nd.kind, nd.why_kind
            end
        end
        | _ -> loop al
    end    
  in
  loop al
    
(* Make a new node *)
let newNode (p: place) (idx: int) (bt: typ) (a: attribute list) : node =
  let where = p, idx in
  incr nextId;
  let n = { id = !nextId;
            btype   = bt;
            attr    = addAttribute (ACons("_ptrnode", [AInt !nextId])) a;
            where   = where;
            onStack = false;
            updated = false;
            arith   = false;
            posarith= false;
            null    = false;
            intcast = false;
            succ = [];
            kind = Safe;
            why_kind = Default; 
            pointsto = [];
            mark = false;
            pred = []; } in
(*  ignore (E.log "Created new node(%d) at %a\n" n.id d_placeidx where); *)
  H.add placeId where n;
  H.add idNode n.id n;
  (* Now set the pointsto nodes *)
  let _ =
    let doOneType = function
        TPtr (_, a) as t -> 
          (match nodeOfAttrlist a with
            Some n' -> addPointsTo n n'
          | None -> ());
          ExistsFalse

      | _ -> ExistsMaybe
    in
    existsType doOneType n.btype
  in
  n
    
  
let dummyNode = newNode (PGlob "@dummy") 0 voidType []

(* Get a node for a place and an index. Give also the base type and the 
 * attributes *)
let getNode (p: place) (idx: int) (bt: typ) (a: attribute list) : node = 
  (* See if exists already *)
  let where = (p, idx) in
  try
    H.find placeId where
  with Not_found -> newNode p idx bt a


let nodeExists (p: place) (idx: int) = 
  H.mem placeId (p, idx)


let addEdge (start: node) (dest: node) (kind: edgekind) (callid: int) = 
  if start != dummyNode && dest != dummyNode then begin
    let nedge = 
      { efrom = start; eto= dest; ekind = kind; ecallid = callid; } in
    start.succ <- nedge :: start.succ;
    dest.pred <- nedge :: dest.pred
  end


let removeSucc n sid = 
  n.succ <- List.filter (fun e -> e.eto.id <> sid) n.succ

let removePred n pid = 
  n.pred <- List.filter (fun e -> e.efrom.id <> pid) n.pred

let ptrAttrCustom oldCustom = function
      ACons("_ptrnode", [AInt n]) -> Some (dprintf "NODE(%d)" n)
    | AId("_ronly") -> Some (text "RONLY")
    | AId("_safe") -> Some (text "SAFE")
    | AId("_seq") -> Some (text "SEQ")
    | AId("_index") -> Some (text "INDEX")
    | AId("_stack") -> Some (text "STACK")
    | AId("_opt") -> Some (text "OPT")
    | AId("_wild") -> Some (text "WILD")
    | a -> oldCustom a


(**** Garbage collection of nodes ****)
(* I guess it is safe to call this even if you are not done with the whole 
 * program. Some not-yet-used globals will be collected but they will be 
 * regenerated later if needed *)
let gc () = 
  (* A list of all the nodes *)
  let all : node list ref = ref [] in
  H.iter (fun id n -> all := n :: !all) idNode;
  (* Scan all the nodes. The roots are globals with successors or 
   * predecessors *)
  let rec scanRoots n = 
    match n.where with
      (PGlob _, _) | (PStatic _, _) 
        when n.succ <> [] || n.pred <> [] -> scanOneNode n
    | _ -> ()
  and scanOneNode n = 
    if n.mark then ()
    else begin
      (* Do not mark the Offset nodes that have no successor and their only 
       * predecessor is the parent *)
      let keep =
        match n.where with
          (POffset (pid, _), _) -> 
            (match n.succ, n.pred with 
              [], [p] when p.efrom.id = pid -> false
            | _ -> true)
        | _ -> true
      in
      if keep then begin
        n.mark <- true;
        List.iter (fun se -> scanOneNode se.eto) n.succ;
        List.iter (fun se -> scanOneNode se.efrom) n.pred;
        List.iter scanOneNode n.pointsto
      end
    end
  in
  List.iter scanRoots !all;
  (* Now go over all nodes and delete those that are not marked *)
  List.iter 
    (fun n -> 
      if not n.mark then begin
        H.remove idNode n.id;
        H.remove placeId n.where;
        (* Remove this edge from all predecessors that are kept *)
        List.iter 
          (fun ep -> 
            let p = ep.efrom in 
            if p.mark then removeSucc p n.id) n.pred;
        List.iter
          (fun es -> 
            let s = es.eto in
            if s.mark then removePred s n.id) n.succ;
      end) !all;
  (* Now clear the mark *)
  List.iter (fun n -> n.mark <- false) !all
        
      
(** Graph Simplification **)
(* Collapse nodes which:
 *  (1) have only 1 predecessor edge
 *  (2) have only 1 successor edge
 *  (3) successor edge type = predecessor edge type
 *  (4) are the 0th node of a Local
 *)
let simplify () = 
  (* A list of all the nodes *)
  let examine_node id n = begin
    match n.where with
      (PAnon(_),0) | 
      (PLocal(_,_,_),1) -> if ((List.length n.succ) = 1) && 
                          ((List.length n.pred) = 1) && 
                          ((List.hd n.succ).ekind = (List.hd n.pred).ekind)
                          then begin
        (* we can remove this node *)
        let s = (List.hd n.succ).eto in
        let p = (List.hd n.pred).efrom in
        let k = (List.hd n.succ).ekind in
        removePred s id ;
        removeSucc p id ;
        addEdge p s k (-1) ;
        H.remove idNode n.id;
        H.remove placeId n.where;
      end
    | _ -> ()
  end in
  H.iter examine_node idNode 
