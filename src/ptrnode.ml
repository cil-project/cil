

(* Implements nodes in a graph representing the pointer locations in a 
 * program *)
open Cil
open Pretty

module H = Hashtbl

(* A place where a pointer type can occur *)
type place = 
    PGlob of string  (* A global variable or a global function *)
  | PType of string  (* A global typedef *)
  | PStatic of string * string (* A static variable or function. Second is 
                                * the filename in which it occurs *)
  | PLocal of string * string * string (* A local name, the name of the 
                                        * file, the function and the name of 
                                        * the local itself *)
  | POffset of int * string             (* An offset node, give the host node 
                                         * id and a field name *)
  | PField of fieldinfo


(* Each node corresponds to a place in the program where a qualifier for a 
 * pointer type could occur. As a special case we also add qualifiers for the 
 * global variables or for those whose address is taken. *)
type node = 
    {         id: int;                  (* A program-wide unique identifier *)
              where: place * int;       (* A way to identify where is this 
                                         * coming from. We use this to make 
                                         * sure we do not create duplicate 
                                         * nodes. *)

              btype: typ;               (* The base type of ths pointer *)
      mutable attr: attribute list;     (* The attributes of this pointer 
                                         * type *)

      mutable onStack: bool;            (* Whether might contain stack 
                                         * addresses *)
      mutable updated: bool;            (* Whether it is used in a write 
                                         * operation *)
      mutable arith: exp list;          (* A list of expressions that are 
                                         * _added_ to this one *)
      mutable index: bool;              (* Whether this is used as an array 
                                         * base address in a [e] operation. 
                                         * Is just like arith but you know 
                                         * that the programmer intends e to 
                                         * be positive *)
      mutable null: bool;               (* The number 0 might be stored in 
                                         * such a this pointer  *)
      mutable intcast: bool;            (* Some integer other than 0 is 
                                         * stored in this pointer *)

      mutable succ: edge list;          (* All edges with "from" this node *)
      mutable pred: edge list;          (* All edges with "to" this node *)

    }       
   

and edge = 
    { mutable efrom:    node;
      mutable eto:      node;

      (* It would be nice to add some reason why this edge was added *)
    } 


(* A mapping from place * index to ids *)
let placeId: (place * int, node) H.t = H.create 1111

(* A mapping from ids to nodes *)
let idNode: (int, node) H.t = H.create 1111

(* Next identifier *)
let nextId = ref 0

let initialize () = 
  H.clear placeId;
  H.clear idNode;
  nextId := 0


(* Make a new node *)
let newNode (p: place) (idx: int)  (bt: typ) (a: attribute list) : node =
  let where = p, idx in
  incr nextId;
  let n = { id = !nextId;
            btype   = bt;
            attr    = addAttribute (ACons("_ptrnode", [AInt !nextId])) a;
            where   = where;
            onStack = false;
            updated = false;
            arith   = [];
            index   = false;
            null    = false;
            intcast = false;
            succ = [];
            pred = []; } in
  H.add placeId where n;
  H.add idNode n.id n;
  n
  
  
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


let addEdge (start: node) (dest: node) = 
  let nedge = 
    { efrom = start; eto= dest } in
  start.succ <- nedge :: start.succ;
  dest.pred <- nedge :: dest.pred




(* Print the graph *)
let d_place () = function
    PGlob s -> dprintf "Glob(%s)" s
  | PType s -> dprintf "Type(%s)" s
  | PStatic (f, s) -> dprintf "Static(%s.%s)" f s
  | PLocal (f, func, s) -> dprintf "Local(%s.%s.%s)" f func s
  | POffset (nid, fld) -> dprintf "Offset(%d, %s)" nid fld
  | PField(fi) -> dprintf "Field(%s)" fi.fname

let d_placeidx () (p, idx) = 
  dprintf "%a.%d" d_place p idx

let d_node n = 
  dprintf "%d : %a (%s%s%s%s)@!  S=@[%a@]@!  P=@[%a@]@!" 
    n.id d_placeidx n.where
    (if n.onStack then "stack," else "")
    (if n.updated then "upd," else "")
    (if n.index then "idx," else "")
    (if n.null  then "null," else "")
    (docList (chr ',' ++ break)
       (fun e -> num e.eto.id)) n.succ
    (docList (chr ',' ++ break)
       (fun e -> num e.efrom.id)) n.pred



let printGraph (c: out_channel) = 
  (* Get the nodes ordered by ID *)
  let all : node list ref = ref [] in
  H.iter (fun id n -> all := n :: !all) idNode;
  let allsorted = 
    List.sort (fun n1 n2 -> compare n1.id n2.id) !all in
  List.iter (fun n -> fprint c 80 (d_node n)) allsorted
    
