

(* Implements nodes in a graph representing the pointer locations in a 
 * program *)
open Cil
module H = Hashtbl

(* A place where a pointer type can occur *)
type place = 
    PGlob of string  (* A global variable or a global function *)
  | PType of string  (* A global typedef *)
  | PStatic of string * string (* A static variable or function. Second is 
                                * the filename in which it occurs *)
  | PLocal of string * string * string (* A local name, the name of the 
                                        * function and the name of the file 
                                        * in which it occurs *)


type node = 
    {         id: int;
              where: place * int;
              typ: typ;             (* The type *)

      mutable onStack: bool;            (* Whether might contain stack 
                                         * addresses *)
      mutable updated: bool;            (* Whether it is used in a write 
                                         * operation *)
      mutable escapes: bool;            (* Whether it is stored in a global 
                                         * or in memory *)

      mutable castTo: typ list;

      mutable succ: node list;          (* Successors *)
      mutable pred: node list;          (* Predecessors *)
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
let newNode (p: place) (idx: int) (t: typ) : node =
  let where = p, idx in
  incr nextId;
  let n = { id = !nextId;
            where = where;
            typ = t;
            onStack = false;
            updated = false;
            escapes = false;
            castTo  = [];
            succ = [];
            pred = []; } in
  H.add placeId where n;
  H.add idNode n.id n;
  n
  
  
(* Get a node for a place and an index *)
let getNode (p: place) (idx: int) (t: typ) : node = 
  (* See if exists already *)
  let where = (p, idx) in
  try
    H.find placeId where
  with Not_found -> newNode p idx t




(* Set some fields *)
let setEscapes: node -> unit = 
  fun n -> n.escapes <- true

let setUpdated: node -> unit =
  fun n -> n.updated <- true

let setCastTo: node -> typ -> unit = 
  fun n t -> n.castTo <- t :: n.castTo


(* Model n := m *)
let setAssigned (n: node)  (m: node) : unit = 
  let rec insertUnique what = function
      [] -> [what]
    | (x :: rest) as l ->
        if x.id = what.id then l
        else if x.id > what.id then what :: l
        else x :: insertUnique what rest
  in
  n.pred <- insertUnique m n.pred;
  m.succ <- insertUnique n m.succ
    




