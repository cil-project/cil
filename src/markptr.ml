(* A module that scans a CIL file and tags all TPtr with a unique attribute 
 * id. It also constructs a mapping from attributes id to places where they 
 * were introduced  *)
open Cil
open Pretty

module H = Hashtbl

module N = Nodes

(* A place where a pointer type can occur *)
type place = 
    PGlob of string  (* A global variable or a global function *)
  | PType of string  (* A global typedef *)
  | PStatic of string * string (* A static variable or function. Second is 
                                * the filename in which it occurs *)
  | PLocal of string * string * string (* A local name, the name of the 
                                        * function and the name of the file 
                                        * in which it occurs *)



let idArray : N.node Array.t = Array.empty ()
let idMap : (place * int, int) H.t = H.create 117

let nextId = ref 0
let getId (p: place) (idx: int) = 
  (* See if it exists already *)
  try
    H.find idMap (p, idx)
  with Not_found -> begin
    incr nextId;
    
  end
