(* If defaultIsNotWild then pointers without a qualifier are SAFE and only 
 * the arrays that are specfically SIZED contain a size field and only the 
 * variables that are specifically TAGGED contain tags *)
val defaultIsWild: bool ref


(* A marker that the solver places, if we use lean fats *)
val useLeanFats: bool ref


val externPoly: bool ref
val allPoly: bool ref

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
  | PField of Cil.fieldinfo             (* A field of a composite type *)

  | PAnon of int                        (* Anonymous. This one must use a 
                                         * fresh int every time. Use 
                                         * anonPlace() to create one of these 
                                         * *)

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

              btype: Cil.typ;           (* The base type of this pointer *)
      mutable attr: Cil.attribute list; (* The attributes of this pointer 
                                         * type *)

      mutable flags: int; 

      mutable mustHaveEnd: bool;        (* If this is SAFE at the very end, 
                                         * make it FSEQ *)

      mutable succ: edge list;          (* All edges with "from" = this node *)
      mutable pred: edge list;          (* All edges with "to" = this node *)

      mutable pointsto: node list;      (* A list of nodes whose types are 
                                         * pointed to by this pointer type. 
                                         * This is needed because we cannot 
                                         * have wild pointers to memory 
                                         * containing safe pointers. *)
      
      (* The rest are the computed results of constraint resolution *)
      mutable kind: opointerkind;
      mutable why_kind: whykind;
      mutable sized: bool ;            (* An array may be SIZED at which
                                         * point it has a length field
                                         * stored right before it. This
                                         * leads to INDEX pointers. *)
      
      mutable locked: bool;            (* do not change this kind later *)
      mutable mark: bool;               (* For mark-and-sweep GC of nodes. 
                                         * Most of the time is false *)
    }       

and opointerkind = 
    Safe
  | Scalar (* not actually a pointer *)
  | Seq    (* A three word pointer, like Index but with the length in the 
            * pointer itself *)
  | FSeq

  | SeqN   (* A sequence in a null-terminated char array *)
  | FSeqN  (* A FSeq in a null-terminated char array *)

  | String (* fseq <= string <= fseq *)
  | ROString (* string->rostring *)

  | Index
  | Wild

  | WildT
  | SeqT
  | FSeqT
  | SeqNT
  | FSeqNT
  | IndexT

  | Unknown

and pkind = 
  | KUnknown
  | KScalar
  | KSafe   of int32
  | KString of int32
  | KSeq    of int32
  | KWild   of int32

and whykind = (* why did we give it this kind? *)
    BadCast of edge
  | PolyCast of edge
  | SpreadFromEdge of node 
  | SpreadPointsTo of node
  | SpreadToArrayFrom of node
  | BoolFlag
  | Default
  | UserSpec
  | Unconstrained
  | PrintfArg (* printf inference *)
  | Special of string * Cil.location

and edge = 
    { mutable efrom:    node;
      mutable eto:      node;
      mutable ekind:    edgekind;
      mutable ecallid:   int;(* Normnally -1. Except if this edge is added 
                              * because of a call to a function (either 
                              * passing arguments or getting a result) then 
                              * we put a program-unique callid, to make it 
                              * possible later to do push-down verification *)
      mutable eloc: Cil.location;
      (* It would be nice to add some reason why this edge was added, to 
       * explain later to the programmer.  *)
    } 
      

and edgekind = 
    ECast                    (* T_from ref q_from <= T_to ref q_to *)
  | ESafe                    (* q_to = if q_from = wild then wild else safe *)
  | EIndex                   (* q_to = if q_from = wild then wild else index *)
  | ENull                    (* a NULL flows in the direction of the edge *)
  | ECompat                  (* the kinds of these two nodes must be
                              * compatible: either both wild, index or
                              * safe. This edge type is added by the solver
                              * for its convenience. In cases like
                              * int * 1 * 2 x; 
                              * int * 3 * 4 y;
                              * We will connect 1 and 3 with ECompat. *)
  | EPolyCast                (* This is a edge that is added in a polymorphic 
                              * function call or return. *)

val setFlag : node -> int -> unit
val hasFlag : node -> int -> bool
val pkInterface : int (* this is an interface node *)
val pkUpdated : int (* we write through this pointer *)
val pkOnStack : int (* can contain a stack address *)
val pkNull : int (* can be NULL *)
val pkIntCast : int (* can contain an integer *)
val pkPosArith : int (* subject to positive pointer arithmetic *)
val pkArith : int (* subject to arbitrary pointer arithmetic *)
val pkReachString : int (* can reach a String node *)
val pkReachIndex : int (* can reach an Index node *)
val pkReachSeq : int (* can reach a Seq node *)
val pkNoPrototype : int (* Used as actual argument in a function without 
                              * prototype *)
val pkEscape : int (* can "escape" i.e. be assigned to heap memory *)

(* The main graph *)
val idNode: (int, node) Hashtbl.t

val dummyNode: node (* A node with ID=0. Use when you don't have one *)

(* Get a node for a place and an index. Give also the base type and the 
 * attributes *)
val getNode: p:place -> idx:int -> bt:Cil.typ -> al:Cil.attribute list -> node

(* Make a new node *)
val newNode: p:place -> idx:int -> bt:Cil.typ -> al:Cil.attribute list -> node


(* Make a new anonymous place *)
val anonPlace: unit -> place

(* Alternative definitions for gflags *)
val pkReachString: int
val pkReachSeq: int
val pkReachIndex: int
val pkInterface: int
val pkUpdated: int
val pkArith: int
val pkPosArith: int
val pkIntCast: int
val pkNull: int

val pkCastPredFlags: int
val pkCastSuccFlags: int
val pkCNIPredFlags: int

val setFlag: node -> int -> unit
val hasFlag: node -> int -> bool

val d_opointerkind: unit -> opointerkind -> Pretty.doc
val d_whykind: unit -> whykind -> Pretty.doc
val d_node: unit -> node -> Pretty.doc
val d_place: unit -> place -> Pretty.doc
val d_pkind: unit -> pkind -> Pretty.doc

val ptrAttrCustom: printnode:bool -> Cil.attribute -> Pretty.doc option

val printGraph: out_channel -> unit
val printGraphStats: unit -> unit

(* Add an edge to the graph *)
val addEdge: start:node -> dest:node -> kind:edgekind -> callid:int -> unit


val nodeOfAttrlist: Cil.attribute list -> node option
val kindOfAttrlist: Cil.attribute list -> opointerkind * whykind


(* Replace the ptrnode attribute with the actual qualifier attribute *)
type whichAttr = 
    AtPtr  (* In a pointer type *)
  | AtArray  (* In an array type *)
  | AtOpenArray (* In an array type without a size *)
  | AtVar (* For a variable *)
  | AtOther (* Anything else *)

val replacePtrNodeAttrList: which:whichAttr 
                           ->  Cil.attribute list -> Cil.attribute list

(* Checks if a pointer kind has the TABLE flag *)
val isT: opointerkind -> bool

(* Add the TABLE flag to a pointer kind *)
val addT: opointerkind -> opointerkind

(* Remove the TABLE flag from a pointer kind *)
val stripT: opointerkind -> opointerkind


val k2attr: opointerkind -> Cil.attribute
