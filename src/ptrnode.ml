

(* Implements nodes in a graph representing the pointer locations in a 
 * program *)
open Cil
open Pretty
open Int32

module H = Hashtbl
module E = Errormsg

(* If defaultIsNotWild then pointers without a qualifier are SAFE and only 
 * the arrays that are specfically SIZED contain a size field and only the 
 * variables that are specifically TAGGED contain tags *)
let defaultIsWild  = ref false


(* A marker that the solver places, if we use lean fats *)
let useLeanFats = ref false

(* If allPoly is true then all un-defined functions are treated 
 * polymorphically *)
let allPoly = ref false


let externPoly = ref false

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

      mutable flags : int; 

(* 
      mutable flagsCastPred: int; (* cast_pred: updated *)
      mutable flagsCNIPred: int;
      mutable flagsCastSucc: int ; 
      *)
      
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

      mutable noPrototype: bool;        (* Used as an argument in a function 
                                         * without prototype or a function 
                                         * what is invoked with more 
                                         * arguments than it has declared *)
      mutable succ: edge list;          (* All edges with "from" = this node *)
      mutable pred: edge list;          (* All edges with "to" = this node *)

      mutable pointsto: node list;      (* A list of nodes whose types are 
                                         * pointed to by this pointer type. 
                                         * This is needed because we cannot 
                                         * have wild pointers to memory 
                                         * containing safe pointers. *)
      mutable interface: bool;          (* Is part of the interface *)
      
      (* The rest are the computed results of constraint resolution *)
      mutable kind: opointerkind;
      mutable why_kind: whykind;
      mutable sized: bool ;            (* An array may be SIZED at which
                                         * point it has a length field
                                         * stored right before it. This
                                         * leads to INDEX pointers. *)
      
      mutable can_reach_string: bool;  (* used by the solvers *)
      mutable can_reach_seq: bool;     (* used by the solvers *)
      mutable can_reach_index: bool;   (* used by the solvers *)
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
  | Special of string * location

and edge = 
    { mutable efrom:    node;
      mutable eto:      node;
      mutable ekind:    edgekind;
      mutable ecallid:   int;(* Normnally -1. Except if this edge is added 
                              * because of a call to a function (either 
                              * passing arguments or getting a result) then 
                              * we put a program-unique callid, to make it 
                              * possible later to do push-down verification *)
      mutable eloc: location;
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


(* set a boolean bitflag *)
let setFlag n f = n.flags <- (n.flags lor f)
(* check a boolean bitflag *)
let hasFlag n f = (n.flags land f) <> 0 

let pkInterface = 1          (* this is an interface node *)
let pkUpdated = 2            (* we write through this pointer *)
let pkOnStack = 4            (* can contain a stack address *)
let pkNull = 8               (* can be NULL *)
let pkIntCast = 16           (* can contain an integer *)
let pkPosArith = 32          (* subject to positive pointer arithmetic *)
let pkArith = 64             (* subject to arbitrary pointer arithmetic *)
let pkReachString = 128      (* can reach a String node *)
let pkReachIndex = 256       (* can reach an Index node *)
let pkReachSeq = 512         (* can reach a Seq node *)
let pkNoPrototype = 1024     (* Used as actual argument in a function without 
                              * prototype *)

(* George may also want:
let pkNullTerm   = of_int 8  (* Points to a null-terminated buffer *)
let pkRegistered = of_int 16 (* The pointer always points to a memory 
                              * area that is registered  *)
(* Some representation flags *)
let pkLean       = of_int 32  (* The pointer is 1 word. It's capabilities, if 
                               * any, are registered. In that case it must 
                               * have the pkRegistered flag as well *)
let pkIndex      = of_int 64  (* The pointer is into a sized array. *)
let pkForward    = of_int 128 (* The pointer is into an array, and is moving 
                               * only forward *)
*)

(* These are bitmasks of flags. *) 
let pkCastPredFlags = (pkUpdated lor pkPosArith lor pkArith)
let pkCastSuccFlags = (pkOnStack lor pkNull lor pkIntCast)
let pkCNIPredFlags =  (pkReachString lor pkReachIndex lor pkReachSeq)

let pkIsWild = function
    KWild _ -> true | _ -> false

let pkIsSafe = function
    KSafe _ -> true | _ -> false

let pkWithout (pk: int32) (f: int32) = 
  logand pk (lognot f)

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

let d_opointerkind () = function
    Safe -> text "SAFE"
  | Scalar -> text "SCALAR"
  | FSeq -> text "FSEQ" 
  | FSeqN -> text "FSEQN" 
  | String -> text "STRING" 
  | ROString -> text "ROSTRING"
  | Index -> text "INDEX"
  | Seq -> text "SEQ"
  | SeqN -> text "SEQN"
  | Wild -> text "WILD" 
  | WildT -> text "WILDT" 
  | SeqT -> text "SEQT" 
  | FSeqT -> text "FSEQT" 
  | SeqNT -> text "SEQNT" 
  | FSeqNT -> text "FSEQNT" 
  | IndexT -> text "INDEXT"
  | Unknown -> text "UNKNOWN" 

let d_pkind () = function
    | KUnknown -> text "UNKNOWN"
    | KScalar -> text "SCALAR"
    | KSafe f -> text "SAFE"
    | KSeq f -> text "SEQ"
    | KWild f -> text "WILD"
    | KString f -> text "STRING"

let d_ekind () = function
    ECast -> text "Cast"
  | ESafe -> text "Safe"
  | EIndex -> text "Index"
  | ENull -> text "Null"
  | ECompat -> text "Compat" 

let d_whykind () = function
(*    BadCast(t1,t2) -> dprintf "cast(%a<= %a)" d_type t1 d_type t2
  | PolyCast(t1,t2) -> dprintf "polymorphic(%a<= %a)" d_type t1 d_type t2
*)
  | BadCast e -> 
      dprintf "cast(%a(%d) <= %a(%d)) at %a" 
        d_type e.eto.btype e.eto.id d_type e.efrom.btype e.efrom.id 
        d_loc e.eloc
  | PolyCast e -> 
      dprintf "polymorphic(%a (%d) <= %a(%d))" 
        d_type e.eto.btype e.eto.id d_type e.efrom.btype e.efrom.id
  | BoolFlag -> text "from_flag"
  | SpreadToArrayFrom(n) -> dprintf "spread_to_array_from(%d)" n.id
  | SpreadFromEdge(n) -> dprintf "spread_from_edge(%d)" n.id
  | SpreadPointsTo(n) -> dprintf "spread_points_to(%d)" n.id
  | Default -> text "by_default"
  | UserSpec -> text "user_spec"
  | Unconstrained -> text "unconstrained"
  | PrintfArg -> text "printf_arg"
  | Special (s, l) -> text (s ^ " at ") ++ d_loc () l

let d_node () n = 
(*
  dprintf "%d : %a (%s%s%s%s%s%s%s%s%s%s%s) (@[%a@])@! K=%a/%a T=%a@!  S=@[%a@]@!  P=@[%a@]@!" 
    n.id d_placeidx n.where
    (if n.onStack || hasFlag n pkOnStack then "stack," else "")
    (if n.updated || hasFlag n pkUpdated then "upd," else "")
    (if n.posarith || hasFlag n pkPosArith then "posarith," else "")
    (if n.arith || hasFlag n pkArith then "arith," else "")
    (if n.null || hasFlag n pkNull then "null," else "")
    (if n.intcast || hasFlag n pkIntCast then "int," else "")
    (if n.interface || hasFlag n pkInterface then "interf," else "")
    (if n.sized  then "sized," else "")
    (if n.can_reach_string || hasFlag n pkReachString then "reach_s," else "")
    (if n.can_reach_seq    || hasFlag n pkReachSeq    then "reach_q," else "")
    (if n.can_reach_index  || hasFlag n pkReachIndex  then "reach_i," else "")
    (docList (chr ',' ++ break)
       (fun n -> num n.id)) n.pointsto
    d_opointerkind n.kind
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
*)    
    num n.id 
     ++ text " : " 
     ++ d_placeidx () n.where
     ++ text " ("
     ++ text ((if n.onStack || hasFlag n pkOnStack then "stack," else "") ^
              (if n.updated || hasFlag n pkUpdated then "upd," else "") ^
              (if n.posarith || hasFlag n pkPosArith 
              then "posarith," else "") ^
              (if n.arith || hasFlag n pkArith then "arith," else "") ^
              (if n.null || hasFlag n pkNull then "null," else "") ^
              (if n.intcast || hasFlag n pkIntCast then "int," else "") ^
              (if n.noPrototype || hasFlag n pkNoPrototype 
              then "noproto," else "") ^
              (if n.interface || hasFlag n pkInterface 
              then "interf," else "") ^
              (if n.sized  then "sized," else "") ^
              (if n.can_reach_string || hasFlag n pkReachString 
              then "reach_s," else "") ^
              (if n.can_reach_seq    || hasFlag n pkReachSeq    
              then "reach_q," else "") ^
              (if n.can_reach_index  || hasFlag n pkReachIndex  
              then "reach_i," else ""))
    ++ text ") ("
    ++ (align 
          ++ (docList (chr ',' ++ break)
                (fun n -> num n.id) () n.pointsto)
          ++ unalign)
    ++ text ")"
    ++ line
    ++ text " K="
    ++ d_opointerkind () n.kind
    ++ text "/"
    ++ d_whykind () n.why_kind
    ++ text " T="
    ++ d_type () n.btype 
    ++ line
    ++ text "  S="
    ++ (align 
          ++ (docList (chr ',' ++ break)
                (fun e ->
                    num e.eto.id
                    ++ text ":"
                    ++ d_ekind () e.ekind
                    ++ (if e.ecallid >= 0 then 
                      text "(" ++ num e.ecallid ++ text ")" else nil))
                ()
                n.succ)
          ++ unalign)
    ++ line
    ++ text "  P="
    ++ (align 
          ++ (docList (chr ',' ++ break)
                (fun e ->
                    num e.efrom.id
                    ++ text ":"
                    ++ d_ekind () e.ekind
                    ++ (if e.ecallid >= 0 
                    then text "(" ++ num e.ecallid ++ text ")" else nil))
                ()
                n.pred)
          ++ unalign)
    ++ line

(* Convert the old kind into the new kind *)    
(*
let okind2kind = function
    Safe -> KSafe zero
  | Index -> KSeq pkIndex
  | Seq -> KSeq zero
  | SeqN -> KSeq pkNullTerm
  | SeqT -> KSeq pkLean
  | SeqNT -> KSeq (logor pkNullTerm pkLean)

  | FSeq -> KSeq pkForward
  | FSeqN -> KSeq (logor pkNullTerm pkForward)
  | FSeqT -> KSeq (logor pkLean pkForward)
  | FSeqNT -> KSeq (logor pkNullTerm (logor pkLean pkForward))

  | Wild -> KWild zero
  | WildT -> KWild pkLean

  | String -> KString zero
  | ROString -> KString pkROnly

  | k -> E.s (E.unimp "okind2kind: %a\n" d_opointerkind k)
  *)

(* Convert the new kind into the old kind *)    
(*
let pk2okind = function
    KSafe _ -> Safe
  | KSeq f -> 
      if pkHas f pkIndex then 
        if pkHas f pkLean then IndexT else Index
      else if pkHas f pkForward then 
        if pkHas f pkNullTerm then 
          if pkHas f pkLean then FSeqNT else FSeqN
        else
          if pkHas f pkLean then FSeqT else FSeq
      else
        if pkHas f pkNullTerm then 
          if pkHas f pkLean then SeqNT else SeqN
        else
          if pkHas f pkLean then SeqT else Seq
  | KString f -> 
      if pkHas f pkROnly then ROString else String
  | KWild f -> if pkHas f pkLean then WildT else Wild
  | KScalar -> Scalar
  | KUnknown -> Unknown
  *)

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
  let allsorted = 
    Stats.time "sortgraph"
      (fun () -> 
        let all : node list ref = ref [] in
        H.iter (fun id n -> all := n :: !all) idNode;
        List.sort (fun n1 n2 -> compare n1.id n2.id) !all) ()
  in
  printShortTypes := true;
  Stats.time "printnodes" 
    (List.iter (fun n -> fprint c 80 (d_node () n))) allsorted;
  printShortTypes := false
       
let nodeOfAttrlist al = 
  let findnode n =
    try Some (H.find idNode n)
    with Not_found -> E.s (E.bug "Cannot find node with id = %d\n" n)
  in
  match filterAttributes "_ptrnode" al with
    [] -> None
  | [Attr(_, [AInt n])] -> findnode n
  | (Attr(_, [AInt n]) :: _) as filtered -> 
      ignore (E.warn "nodeOfAttrlist(%a)" (d_attrlist true) filtered);
      findnode n
  | _ -> E.s (E.bug "nodeOfAttrlist")

(* Add a new points-to to the node *)
let addPointsTo n n' = 
  assert (n.id <> 0);
  n.pointsto <- n' :: n.pointsto

let addPointsToType (n: node) (t: typ) = 
  match nodeOfAttrlist (typeAttrs t) with
  | Some n' -> addPointsTo n n'
  | _ -> ()

let stripT = function
  | WildT -> Wild
  | SeqT -> Seq
  | FSeqT -> FSeq
  | SeqNT -> SeqN
  | FSeqNT -> FSeqN
  | IndexT -> Index
  | x -> x


let addT = function
  | Wild -> WildT
  | Seq -> SeqT
  | FSeq -> FSeqT
  | SeqN -> SeqNT
  | FSeqN -> FSeqNT
  | Index -> IndexT
  | x -> x


let isT k = stripT k <> k


let k2attr = function
    Safe -> Attr("safe", [])
  | Index -> Attr("index", [])
  | Wild -> Attr("wild", [])
  | Seq -> Attr("seq", [])
  | FSeq -> Attr("fseq", [])
  | SeqN -> Attr("seqn", [])
  | FSeqN -> Attr("fseqn", [])
  | IndexT -> Attr("indext", [])
  | WildT -> Attr("wildt", [])
  | SeqT -> Attr("seqt", [])
  | FSeqT -> Attr("fseqt", [])
  | SeqNT -> Attr("seqnt", [])
  | FSeqNT -> Attr("fseqnt", [])
  | String -> Attr("string", [])
  | ROString -> Attr("rostring", []) 
  | k -> E.s (E.unimp "k2attr:%a" d_opointerkind k)

(*
let pk2attr (pk: pkind) : attribute = 
  k2attr (pk2okind pk)
  *)

let kindOfAttrlist al = 
  let rec loop = function
      [] -> Unknown, Default
    | a :: al -> begin
        match a with
          Attr ("safe", []) -> Safe, UserSpec
        | Attr ("index", []) -> Index, UserSpec
        | Attr ("seq", []) -> Seq, UserSpec
        | Attr ("fseq", []) -> FSeq, UserSpec
        | Attr ("seqn", []) -> SeqN, UserSpec
        | Attr ("fseqn", []) -> FSeqN, UserSpec
        | Attr ("wild", []) -> Wild, UserSpec
        | Attr ("wildt", []) -> WildT, UserSpec
        | Attr ("seqt", []) -> SeqT, UserSpec
        | Attr ("fseqt", []) -> FSeqT, UserSpec
        | Attr ("seqnt", []) -> SeqNT, UserSpec
        | Attr ("fseqnt", []) -> FSeqNT, UserSpec
        | Attr ("sized", []) -> Index, UserSpec
        | Attr ("tagged", []) -> Wild, UserSpec
        | Attr ("string", []) -> String, UserSpec
        | Attr ("rostring", []) -> ROString, UserSpec
        | Attr ("nullterm", []) -> String, UserSpec
        | _ -> loop al
    end    
  in
  loop al


(*
let pkindOfAttrlist al = 
  let k, why = kindOfAttrlist al in
  okind2kind k, why
  *)

(* Replace the ptrnode attribute with the actual qualifier attribute *)
type whichAttr = 
    AtPtr  (* In a pointer type *)
  | AtArray  (* In an array type *)
  | AtOpenArray (* In an array type without a size *)
  | AtVar (* For a variable *)
  | AtOther (* Anything else *)


let replacePtrNodeAttrList where al = 
(*  ignore (E.log "replacePtrNode: %a\n"
            (d_attrlist true) al); *)
  let foundNode : string ref = ref "" in
  let rec loop = function
      [] -> []
    | a :: al -> begin
        match a with
          Attr("_ptrnode", [AInt n]) -> begin
            try 
              let nd = H.find idNode n in
              let found = 
                if nd.kind = Unknown then begin
                  ignore (E.warn "Found node %d with kind Unkown\n" n);
                  ""
                end else 
                  match k2attr nd.kind with
                    Attr(s, _)  -> s
              in
              foundNode := found;
              a :: loop al
            with Not_found -> begin
              ignore (E.warn "Cannot find node %d\n" n);
              a :: loop al
            end
          end
        | Attr("safe", []) -> foundNode := "safe"; loop al
        | Attr("index", []) -> foundNode := "index"; loop al
        | Attr("seq", []) -> foundNode := "seq"; loop al
        | Attr("fseq", []) -> foundNode := "fseq"; loop al
        | Attr("seqn", []) -> foundNode := "seqn"; loop al
        | Attr("fseqn", []) -> foundNode := "fseqn"; loop al
        | Attr("wild", []) -> foundNode := "wild"; loop al
        | Attr("indext", []) -> foundNode := "indext"; loop al
        | Attr("seqt", []) -> foundNode := "seqt"; loop al
        | Attr("fseqt", []) -> foundNode := "fseqt"; loop al
        | Attr("seqnt", []) -> foundNode := "seqnt"; loop al
        | Attr("fseqnt", []) -> foundNode := "fseqnt"; loop al
        | Attr("wildt", []) -> foundNode := "wildt"; loop al
        | Attr("sized", []) -> foundNode := "sized"; loop al
        | Attr("tagged", []) -> foundNode := "tagged"; loop al
        | Attr("string", []) -> foundNode := "string"; loop al
        | Attr("rostring", []) -> foundNode := "rostring"; loop al
        | _ -> a :: loop al
    end
  in 
  let al' = loop al in (* Get the filtered attributes *)
  let kres = 
    match where with
      AtPtr -> 
        if !foundNode <> "" then !foundNode 
        else if !defaultIsWild then "wild" else "safe" 
    | (AtArray | AtOpenArray) -> 
        if !foundNode = "index" then "sized" 
        else if !foundNode = "indext" then "sized" 
        else if !foundNode = "seqn" then "nullterm" 
        else if !foundNode = "fseqn" then "nullterm" 
        else if !foundNode = "seqnt" then "nullterm" 
        else if !foundNode = "fseqnt" then "nullterm" 
        else if !foundNode = "string" then "nullterm" 
        else if !foundNode = "rostring" then "nullterm" 
        else if !foundNode = "wild" then "wild" 
        else if !foundNode = "wildt" then "wild"
        else if where = AtOpenArray then 
          if !defaultIsWild then "wild" else "sized" 
        else !foundNode
    | AtVar ->
        if !foundNode = "wild" then "tagged" 
        else if !foundNode = "wildt" then "tagged"
        else !foundNode
    | AtOther -> !foundNode
  in
  if kres <> "" then 
    addAttribute (Attr(kres,[])) al' 
  else 
    al'

  
(* Make a new node *)
let newNode (p: place) (idx: int) (bt: typ) (al: attribute list) : node =
  let where = p, idx in
  incr nextId;
  let kind,why_kind = kindOfAttrlist al in
  let n = { id = !nextId;
            btype   = bt;
            attr    = addAttribute (Attr("_ptrnode", [AInt !nextId])) al;
            where   = where;
            flags   = 0 ;
            onStack = false;
            updated = false;
            arith   = false;
            posarith= false;
            null    = false;
            intcast = false;
            noPrototype = false;
            interface = false;
            locked = false;
            succ = [];
            kind = kind;
            why_kind = why_kind; 
            sized = false ;
            pointsto = [];
            mark = false;
            can_reach_string = false;
            can_reach_seq = false;
            can_reach_index = false;
            pred = []; } in
(*  ignore (E.log "Created new node(%d) at %a\n" n.id d_placeidx where); *)
  H.add placeId where n;
  H.add idNode n.id n;
  (* Now set the pointsto nodes *)
  let _ =
    let doOneType = function
        (* This will add points to to pointers embedded in structures or in 
         * functions (function return or arguments) *)
        TPtr (_, a) as t -> addPointsToType n t; ExistsFalse

      | _ -> ExistsMaybe
    in
    ignore (existsType doOneType n.btype);


    (* If a structure contains an array, a pointer to that structure also 
     * contains a pointer to the array. We need this information to
     * properly handle wild pointers. *)
    let lookForInternalArrays = function
        TArray(bt,len,al) as t -> addPointsToType n t; ExistsFalse
        | _ -> ExistsMaybe
    in 
    ignore (existsType lookForInternalArrays n.btype)
  in
  n
    
let dummyNode = newNode (PGlob "@dummy") 0 voidType []
  

(* Get a node for a place and an index. Give also the base type and the 
 * attributes *)
let getNode (p: place) (idx: int) (bt: typ) (al: attribute list) : node = 
  (* See if exists already *)
  let where = (p, idx) in
  try
    H.find placeId where
  with Not_found -> newNode p idx bt al


let nodeExists (p: place) (idx: int) = 
  H.mem placeId (p, idx)


let addEdge (start: node) (dest: node) (kind: edgekind) (callid: int) =
  if start == dummyNode || dest == dummyNode then
    ignore (E.warn "Adding edge between nodes %d and %d\n" start.id dest.id)
  else begin
    let nedge = 
      { efrom = start; eto= dest; ekind = kind; ecallid = callid; 
        eloc = !currentLoc} in
    start.succ <- nedge :: start.succ;
    dest.pred <- nedge :: dest.pred
  end


let removeSucc n sid = 
  n.succ <- List.filter (fun e -> e.eto.id <> sid) n.succ

let removePred n pid = 
  n.pred <- List.filter (fun e -> e.efrom.id <> pid) n.pred

let ptrAttrCustom printnode = function
      Attr("_ptrnode", [AInt n]) -> 
        if printnode then
          Some (dprintf "__NODE(%d)" n)
        else begin
          try
            let nd = H.find idNode n in
            if nd.kind = Unknown && nd.why_kind = Default then
              Some nil (* Do not print these nodes *)
            else
              Some (d_opointerkind () nd.kind)
          with Not_found -> Some nil (* Do not print these nodes *)
        end
    | Attr("ronly", []) -> Some (text "__RONLY")
    | Attr("safe", []) -> Some (text "__SAFE")
    | Attr("seq", []) -> Some (text "__SEQ")
    | Attr("fseq", []) -> Some (text "__FSEQ")
    | Attr("seqn", []) -> Some (text "__SEQN")
    | Attr("fseqn", []) -> Some (text "__FSEQN")
    | Attr("index", []) -> Some (text "__INDEX")
    | Attr("wild", []) -> Some (text "__WILD")
    | Attr("seqt", []) -> Some (text "__SEQT")
    | Attr("fseqt", []) -> Some (text "__FSEQT")
    | Attr("seqnt", []) -> Some (text "__SEQNT")
    | Attr("fseqnt", []) -> Some (text "__FSEQNT")
    | Attr("indext", []) -> Some (text "__INDEXT")
    | Attr("wildt", []) -> Some (text "__WILDT")
    | Attr("stack", []) -> Some (text "__STACK")
    | Attr("opt", []) -> Some (text "__OPT")
    | Attr("string", []) -> Some (text "__RWSTRING")
    | Attr("rostring", []) -> Some (text "__ROSTRING")
    | Attr("sized", []) -> Some (text "__SIZED")
    | Attr("tagged", []) -> Some (text "__TAGGED")
    | Attr("nullterm", []) -> Some (text "__NULLTERM")
    | Attr("safeunion", []) -> Some (text "__SAFEUNION")
    | a -> None



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


(* Type names, computed in such a way that compatible types have the same id, 
 * even if they are syntactically different. Right now we flatten structures 
 * but we do not pull common subcomponents out of unions and we do not unroll 
 * arrays. *)


(* Some structs (those involved in recursive types) are named. This hash maps 
 * their name to the ID *)
let namedStructs : (string, string) H.t = H.create 110


(* Keep track of the structs in which we are (to detect loops). When we 
 * detect a loop we remember that *)
let inStruct : (string, bool ref) H.t = H.create 110


let rec typeIdentifier (t: typ) : string = 
  let res = typeId t in
  H.clear inStruct;  (* Start afresh next time *)
  res

and typeId = function
    TInt(ik, a) -> ikId ik ^ attrsId a
  | TVoid a -> "V" ^ attrsId a
  | TBitfield (ik, w, a) ->  "B" ^ ikId ik ^ 
                             string_of_int w ^ attrsId a
  | TFloat (fk, a) -> fkId fk ^ attrsId a
  | TEnum _ -> ikId IInt (* !!! *)
  | TNamed (_, t, a) -> typeId (typeAddAttributes a t)
  | TComp (comp, a) when comp.cstruct -> begin
      let hasPrefix s p = 
        let pl = String.length p in
        (String.length s >= pl) && String.sub s 0 pl = p
      in
      (* See if we are in a loop *)
      try
        let inloop = H.find inStruct comp.cname in
        inloop := true; (* Part of a recursive type *)
        "t" ^ prependLength comp.cname (* ^ attrsId comp.cattr *)
      with Not_found -> 
        let inloop = ref false in
        let isanon = hasPrefix comp.cname "__anon" in
        if not isanon then H.add inStruct comp.cname inloop;
        let fieldsids = 
          List.fold_left (fun acc f -> acc ^ typeId f.ftype) "" comp.cfields in
        (* If it is in a loop then keep its name *)
        let res = fieldsids (* ^ attrsId comp.cattr *) in
        if not isanon then H.remove inStruct comp.cname;
        if !inloop && not (H.mem namedStructs comp.cname) then begin
          H.add namedStructs comp.cname res;
          "t" ^ prependLength comp.cname (* ^ attrsId comp.cattr *)
        end else
          res
  end
  | TComp (comp, a) when not comp.cstruct -> 
      "N" ^ (string_of_int (List.length comp.cfields)) ^
      (List.fold_left (fun acc f -> acc ^ typeId f.ftype ^ "n") 
         "" comp.cfields) ^
      attrsId (addAttributes comp.cattr  a)
  | TPtr (t, a) -> "P" ^ typeId t ^ "p" ^ attrsId a
  | TArray (t, lo, a) -> 
      let thelen = "len" in
      "A" ^ typeId t ^ "a" ^ prependLength thelen ^ attrsId a
  | TFun (tres, args, va, a) -> 
      "F" ^ typeId tres ^ "f" ^ (string_of_int (List.length args)) ^ 
      (List.fold_left (fun acc arg -> acc ^ typeId arg.vtype ^ "f") 
         "" args) ^ (if va then "V" else "v") ^ attrsId a
  | _ -> E.s (E.bug "typeId")
      
and ikId = function
    IChar -> "C"
  | ISChar -> "c"
  | IUChar -> "b"
  | IInt -> "I"
  | IUInt -> "U"
  | IShort -> "S"
  | IUShort -> "s"
  | ILong -> "L"
  | IULong -> "l"
  | ILongLong -> "W"
  | IULongLong -> "w"

and fkId = function
    FFloat -> "O"
  | FDouble -> "D"
  | FLongDouble -> "T"

and prependLength s = 
  let l = String.length s in
  if s = "" || (s >= "0" && s <= "9") then
    E.s (E.unimp "String %s starts with a digit\n" s);
  string_of_int l ^ s

and attrsId al = 
  match al with
    [] -> "_"
  | _ -> "r" ^ List.fold_left (fun acc (Attr(an,_)) -> acc ^ an) "" al ^ "r"



(************ Print statistics about the graph ******************)
let addToHisto (histo: ('a, int ref) H.t) (much: int) (which: 'a) : unit = 
  try let r = H.find histo which in r := !r + much 
  with Not_found -> let r = ref much in H.add histo which r
let getHisto (histo: ('a, int ref) H.t) (which: 'a) : int = 
  try let r = H.find histo which in !r with Not_found -> 0
let sortHisto (histo: ('a, int ref) H.t) : ('a * int) list = 
  let theList : ('a * int) list ref = ref [] in
  H.iter (fun k r -> theList := (k, !r) :: !theList) histo;
  List.sort (fun (_,v1) (_,v2) -> - (compare v1 v2)) !theList 

let showFirst (showone: 'a -> int -> unit)
              (many: int) (lst: ('a * int) list) =    
  let rec loop i = function
      (n, s) :: rest when i >= 0 && s > 0 -> 
        showone n s;
        loop (i - 1) rest
    | _ -> ()
  in
  loop many lst
    



let printGraphStats () =     
  (* Keep a histograph per kind *)
  let totKind : (opointerkind, int ref) H.t = H.create 17 in 
  let totalNodes = ref 0 in
  (* The number of bad casts *)
  let badCasts = ref 0 in
  let badCastsVoid = ref 0 in
  (* Keep track of spread_from_edge. For each node how many other nodes have 
   * WILD/spread_from_edge(n).  *)
  let spreadTo : (int, int ref) H.t = H.create 117 in
  let examine_node id n =
    incr totalNodes;
    addToHisto totKind 1 n.kind;
    if n.kind = Wild then begin
      (match n.why_kind with
        SpreadFromEdge(fromn) -> addToHisto spreadTo 1 fromn.id
      | BadCast e -> 
          incr badCasts;
          (match e.efrom.btype, e.eto.btype with
            TVoid _, _ -> incr badCastsVoid
          | _, TVoid _ -> incr badCastsVoid
          | _ -> ())

      | _ -> ())
    end
  in
  H.iter examine_node idNode;
  ignore (E.log "Graph contains %d nodes\n" !totalNodes);
  H.iter
    (fun k r -> ignore (E.log "  %a - %d (%3.0f%%)\n"
                          d_opointerkind k !r
                          (float_of_int(!r) 
                             /. float_of_int(!totalNodes) *. 100.0)))
    totKind;
  ignore (E.log "%d bad casts of which %d involved void*\n"
            !badCasts !badCastsVoid);
  (* Now print the WILD bottlenecks. Places that have many successors in the 
   * spreadToEdge *)
  let spreadsToImmediate = sortHisto spreadTo in
  ignore (E.log "Node xxx spreads to yyy immediate successors\n");
  showFirst 
    (fun nid many -> ignore (E.log " %d -> %d@!" nid many))
    10
    spreadsToImmediate;
  
  (* Now compute for each WILD node at which node its WILD originates *)
  let castReaches : (location, int ref) H.t = H.create 117 in
  H.iter 
    (fun id n -> 
      if n.kind = Wild then begin
        let rec searchOrigin n = 
          match n.why_kind with
            SpreadFromEdge (fromn) -> searchOrigin fromn
          | SpreadPointsTo fromn -> searchOrigin fromn
          | BadCast e -> e.eloc
          | _ -> locUnknown
        in
        let origin = searchOrigin n in
        if origin != locUnknown then 
          addToHisto castReaches 1 origin
      end)
    idNode;
  let castReaches = sortHisto castReaches in
  ignore (E.log "Cast from node xxx reaches to yyy nodes:\n");
  showFirst 
    (fun nloc many -> ignore (E.log " %a -> %d@!" d_loc nloc many))
    20
    castReaches;
  
  H.clear totKind;
  H.clear spreadTo;
  ()

