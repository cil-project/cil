(*
 * Type-check a fully annotated CCured program.
 *)

open Ptrnode
open Cil
module E = Errormsg

let loc = ref locUnknown (* global location: where are we typechecking? *)

(* Return the target type and the kind of a pointer type. *)
let kind_of_ptr_type t =
  match unrollType t with
  | TPtr(targ,al) -> 
      let kind, why = kindOfAttrlist al in 
      targ, kind
  | TArray(targ,_,al) -> 
      let kind, why = kindOfAttrlist al in 
      targ, kind
  | TFun(_,_,_,al) -> 
      let kind, why = kindOfAttrlist al in 
      t, kind
  | _ -> failwith "not a pointer"

(* Check to make sure that the given type (and everything it can point to)
 * is WILD. *)
let typechecked_wild = Hashtbl.create 511 
let rec typecheck_wild_type t =
  if Hashtbl.mem typechecked_wild t then
    ()
  else begin
    Hashtbl.add typechecked_wild t true ;
    match unrollType t with
      TVoid(al) -> ()
    | TInt(i,al) -> ()
    | TFloat(f,al) -> ()
    | TEnum(e,al) -> ()
    | TPtr(t2,al) -> let kind,why = kindOfAttrlist al in
                     (if kind <> Wild then ignore
                      (E.warn "typecheck: %a should be Wild" d_type t)) ;
                     typecheck_wild_type t2 
    | TComp(ci,al) -> List.iter (fun fi -> typecheck_wild_type fi.ftype)
                          ci.cfields
    | TFun(t,vl,vararg,al) -> 
                     typecheck_wild_type t ;
                     List.iter (fun vi -> typecheck_wild_type vi.vtype) vl
    | TArray(t2,_,al) -> let kind,why = kindOfAttrlist al in
                     (if kind <> Wild then ignore
                      (E.warn "typecheck: %a should be Wild" d_type t)) ;
                     typecheck_wild_type t2 
    | TNamed(_) -> failwith "unrollType"
  end

(* Consistency checking on a type.
 * Right now this just checks that WILD pointers point to WILD things. 
 * other_list is a list of types that should be WILD if the given type
 * is WILD. *)
let typecheck_type tau other_list = 
  match unrollType tau with
    TPtr(t,al) -> let kind,why = kindOfAttrlist al in 
                  if kind = Wild then begin
                    typecheck_wild_type t ;
                    List.iter (fun tau -> typecheck_wild_type tau) other_list
                  end
  | _ -> ()

let typecheck_pointer_arithmetic a b e =
  (* a = pointer, b = integer, e = whole expression *)
  try
    let ptr_target, ptr_kind = kind_of_ptr_type (typeOf a) in
    let okay = 
    match ptr_kind with
    | Safe -> false
    | Seq | SeqN -> true
    | FSeq | FSeqN | String | ROString -> (* must be positive *)
      begin
      match isInteger (constFold true b) with 
        Some(i) when i >= Int64.zero -> ()
      | Some _ | None -> ignore
          (E.warn "typecheck: %a@?not positive arith on %a pointer in %a" 
            d_loc !loc d_opointerkind ptr_kind d_exp e)
      end ; true 
    | Wild -> true
    | _ -> false
    in if not okay then ignore
      (E.warn "typecheck: %a@?bad arithmetic on %a %a pointer %a in@!%a" 
        d_loc !loc d_opointerkind ptr_kind d_type (typeOf a) d_exp a d_exp e)
  with _ -> ignore
      (E.warn "typecheck: %a@?no qualifier for %a in %a" 
        d_loc !loc d_exp a d_exp e)

(* Consistency checking on a variable declaration.
 * Right now this just checks its type. *)
let typecheck_varinfo vi other_list = typecheck_type vi.vtype other_list

(* Consistency checking for code. *)
class typecheck = object
  inherit nopCilVisitor 
  method vexpr e = begin
    match e with
      CastE(to_type, from_exp) -> begin
        let from_type = typeOf(from_exp) in 
        try begin
          let to_target, to_kind = kind_of_ptr_type to_type in
          try begin
            let from_target, from_kind = kind_of_ptr_type from_type in
            (* check a cast from one pointer to another *)
            let okay = 
            match from_kind, to_kind with
              Safe, Safe -> Type.subtype to_target from_target 
            | Safe, Seq 
            | Safe, FSeq ->
                let n = (bitsSizeOf(to_target) / bitsSizeOf(from_target)) + 1 in
                Type.subtype from_target 
                    (TArray(to_target, Some(integer n), [])) 
            | Seq, Safe 
            | FSeq, Safe -> 
                let n = (bitsSizeOf(from_target) / bitsSizeOf(to_target)) + 1 in
                Type.subtype to_target 
                    (TArray(from_target, Some(integer n), [])) 
            | Seq, Seq 
            | FSeq, FSeq 
            | Seq, FSeq  
            | FSeq, Seq -> 
                let from_size = bitsSizeOf(from_target) / 8 in
                let to_size = bitsSizeOf(to_target) / 8 in
                Type.equal 
                  (TArray(from_target, Some(integer to_size), []))
                  (TArray(to_target, Some(integer from_size), []))
            | Wild, Wild -> true (* always OK *)
            | _, _ -> ignore
              (E.warn "typecheck: %a@?unfamiliar %a -> %a cast@?%a -> %a@!%a" 
                d_loc !loc
                d_opointerkind from_kind d_opointerkind to_kind 
                d_type from_type d_type to_type d_exp e) ; true
            in if not okay then ignore
              (E.warn "typecheck: %a@?bad %a -> %a cast@?%a -> %a@!%a" 
                d_loc !loc
                d_opointerkind from_kind d_opointerkind to_kind 
                d_type from_type d_type to_type d_exp e)
          end with _ -> begin
            (* to is a pointer, from is not *)
            match to_kind with
              Safe | String | ROString ->  (* from must be 0 *)
                if isZero from_exp then
                  ()
                else ignore
                  (E.warn 
                    "typecheck: %a@?non-zero value %a cast into %a type %a"
                    d_loc !loc
                    d_exp from_exp d_opointerkind to_kind 
                    d_type to_type)
            | _ -> () (* any int is fine *)
          end
        end with _ -> ()
        end
    | BinOp(PlusPI,a,b,_) 
    | BinOp(MinusPI,a,b,_) -> (* pointer is always on the left *)
        typecheck_pointer_arithmetic a b e 
    | _ -> () 
    end ; 
    DoChildren
  method vlval v = begin
    match v with
      (base,(Index(e,o))) -> 
        typecheck_pointer_arithmetic (Lval(base,NoOffset)) e (Lval(v))
    | _ -> ()
    end ; DoChildren
  method vinst i = 
    (loc := match i with Set(_,_,l) | Call(_,_,_,l) | Asm(_,_,_,_,_,l) -> l);
    DoChildren
  method vstmt s = 
    (loc := match s.skind with
      Return(_,l) | Goto(_,l) | Break(l) | Continue(l) | 
      If(_,_,_,l) | Switch(_,_,_,l) | Loop(_,l) -> l
      | _ -> !loc) ;
    DoChildren
  method vblock b = 
    if hasAttribute "nobox" b.battrs then
      SkipChildren
    else
      DoChildren
end

let typechecker = new typecheck 
  
(* Consistency checking for functions. *)
let typecheck_fun f = 
  typecheck_varinfo f.svar 
    (List.map (fun vi -> vi.vtype) f.sformals) ;
  List.iter (fun vi -> typecheck_varinfo vi []) f.sformals ;
  List.iter (fun vi -> typecheck_varinfo vi []) f.slocals ;
  ignore (visitCilBlock typechecker f.sbody)

let checking = ref true 

let typecheck_global g = match g with
    GFun(fundec,_) -> (if !checking then typecheck_fun fundec )
  | GType(str,tau,_) -> (if !checking then typecheck_type tau [] )
  | GEnumTag(_) -> ()
  | GCompTag(ci,_) -> ()
  | GVar(vi,io,_) -> ()
  | GAsm(_) -> ()
  | GPragma(a, _) -> begin
      match a with
        | Attr("box", [AId("on")]) -> checking := true
        | Attr("box", [AId("off")]) -> checking := false
        | _ -> () 
    end
  | GText(_) -> ()
  | GDecl(_) -> () 

let typecheck_file f =
  checking := true ; 
  List.iter (fun glob -> typecheck_global glob) f.globals
