(*
 * Type-check a fully annotated CCured program.
 *)

open Ptrnode
open Cil

(* For a probably-pointer P, we want to determine:
 *  (a) the Cil.type associated with *P
 *  (b) the pointer kind associated with P
 *  (c) the pointer kind associated with &P
 * Can raise Failure. 
 *)
let rec kind_of_ptr_type t =
  let targ, al = match unrollType t with
  | TPtr(targ,al) -> targ, al
  | TArray(targ,_,al) -> targ, al
  | TFun(_,_,_,al) -> t, al 
  | _ -> failwith "not a pointer"
  in let kind, why = kindOfAttrlist al in 
  (targ, kind, Unknown)
and kind_of_ptr_exp e =
  match e with
  | UnOp(op,e1,tau) -> kind_of_ptr_type tau
  | BinOp(op,e1,e2,tau) -> kind_of_ptr_type tau
  | Question(e1,e2,e3) -> kind_of_ptr_exp e2 
  | CastE(tau,e) -> kind_of_ptr_type tau 
  | Lval(lv) -> kind_of_ptr_lval lv 
  | AddrOf(lv) -> 
      let a,b,c = kind_of_ptr_lval lv in
      (typeOfLval lv),c,Unknown
  | StartOf(lv) -> kind_of_ptr_lval lv
  | _ -> kind_of_ptr_type (typeOf e)
and kind_of_ptr_lval l = match l with
  | Var(vi),o -> 
    let a,b,c = kind_of_ptr_type vi.vtype in 
    let kind, why = kindOfAttrlist vi.vattr in  (* & information *)
    kind_of_ptr_offset o (a,b,kind)
  | Mem(e),o -> 
    let et = typeOf e in 
    let lt = typeOfLval l in
    let la,lb,lc = kind_of_ptr_type lt in
    let ea,eb,ec = kind_of_ptr_type et in
    kind_of_ptr_offset o (la,lb,eb)
and kind_of_ptr_offset o (a,b,c) =
  match o with
  | NoOffset -> (a,b,c)
  | Field(fi,o') -> 
      let a,b,c = kind_of_ptr_type fi.ftype in
      let kind, why = kindOfAttrlist fi.fattr in
      kind_of_ptr_offset o' (a,b,kind)
  | Index(e,o') -> 
      let a',b',c' = kind_of_ptr_type a in
      (a',b',b)

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
                      (warn "typecheck:@?%a should be Wild" d_type t)) ;
                     typecheck_wild_type t2 
    | TComp(ci,al) -> List.iter (fun fi -> typecheck_wild_type fi.ftype)
                          ci.cfields
    | TFun(t,vl,vararg,al) -> 
                     typecheck_wild_type t ;
                     List.iter (fun vi -> typecheck_wild_type vi.vtype) 
                               (argsToList vl)
    | TArray(t2,_,al) -> let kind,why = kindOfAttrlist al in
                     (if kind <> Wild then ignore
                      (warn "typecheck:@?%a should be Wild" d_type t)) ;
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
    let ptr_target, ptr_kind, ptr_addr_kind = 
      kind_of_ptr_exp a in
    let okay = 
    match ptr_kind with
    | Safe -> false
    | Seq | SeqN -> true
    | FSeq | FSeqN | String | ROString -> (* must be positive *)
      begin
      match isInteger (constFold true b) with 
        Some(i) when i >= Int64.zero -> ()
      | Some _ | None -> ignore
          (warn "typecheck:@?not positive arith on %a pointer in %a" 
            d_opointerkind ptr_kind d_exp e)
      end ; true 
    | Wild -> true
    | _ -> false
    in if not okay then ignore
      (warn "typecheck:@?bad arithmetic on %a %a pointer %a in@!%a" 
        d_opointerkind ptr_kind d_type (typeOf a) d_exp a d_exp e)
  with _ -> ignore
      (warn "typecheck:@?no qualifier for %a in %a" 
        d_exp a d_exp e)

(* Consistency checking on a variable declaration.
 * Right now this just checks its type. *)
let typecheck_varinfo vi other_list = typecheck_type vi.vtype other_list

let checking = ref true 

(* Make sure casts/assignments work correctly. 
 * 
 *)
let check_cast (context : Pretty.doc) to_type from_exp = begin
  let from_type = typeOf(from_exp) in 
  try begin
    let to_target, to_kind, to_addr_kind = 
      kind_of_ptr_type to_type in
    try begin
      let from_target, from_kind, from_addr_kind = 
        kind_of_ptr_exp from_exp in
      (* check a cast from one pointer to another *)
      try begin
      let okay = 
      match from_kind, to_kind with
        Safe, Safe -> Type.subtype to_target from_target 
      | Safe, Seq 
      | Safe, FSeq ->
          let n = (Type.bytesSizeOf(from_target) / Type.bytesSizeOf(to_target)) + 1 in
          Type.subtype from_target 
              (TArray(to_target, Some(integer n), [])) 
      | Seq, Safe 
      | FSeq, Safe -> 
          let n = (Type.bytesSizeOf(to_target) / Type.bytesSizeOf(from_target)) + 1 in
          Type.subtype to_target 
              (TArray(from_target, Some(integer n), [])) 
      | Seq, Seq 
      | FSeq, FSeq 
      | Seq, FSeq  
      | FSeq, Seq -> 
          let from_size = Type.bytesSizeOf(from_target) in
          let to_size = Type.bytesSizeOf(to_target) in
          Type.equal 
            (TArray(from_target, Some(integer to_size), []))
            (TArray(to_target, Some(integer from_size), []))
      | Wild, Wild -> true (* always OK *)

      | _, ROString -> true (* always OK *)

      | _, _ -> ignore
        (warn "typecheck:@?unfamiliar %a -> %a cast@?%a -> %a@!%a" 
          d_opointerkind from_kind d_opointerkind to_kind 
          d_type from_type d_type to_type Pretty.insert context) ; true
      in if not okay then ignore
        (warn "typecheck:@?bad %a -> %a cast@?%a -> %a@!%a" 
          d_opointerkind from_kind d_opointerkind to_kind 
          d_type from_type d_type to_type Pretty.insert context)
      end with problem -> ignore
        (warn "typecheck:@?cannot check %a -> %a cast@?%a -> %a@!%a@!%s" 
          d_opointerkind from_kind d_opointerkind to_kind 
          d_type from_type d_type to_type Pretty.insert context
          (Printexc.to_string problem))
    end with _ -> begin
      (* 'to' is a pointer, 'from' is not *)
      match to_kind with
        Safe | String | ROString ->  (* from must be 0 *)
          if isZero from_exp then
            ()
          else ignore
            (warn 
            "typecheck:@?non-zero value %a of type %a cast into %a type %a@!%a"
              d_exp from_exp d_type from_type d_opointerkind to_kind 
              d_type to_type Pretty.insert context)
      | _ -> () (* any int is fine *)
    end
  end with _ -> ()
end

let current_function_type = ref None

(* Consistency checking for code. *)
class typecheck = object
  inherit nopCilVisitor 
  method vexpr e = begin
    match e with
      CastE(to_type, from_exp) -> check_cast (d_exp () e) to_type from_exp
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
    match i with 
    | Set(lv,exp,_) -> 
      if (isPointerType (typeOfLval lv)) then begin
        check_cast (d_instr () i) (typeOfLval lv) exp
      end ; DoChildren
    | Call(lvopt,f,args,_) -> begin
      match typeOf f with
        TFun(ret_type, formal_vi_list, vararg, al) -> begin
        (* check the actual arguments *)
        (try 
          List.iter2 (fun formal_vi actual_exp ->
            check_cast (d_instr () i) (formal_vi.vtype) actual_exp)
            (argsToList formal_vi_list) args
        with _ -> ()) ; 
        (* finally, check the lvalue against the return type *)
        match lvopt with
          Some(lv) -> 
            check_cast (d_instr () i) (typeOfLval lv) (CastE(ret_type, f))
        | None -> ()
        end
      | tau -> ignore (warn "typecheck:@?call to non-function type %a@!%a"
              d_type tau d_instr i)
      end; DoChildren
    | Asm _ -> DoChildren
  method vstmt s = 
    match s.skind with
    | Return(Some(e),l) -> begin
      match !current_function_type with
        Some(TFun(ret_type,_,_,_)) ->
          check_cast (d_stmt () s) ret_type e
      | Some(tau) -> ignore (warn "typecheck:@?return inside non-function@!%a"
          d_stmt s)
      | None -> ignore (warn "typecheck:@?return not inside a function@!%a"
          d_stmt s)
      end;
      DoChildren
    | _ -> DoChildren
  method vblock b = 
    if hasAttribute "nobox" b.battrs then
      SkipChildren
    else
      DoChildren
  method vfunc f = 
    typecheck_varinfo f.svar (List.map (fun vi -> vi.vtype) f.sformals) ;
    List.iter (fun vi -> typecheck_varinfo vi []) f.sformals ;
    List.iter (fun vi -> typecheck_varinfo vi []) f.slocals ;
    DoChildren
  method vglob g = match g with
      GFun(fundec,_) -> 
        current_function_type := Some(fundec.svar.vtype) ; 
        (if !checking then DoChildren else SkipChildren)
    | GType(str,tau,_) -> (if !checking then typecheck_type tau [] ) ;
                          SkipChildren
    | GEnumTag(_) -> DoChildren
    | GCompTag(ci,_) -> DoChildren
    | GVar(vi,None,_) -> DoChildren
    | GVar(vi,Some(init),_) -> DoChildren
    | GAsm(_) -> DoChildren
    | GPragma(a, _) -> begin
        match a with
          | Attr("box", [AId("on")]) -> checking := true
          | Attr("box", [AId("off")]) -> checking := false
          | _ -> () 
      end ; DoChildren
    | GText(_) -> DoChildren
    | GDecl(_) -> DoChildren
end

let typechecker = new typecheck 
  
(* The main entry point to our typechecker *)
let typecheck_file f =
  checking := true ; ignore (visitCilFile typechecker f) 
