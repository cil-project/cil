open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let i = ref 0
let name = ref ""

(* sm: switches! *)
let linux = ref false          (* when true, use printk *)
let allInsts = ref false       (* when true, instrument every instruction *)
let printPtrs = ref false      (* when true, print pointer values *)
let printStrings = ref false   (* when true, print char* as strings *)
let noCFuncs = ref false       (* when true, don't print calls to *)
                               (* functions whose name begins with "C" *)
                               (* (only relevant when allInsts = true) *)

let styleHelp:string = "sets logcalls style, as sum of:\n" ^
  "      1=linux, 2=allInsts, 4=printPtrs, 8=printStrings, 16=noCFuncs"

(* sm: decided to pass cryptic integer instead of making lots of separate *)
(* command-line arguments *)
let setStyle (i:int) =
  linux        := (i land 1) != 0;
  allInsts     := (i land 2) != 0;
  printPtrs    := (i land 4) != 0;
  printStrings := (i land 8) != 0;
  noCFuncs     := (i land 16) != 0;

(* instrument every instruction! aie! *)
class verboseLogVisitor printfFun funstr prefix = object
  inherit nopCilVisitor
	(* overridden
  method vinst (inst  : instr) = begin
    let str = Printf.sprintf "<5> %s::%d\n" !name !i in
    incr i ;
    let newinst = ((Call (None, Lval(var printfFun.svar),
                                ( [ (* one ; *) Const(CStr(str)) ]),
                                locUnknown)) : instr )in
    let ilist = ([ inst ; newinst ] : instr list) in
    (ChangeTo(ilist))
  end
	*)
  method vinst i = begin
    match i with
    | Call(lo,Lval(Var(vi),NoOffset),al,l)
        when (!noCFuncs && vi.vname.[0] = 'C') -> SkipChildren
    | Call(lo,e,al,l) ->
      let str1 = prefix ^
        (Pretty.sprint 800 ( Pretty.dprintf "Calling %a(%a)\n"
          d_exp e
          (docList (chr ',' ++ break ) (fun arg ->
            try
              match unrollType (typeOf arg) with
                  TInt _ | TEnum _ -> dprintf "%a = %%d" d_exp arg
                | TFloat _ -> dprintf "%a = %%g" d_exp arg
                | TVoid _ -> text "void"
                | TComp _ -> text "comp"
                | _ -> dprintf "%a = %%p" d_exp arg
            with  _ -> dprintf "%a = %%p" d_exp arg)) al)) in
      let log_args = List.filter (fun arg ->
        match unrollType (typeOf arg) with
          TVoid _ | TComp _ -> false
        | _ -> true) al in
      let str2 = prefix ^ (Pretty.sprint 800
        ( Pretty.dprintf "Returned from %a\n" d_exp e)) in
      let newinst str args = ((Call (None, Lval(var printfFun.svar),
                                ( [ (* one ; *) Const(CStr(str)) ] @ args),
                                locUnknown)) : instr )in
      let ilist = ([ (newinst str1 log_args) ; i ; (newinst str2 []) ] : instr list) in
    (ChangeTo(ilist))
    | _ -> DoChildren
  end
  method vstmt (s : stmt) = begin
    match s.skind with
      Return(Some(e),l) ->
      let str = prefix ^ Pretty.sprint 800 ( Pretty.dprintf
        "Return(%%p) from %s\n" funstr ) in
      let newinst = ((Call (None, Lval(var printfFun.svar),
                                ( [ (* one ; *) Const(CStr(str)) ; e ]),
                                locUnknown)) : instr )in
      let new_stmt = mkStmtOneInstr newinst in
      let slist = [ new_stmt ; s ] in
      (ChangeTo(mkStmt(Block(mkBlock slist))))
    | Return(None,l) ->
      let str = prefix ^ (Pretty.sprint 800 ( Pretty.dprintf
        "Return void from %s\n" funstr)) in
      let newinst = ((Call (None, Lval(var printfFun.svar),
                                ( [ (* one ; *) Const(CStr(str)) ]),
                                locUnknown)) : instr )in
      let new_stmt = mkStmtOneInstr newinst in
      let slist = [ new_stmt ; s ] in
      (ChangeTo(mkStmt(Block(mkBlock slist))))
    | _ -> DoChildren
  end
end


let logCalls (f: file) : unit =
  let prefix = if !linux then "<5>" else "" in

  (* Create a prototype for the logging function *)
  let printfFun =
    let fdec = emptyFunction (if !linux then "printk" else "printf") in
    (* let argi  = makeLocalVar fdec "prio" intType in *)
    let argf  = makeLocalVar fdec "format" charConstPtrType in
    fdec.svar.vtype <- TFun(intType, Some [ argf ], true, []);
    fdec
  in

  let isCharType (k:ikind) =
    k=IChar || k=ISChar || k=IUChar in

  (* debugging anagram, it's really nice to be able to see the strings *)
  (* inside fat pointers, even if it's a bit of a hassle and a hack here *)
  let isFatCharPtr (cinfo:compinfo) =
    cinfo.cname="wildp_char" ||
    cinfo.cname="fseqp_char" ||
    cinfo.cname="seqp_char" in

  let doGlobal = function
      GFun (fdec, loc) ->
        (* Collect expressions that denote the actual arguments *)
        let actargs =
          (* make lvals out of args which pass test below *)
          (List.map
            (fun vi -> match unrollType vi.vtype with
              | TComp(cinfo, _) when isFatCharPtr(cinfo) ->
                  (* access the _p field for these *)
                  (* luckily it's called "_p" in all three fat pointer variants *)
                  Lval(Var(vi), Field(getCompField cinfo "_p", NoOffset))
              | _ ->
                  Lval(var vi))

            (* decide which args to pass *)
            (List.filter
              (fun vi -> match unrollType vi.vtype with
                | TPtr(TInt(k, _), _) when isCharType(k) ->
                    !printPtrs || !printStrings
                | TComp(cinfo, _) when isFatCharPtr(cinfo) ->
                    !printStrings
                | TVoid _ | TComp _ -> false
                | TPtr _ | TArray _ | TFun _ -> !printPtrs
                | _ -> true)
              fdec.sformals)
          ) in

        (* make a format string for printing them *)
        (* sm: expanded width to 200 because I want one per line *)
        let formatstr = prefix ^ (Pretty.sprint 200
          (dprintf "entering %s(%a)\n" fdec.svar.vname
            (docList (chr ',' ++ break)
              (fun vi -> match unrollType vi.vtype with
              | TInt _ | TEnum _ -> dprintf "%s = %%d" vi.vname
              | TFloat _ -> dprintf "%s = %%g" vi.vname
              | TVoid _ -> dprintf "%s = (void)" vi.vname
              | TComp(cinfo, _) -> (
                  if !printStrings && isFatCharPtr(cinfo) then
                    dprintf "%s = \"%%s\"" vi.vname
                  else
                    dprintf "%s = (comp)" vi.vname
                )
              | TPtr(TInt(k, _), _) when isCharType(k) -> (
                  if (!printStrings) then
                    dprintf "%s = \"%%s\"" vi.vname
                  else if (!printPtrs) then
                    dprintf "%s = %%p" vi.vname
                  else
                    dprintf "%s = (str)" vi.vname
                )
              | TPtr _ | TArray _ | TFun _ -> (
                  if (!printPtrs) then
                    dprintf "%s = %%p" vi.vname
                  else
                    dprintf "%s = (ptr)" vi.vname
                )
              | _ -> dprintf "%s = (?type?)" vi.vname))
            fdec.sformals)) in

        i := 0 ;
        name := fdec.svar.vname ;
        if !allInsts then (
          let thisVisitor = new verboseLogVisitor printfFun !name prefix in
          fdec.sbody <- visitCilBlock thisVisitor fdec.sbody
        );
        fdec.sbody.bstmts <-
              mkStmt (Instr [Call (None, Lval(var printfFun.svar),
                                ( (* one :: *) Const(CStr(formatstr)) :: actargs),
                                loc)]) :: fdec.sbody.bstmts

    | _ -> ()
  in
  Stats.time "logCalls" (iterGlobals f) doGlobal
