
open Escape
open Pretty
open Trace
open Cil

module E = Errormsg
module H = Hashtbl
module IH = Inthash
module M = Machdep
module U = Util
module RD = Reachingdefs
module UD = Usedef

let doElimTemps = ref false
let debug = ref false

(* A hash from variable ids to Call instruction
   options. If a variable id is in this table,
   and it is mapped to Some(Call()), then the
   function call can be printed instead of the
   variable *)
let iioh = IH.create 16

(* A hash from variable ids to information that
   can be used to print a post increment/decrement
   that can replace the variable *)
let incdecHash = IH.create 16

(* A hash from variable ids to a list of statement ids.
   Because a post-inc/dec will be printed elsewhere,
   the assignments of the variable in these statements
   don't need to be printed *)
let idDefHash = IH.create 16

(* Add a pair to the list for vid and create a list if one
   doesn't exist *)
let id_dh_add vid p =
  if IH.mem idDefHash vid then
    let oldlist = IH.find idDefHash vid in
    let newlist = p::oldlist in
    IH.replace idDefHash vid newlist
  else
    IH.add idDefHash vid [p]

class tempElimClass (fd:fundec) : cilVisitor = object (self)
  inherit nopCilVisitor
      
  (* which statement are we working on? *)
  val mutable sid = -1

  (* if a list of instructions is being
     processed, then this is the corresponding
     list of reaching defs *)
  val mutable ivihl = []

  (* these are the reaching defs for the
     current instruction if there is one *)
  val mutable inst_iviho = None

  method vstmt stm =
    sid <- stm.sid;
    match RD.getRDs sid with
      None -> DoChildren
    | Some(ivih, s, iosh) ->
	match stm.skind with
	  Instr il ->
	    ivihl <- RD.instrRDs il (ivih, s, iosh) false;
	    DoChildren
	| _ -> (ivihl <- [];
		inst_iviho <- None;
		DoChildren)

  method vinst i =
    inst_iviho <- Some(List.hd ivihl);
    ivihl <- List.tl ivihl;
    DoChildren

  method vexpr e =

    let do_change (ivih, iosh) vi =
      let ido = RD.iosh_singleton_lookup iosh vi in
      (match ido with
	Some id ->
	  let riviho = RD.getDefRhs id in
	  (match riviho with
	    Some(RD.RDExp(e) as r, defivih, defiosh) ->
	      if !debug then ignore(E.log "Can I replace %s with %a?\n" vi.vname d_exp e);
	      if RD.ok_to_replace (ivih,iosh) (defivih,defiosh) fd r
	      then 
		(if !debug then ignore(E.log "Yes.\n");
		 ChangeTo(e))
	      else
		(match RD.ok_to_replace_with_incdec (ivih,iosh) (defivih,defiosh) fd id vi r with
		  Some(curdef_stmt_id,redefid, rhsvi, b) ->
		    (if !debug then ignore(E.log "No, but I can replace it with a post-inc/dec\n");
		     if !debug then ignore(E.log "cdsi: %d redefid: %d name: %s\n"
					     curdef_stmt_id redefid rhsvi.vname);
		     IH.add incdecHash vi.vid (redefid, rhsvi, b);
		     id_dh_add rhsvi.vid (curdef_stmt_id, redefid);
		     DoChildren)
		| None ->
		    (if !debug then ignore(E.log "No.\n");
		     DoChildren))
	  | Some(RD.RDCall(i) as r, defivih, defiosh) ->
	      if !debug then ignore(E.log "Can I replace %s with %a?\n" vi.vname d_instr i);
	      if RD.ok_to_replace (ivih,iosh) (defivih,defiosh) fd r
	      then (if !debug then ignore(E.log "Yes.\n");
		    IH.add iioh vi.vid (Some(i));
		    DoChildren)
	      else (if !debug then ignore(E.log "No.\n");
		    DoChildren)
	  | _ -> DoChildren)
      | _ -> DoChildren)
    in

    match e with
      Lval (Var vi,off) ->
	(if String.length vi.vname >= 3 &&
	  compare (String.sub vi.vname 0 3) "tmp" == 0 then
	  (* only allowed to replace a tmp with a function call once *)
	  if IH.mem iioh vi.vid
	  then (IH.replace iioh vi.vid None; DoChildren)
	  else
	    (match inst_iviho with
	      Some(ivih,s,iosh) -> do_change (ivih, iosh) vi
	    | None -> let iviho = RD.getRDs sid in
	      match iviho with
		Some(ivih,s,iosh) -> 
		  (if !debug then ignore (E.log "Try to change %s outside of instruction.\n" vi.vname);
		   do_change (ivih, iosh) vi)
	      | None -> 
		  (if !debug then ignore (E.log "%s in statement w/o RD info\n" vi.vname);
		   DoChildren))
	else DoChildren)
    | _ -> DoChildren

end

(* Remove temp variables that are set but not used *)
class unusedRemoverClass : cilVisitor = object(self)
    inherit nopCilVisitor

  val mutable unused_set = UD.VS.empty

  (* figure out which locals aren't used *)
  method vfunc f =	

    (* the set of used variables *)
    let used = List.fold_left (fun u s ->
      let u', _ = UD.computeDeepUseDefStmtKind s.skind in
      UD.VS.union u u') UD.VS.empty f.sbody.bstmts in

    (* the set of unused locals *)
    let unused = List.fold_left (fun un vi ->
      if UD.VS.mem vi used
      then un
      else (if !debug then ignore (E.log "unusedRemoverClass: %s is unused\n" vi.vname);
	    UD.VS.add vi un)) UD.VS.empty f.slocals in
    
    (* a filter function for picking out
       the local variables that need to be kept *)
    let good_var vi =
      not(UD.VS.mem vi unused_set) &&
      not(IH.mem iioh vi.vid) &&
      not(IH.mem incdecHash vi.vid)
    in
    let good_locals = List.filter good_var f.slocals in
    f.slocals <- good_locals;
    unused_set <- unused;
    DoChildren

  (* remove instructions that set variables
     that aren't used. Also remove instructions
     that set variables mentioned in iioh *)
  method vstmt stm =

    (* return the list of pairs with fst = f *)
    let findf_in_pl f pl =
      List.filter (fun (fst,snd) ->
	if fst = f then true else false)
	pl
    in

    (* Return true if the assignment of this
       variable in this statement is going to be
       replaced by a post-inc/dec *)
    let check_incdec vi e =
      if IH.mem idDefHash vi.vid then
	let pl = IH.find idDefHash vi.vid in
	match findf_in_pl stm.sid pl with (sid,redefid)::l ->
	  let rhso = RD.getDefRhs redefid in
	  (match rhso with
	    None -> (if !debug then ignore (E.log "check_incdec: couldn't find rhs for def %d\n" redefid);
		     false)
	  | Some(rhs, indefs, indiosh) ->
	      (match rhs with
		RD.RDCall _ -> (if !debug then ignore (E.log "check_incdec: rhs not an expression\n");
				false)
	      | RD.RDExp e' -> 
		  if Util.equals e e' then true
		  else (if !debug then ignore (E.log "check_incdec: rhs of %d: %a, and needed redef %a not equal\n"
					      redefid d_plainexp e' d_plainexp e);
			false)))
	| [] -> (if !debug then ignore (E.log "check_incdec: current statement not in list: %d. %s = %a\n"
					    stm.sid vi.vname d_exp e);
		   false)
      else (if !debug then ignore (E.log "check_incdec: %s not in idDefHash\n" vi.vname);
	    false)
    in

    (* a filter function for picking out
       the instructions that we want to keep *)
    (* instr -> bool *)
    let good_instr i =
      match i with
	Set((Var(vi),_),e,_) ->
	  not (UD.VS.mem vi unused_set) &&
	  not (IH.mem incdecHash vi.vid) &&
	  not (check_incdec vi e)
      | Call (Some(Var(vi),_),_,_,_) ->
	  not (IH.mem iioh vi.vid)
      | _ -> true
    in

    match stm.skind with
      Instr il ->
	let newil = List.filter good_instr il in
	stm.skind <- Instr(newil);
	SkipChildren
    | _ -> DoChildren

end

class zraCilPrinterClass : cilPrinter = object (self)
  inherit defaultCilPrinterClass

  val mutable currentFormals : varinfo list = []
  val genvHtbl : (string, varinfo) H.t = H.create 128
  val lenvHtbl : (string, varinfo) H.t = H.create 128
  method private getLastNamedArgument (s: string) : exp =
    match List.rev currentFormals with 
      f :: _ -> Lval (var f)
    | [] -> 
        E.s (warn "Cannot find the last named argument when priting call to %s\n" s)

  (*** VARIABLES ***)

  (* give the varinfo for the variable to be printed,
   * returns the varinfo for the varinfo with that name
   * in the current environment. Raises Not_found and 
   * returns argument if the variable isn't in the 
   * environment *)
  method private getEnvVi (v:varinfo) : varinfo =
    try
      if H.mem lenvHtbl v.vname
      then H.find lenvHtbl v.vname
      else H.find genvHtbl v.vname
    with Not_found ->
      E.s (warn "variable %s not in pp environment\n" v.vname)

  (* True when v agrees with the entry in the environment for the name of v.
     False otherwise *)
  method private checkVi (v:varinfo) : bool =
    U.equals v (self#getEnvVi v)

  method private checkViAndWarn (v:varinfo) =
    if not (self#checkVi v) then
      ignore (warn "mentioned variable %s and its entry in the current environment have different varinfo.\n"
		v.vname)

  (** What terminator to print after an instruction. sometimes we want to 
   * print sequences of instructions separated by comma *)
  val mutable printInstrTerminator = ";"

  (* variable use *)
  method pVar (v:varinfo) =
    (* warn about instances where a possibly unintentionally 
       conflicting name is used *)
     if IH.mem iioh v.vid then
       let rhso = IH.find iioh v.vid in
       match rhso with
	 Some(Call(_,e,el,l)) ->
	   (* print a call instead of a temp variable *)
	   let oldpit = printInstrTerminator in
	   let _ = printInstrTerminator <- "" in
	   let d = self#pInstr () (Call(None,e,el,l)) in
	   let _ = printInstrTerminator <- oldpit in
	   d
       | _ -> (self#checkViAndWarn v;
	       text v.vname)
     else if IH.mem incdecHash v.vid then
       (* print an post-inc/dec instead of a temp variable *)
       let redefid, rhsvi, b = IH.find incdecHash v.vid in
       match b with
	 PlusA | PlusPI | IndexPI ->
	   text rhsvi.vname ++ text "++"
       | MinusA | MinusPI ->
	   text rhsvi.vname ++ text "--"
     else (self#checkViAndWarn v;
	   text v.vname)

 (* variable declaration *)
  method pVDecl () (v:varinfo) =
    (* See if the name is already in the environment with a 
       different varinfo. If so, give a warning.
       If not, add the name to the environment *)
    let _ = if (H.mem lenvHtbl v.vname) && not(self#checkVi v) then
      ignore( warn "name %s has already been declared locally with different varinfo\n" v.vname)
    else if (H.mem genvHtbl v.vname) && not(self#checkVi v) then
      ignore( warn "name %s has already been declared globally with different varinfo\n" v.vname)
    else if not v.vglob then 
      H.add lenvHtbl v.vname v 
    else
      H.add genvHtbl v.vname v in
    let stom, rest = separateStorageModifiers v.vattr in
    (* First the storage modifiers *)
    text (if v.vinline then "__inline " else "")
      ++ d_storage () v.vstorage
      ++ (self#pAttrs () stom)
      ++ (self#pType (Some (text v.vname)) () v.vtype)
      ++ text " "
      ++ self#pAttrs () rest

    (*** INSTRUCTIONS ****)
  method pInstr () (i:instr) =       (* imperative instruction *)
    match i with
    | Set(lv,e,l) -> begin
        (* Be nice to some special cases *)
        match e with
          BinOp((PlusA|PlusPI|IndexPI),Lval(lv'), Const(CInt64(one,_,_)),_)
            when lv == lv' && one = Int64.one && not !printCilAsIs ->
              self#pLineDirective l
                ++ self#pLval () lv
                ++ text (" ++" ^ printInstrTerminator)

        | BinOp((MinusA|MinusPI),Lval(lv'),
                Const(CInt64(one,_,_)), _) 
            when lv == lv' && one = Int64.one && not !printCilAsIs ->
                  self#pLineDirective l
                    ++ self#pLval () lv
                    ++ text (" --" ^ printInstrTerminator) 

        | BinOp((PlusA|PlusPI|IndexPI),Lval(lv'),Const(CInt64(mone,_,_)),_)
            when lv == lv' && mone = Int64.minus_one && not !printCilAsIs ->
              self#pLineDirective l
                ++ self#pLval () lv
                ++ text (" --" ^ printInstrTerminator)

        | BinOp((PlusA|PlusPI|IndexPI|MinusA|MinusPP|MinusPI|BAnd|BOr|BXor|
          Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
                Lval(lv'),e,_) when lv == lv' ->
                  self#pLineDirective l
                    ++ self#pLval () lv
                    ++ text " " ++ d_binop () bop
                    ++ text "= "
                    ++ self#pExp () e
                    ++ text printInstrTerminator
                    
        | _ ->
            self#pLineDirective l
              ++ self#pLval () lv
              ++ text " = "
              ++ self#pExp () e
              ++ text printInstrTerminator
              
    end
      (* In cabs2cil we have turned the call to builtin_va_arg into a 
       * three-argument call: the last argument is the address of the 
       * destination *)
    | Call(None, Lval(Var vi, NoOffset), [dest; SizeOf t; adest], l) 
        when vi.vname = "__builtin_va_arg" && not !printCilAsIs -> 
          let destlv = match stripCasts adest with 
            AddrOf destlv -> destlv
          | _ -> E.s (E.error "Encountered unexpected call to %s\n" vi.vname)
          in
          self#pLineDirective l
	    ++ self#pLval () destlv ++ text " = "
                   
            (* Now the function name *)
            ++ text "__builtin_va_arg"
            ++ text "(" ++ (align
                              (* Now the arguments *)
                              ++ self#pExp () dest 
                              ++ chr ',' ++ break 
                              ++ self#pType None () t
                              ++ unalign)
            ++ text (")" ^ printInstrTerminator)

      (* In cabs2cil we have dropped the last argument in the call to 
       * __builtin_stdarg_start. *)
    | Call(None, Lval(Var vi, NoOffset), [marker], l) 
        when vi.vname = "__builtin_stdarg_start" && not !printCilAsIs -> begin
          let last = self#getLastNamedArgument vi.vname in
          self#pInstr () (Call(None,Lval(Var vi,NoOffset),[marker; last],l))
        end

      (* In cabs2cil we have dropped the last argument in the call to 
       * __builtin_next_arg. *)
    | Call(res, Lval(Var vi, NoOffset), [ ], l) 
        when vi.vname = "__builtin_next_arg" && not !printCilAsIs -> begin
          let last = self#getLastNamedArgument vi.vname in
          self#pInstr () (Call(res,Lval(Var vi,NoOffset),[last],l))
        end

      (* In cparser we have turned the call to 
       * __builtin_types_compatible_p(t1, t2) into 
       * __builtin_types_compatible_p(sizeof t1, sizeof t2), so that we can
       * represent the types as expressions. 
       * Remove the sizeofs when printing. *)
    | Call(dest, Lval(Var vi, NoOffset), [SizeOf t1; SizeOf t2], l) 
        when vi.vname = "__builtin_types_compatible_p" && not !printCilAsIs -> 
        self#pLineDirective l
          (* Print the destination *)
        ++ (match dest with
              None -> nil
            | Some lv -> 
                self#pLval () lv ++ text " = ")
          (* Now the call itself *)
        ++ dprintf "%s(%a, %a)" vi.vname
             (self#pType None) t1  (self#pType None) t2
        ++ text printInstrTerminator
    | Call(_, Lval(Var vi, NoOffset), _, l) 
        when vi.vname = "__builtin_types_compatible_p" && not !printCilAsIs -> 
        E.s (bug "__builtin_types_compatible_p: cabs2cil should have added sizeof to the arguments.")
          
    | Call(dest,e,args,l) ->
        self#pLineDirective l
          ++ (match dest with
            None -> nil
          | Some lv -> 
              self#pLval () lv ++ text " = " ++
                (* Maybe we need to print a cast *)
                (let destt = typeOfLval lv in
                 let typeSig t = typeSigWithAttrs (fun al -> al) t in
                 (* typeSigNoAttrs stands in for typeSig, which hasn't been 
                    defined yet. *)
                match unrollType (typeOf e) with
                  TFun (rt, _, _, _) 
                      when not (Util.equals (typeSig rt)
                                            (typeSig destt)) ->
                    text "(" ++ self#pType None () destt ++ text ")"
                | _ -> nil))
          (* Now the function name *)
          ++ (let ed = self#pExp () e in
              match e with 
                Lval(Var _, _) -> ed
              | _ -> text "(" ++ ed ++ text ")")
          ++ text "(" ++ 
          (align
             (* Now the arguments *)
             ++ (docList ~sep:(chr ',' ++ break) 
                   (self#pExp ()) () args)
             ++ unalign)
        ++ text (")" ^ printInstrTerminator)

    | Asm(attrs, tmpls, outs, ins, clobs, l) ->
        if !msvcMode then
          self#pLineDirective l
            ++ text "__asm {"
            ++ (align
                  ++ (docList ~sep:line text () tmpls)
                  ++ unalign)
            ++ text ("}" ^ printInstrTerminator)
        else
          self#pLineDirective l
            ++ text ("__asm__ ") 
            ++ self#pAttrs () attrs 
            ++ text " ("
            ++ (align
                  ++ (docList ~sep:line
                        (fun x -> text ("\"" ^ escape_string x ^ "\""))
                        () tmpls)
                  ++
                  (if outs = [] && ins = [] && clobs = [] then
                    chr ':'
                else
                  (text ": "
                     ++ (docList ~sep:(chr ',' ++ break)
                           (fun (c, lv) ->
                             text ("\"" ^ escape_string c ^ "\" (")
                               ++ self#pLval () lv
                               ++ text ")") () outs)))
                ++
                  (if ins = [] && clobs = [] then
                    nil
                  else
                    (text ": "
                       ++ (docList ~sep:(chr ',' ++ break)
                             (fun (c, e) ->
                               text ("\"" ^ escape_string c ^ "\" (")
                                 ++ self#pExp () e
                                 ++ text ")") () ins)))
                  ++
                  (if clobs = [] then nil
                  else
                    (text ": "
                       ++ (docList ~sep:(chr ',' ++ break)
                             (fun c -> text ("\"" ^ escape_string c ^ "\""))
                             ()
                             clobs)))
                  ++ unalign)
            ++ text (")" ^ printInstrTerminator)
            

  (*** GLOBALS ***)
  method pGlobal () (g:global) : doc =       (* global (vars, types, etc.) *)
    match g with 
    | GFun (fundec, l) ->
        (* If the function has attributes then print a prototype because 
        * GCC cannot accept function attributes in a definition *)
        let oldattr = fundec.svar.vattr in
        (* Always pring the file name before function declarations *)
        let proto = 
          if oldattr <> [] then 
            (self#pLineDirective l) ++ (self#pVDecl () fundec.svar) 
              ++ chr ';' ++ line 
          else nil in
        (* Temporarily remove the function attributes *)
        fundec.svar.vattr <- [];
        let body = (self#pLineDirective ~forcefile:true l) 
                      ++ (self#pFunDecl () fundec) in
        fundec.svar.vattr <- oldattr;
        proto ++ body ++ line
          
    | GType (typ, l) ->
        self#pLineDirective ~forcefile:true l ++
          text "typedef "
          ++ (self#pType (Some (text typ.tname)) () typ.ttype)
          ++ text ";\n"

    | GEnumTag (enum, l) ->
        self#pLineDirective l ++
          text "enum" ++ align ++ text (" " ^ enum.ename) ++
          self#pAttrs () enum.eattr ++ text " {" ++ line
          ++ (docList ~sep:(chr ',' ++ line)
                (fun (n,i, loc) -> 
                  text (n ^ " = ") 
                    ++ self#pExp () i)
                () enum.eitems)
          ++ unalign ++ line ++ text "};\n"

    | GEnumTagDecl (enum, l) -> (* This is a declaration of a tag *)
        self#pLineDirective l ++
          text ("enum " ^ enum.ename ^ ";\n")

    | GCompTag (comp, l) -> (* This is a definition of a tag *)
        let n = comp.cname in
        let su, su1, su2 =
          if comp.cstruct then "struct", "str", "uct"
          else "union",  "uni", "on"
        in
        let sto_mod, rest_attr = separateStorageModifiers comp.cattr in
        self#pLineDirective ~forcefile:true l ++
          text su1 ++ (align ++ text su2 ++ chr ' ' ++ (self#pAttrs () sto_mod)
                         ++ text n
                         ++ text " {" ++ line
                         ++ ((docList ~sep:line (self#pFieldDecl ())) () 
                               comp.cfields)
                         ++ unalign)
          ++ line ++ text "}" ++
          (self#pAttrs () rest_attr) ++ text ";\n"

    | GCompTagDecl (comp, l) -> (* This is a declaration of a tag *)
        self#pLineDirective l ++
          text (compFullName comp) ++ text ";\n"

    | GVar (vi, io, l) ->
        self#pLineDirective ~forcefile:true l ++
          self#pVDecl () vi
          ++ chr ' '
          ++ (match io.init with
            None -> nil
          | Some i -> text " = " ++ 
                (let islong = 
                  match i with
                    CompoundInit (_, il) when List.length il >= 8 -> true
                  | _ -> false 
                in
                if islong then 
                  line ++ self#pLineDirective l ++ text "  " 
                else nil) ++
                (self#pInit () i))
          ++ text ";\n"
      
    (* print global variable 'extern' declarations, and function prototypes *)
    | GVarDecl (vi, l) ->
        self#pLineDirective l ++
          (self#pVDecl () vi)
          ++ text ";\n"

    | GAsm (s, l) ->
        self#pLineDirective l ++
          text ("__asm__(\"" ^ escape_string s ^ "\");\n")

    | GPragma (Attr(an, args), l) ->
        (* sm: suppress printing pragmas that gcc does not understand *)
        (* assume anything starting with "ccured" is ours *)
        (* also don't print the 'combiner' pragma *)
        (* nor 'cilnoremove' *)
        let suppress =
          not !print_CIL_Input && 
          not !msvcMode &&
          ((startsWith "box" an) ||
           (startsWith "ccured" an) ||
           (an = "merger") ||
           (an = "cilnoremove")) in
        let d =
	  match an, args with
	  | _, [] ->
              text an
	  | "weak", [ACons (symbol, [])] ->
	      text "weak " ++ text symbol
	  | _ ->
            text (an ^ "(")
              ++ docList ~sep:(chr ',') (self#pAttrParam ()) () args
              ++ text ")"
        in
        self#pLineDirective l 
          ++ (if suppress then text "/* " else text "")
          ++ (text "#pragma ")
          ++ d
          ++ (if suppress then text " */\n" else text "\n")

    | GText s  -> 
        if s <> "//" then 
          text s ++ text "\n"
        else
          nil


   method dGlobal (out: out_channel) (g: global) : unit = 
     (* For all except functions and variable with initializers, use the 
      * pGlobal *)
     match g with 
       GFun (fdec, l) -> 
         (* If the function has attributes then print a prototype because 
          * GCC cannot accept function attributes in a definition *)
         let oldattr = fdec.svar.vattr in
         let proto = 
           if oldattr <> [] then 
             (self#pLineDirective l) ++ (self#pVDecl () fdec.svar) 
               ++ chr ';' ++ line
           else nil in
         fprint out 80 (proto ++ (self#pLineDirective ~forcefile:true l));
         (* Temporarily remove the function attributes *)
         fdec.svar.vattr <- [];
         fprint out 80 (self#pFunDecl () fdec);               
         fdec.svar.vattr <- oldattr;
         output_string out "\n"

     | GVar (vi, {init = Some i}, l) -> begin
         fprint out 80 
           (self#pLineDirective ~forcefile:true l ++
              self#pVDecl () vi
              ++ text " = " 
              ++ (let islong = 
                match i with
                  CompoundInit (_, il) when List.length il >= 8 -> true
                | _ -> false 
              in
              if islong then 
                line ++ self#pLineDirective l ++ text "  " 
              else nil)); 
         self#dInit out 3 i;
         output_string out ";\n"
     end

     | g -> fprint out 80 (self#pGlobal () g)

  method private pFunDecl () f =
    H.add genvHtbl f.svar.vname f.svar;(* add function to global env *)
    H.clear lenvHtbl; (* new local environment *)
    (* add the arguments to the local environment *)
    List.iter (fun vi -> H.add lenvHtbl vi.vname vi) f.sformals;
    RD.computeRDs f;
    let nf = 
      if !doElimTemps
      then
	let _ = IH.clear iioh in
	let _ = IH.clear incdecHash in
	let _ = IH.clear idDefHash in
	let f' = visitCilFunction (new tempElimClass f) f in
	visitCilFunction (new unusedRemoverClass) f'
      else f in
    self#pVDecl () nf.svar
      ++  line
      ++ text "{ "
      ++ (align
	    (* locals. *)
	    ++ (docList ~sep:line (fun vi -> self#pVDecl () vi ++ text ";") 
                  () nf.slocals)
	    ++ line ++ line
	    (* the body *)
	    ++ ((* remember the declaration *) currentFormals <- nf.sformals; 
          let body = self#pBlock () nf.sbody in
          currentFormals <- [];
          body))
      ++ line
      ++ text "}"

end (* class zraCilPrinterClass *)

let zraCilPrinter = new zraCilPrinterClass

type outfile =
    { fname : string;
      fchan : out_channel }
let outChannel : outfile option ref = ref None

(* Processign of output file arguments *)
let openFile (what: string) (takeit: outfile -> unit) (fl: string) = 
  if !E.verboseFlag then
    ignore (Printf.printf "Setting %s to %s\n" what fl);
  (try takeit {fname = fl; fchan = open_out fl}
  with _ ->
    raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ fl)))

let feature : featureDescr = 
  { fd_name = "zrapp";              
    fd_enabled = ref false;
    fd_description = "pretty printing with checks for name conflicts and temp variable elimination";
    fd_extraopt = [
    "--zrapp_elim_temps",
    Arg.Unit (fun n -> doElimTemps := true),
    "Try to eliminate temporary variables during pretty printing";
    "--zrapp_debug",
    Arg.Unit (fun n -> debug := true; RD.debug := true),
    "Lots of debugging info for pretty printing and reaching definitions";];
    fd_doit = 
    (function (f: file) -> 
      lineDirectiveStyle := None;
      printerForMaincil := zraCilPrinter); 
    fd_post_check = false
  }

