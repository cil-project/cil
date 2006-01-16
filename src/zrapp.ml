
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
    | Some(ivih,s ) ->
	match stm.skind with
	  Instr il ->
	    ivihl <- RD.instrRDs il (ivih,s) false;
	    DoChildren
	| _ -> DoChildren

  method vinst i =
    inst_iviho <- Some(List.hd ivihl);
    ivihl <- List.tl ivihl;
    DoChildren

  method vexpr e =
    match e with
      Lval (Var vi,off) ->
	(if String.length vi.vname >= 3 &&
	  compare (String.sub vi.vname 0 3) "tmp" == 0 then
	  (* only allowed to replace a tmp with a function call once *)
	  if IH.mem iioh vi.vid
	  then (IH.replace iioh vi.vid None; DoChildren)
	  else
	    (match inst_iviho with
	      Some(ivih,s) ->
		let ido = RD.ih_reverse_lookup ivih vi in
		(match ido with
		  Some id ->
		    let riviho = RD.getDefRhs id in
		    (match riviho with
		      Some(RD.RDExp(e) as r, defivih) ->
			if !debug then ignore(E.log "Can I replace %s with %a?\n" vi.vname d_exp e);
			if RD.ok_to_replace ivih defivih fd r
			then 
			  (if !debug then ignore(E.log "Yes.\n");
			   ChangeTo(e))
			else 
			  (if !debug then ignore(E.log "No.\n");
			   DoChildren)
		    | Some(RD.RDCall(i) as r, defivih) ->
			if !debug then ignore(E.log "Can I replace %s with %a?\n" vi.vname d_instr i);
			if RD.ok_to_replace ivih defivih fd r
			then (if !debug then ignore(E.log "Yes.\n");
			      IH.add iioh vi.vid (Some(i));
			      DoChildren)
			else (if !debug then ignore(E.log "No.\n");
			      DoChildren)
		    | _ -> DoChildren)
		| _ -> DoChildren)
	    | _ -> DoChildren)
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
      not(IH.mem iioh vi.vid)
    in
    let good_locals = List.filter good_var f.slocals in
    f.slocals <- good_locals;
    unused_set <- unused;
    DoChildren

  (* remove instructions that set variables
     that aren't used. Also remove instructions
     that set variables mentioned in iioh *)
  method vstmt stm =

    (* a filter function for picking out
       the instructions that we want to keep *)
    (* instr -> bool *)
    let good_instr i =
      match i with
	Set((Var(vi),_),_,_) ->
	  not (UD.VS.mem vi unused_set)
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

  (*** L-VALUES ***)
  method pLval () (lv:lval) =  (* lval (base is 1st field)  *)
    match lv with
      Var vi, o -> self#pOffset (self#pVar vi) o
    | Mem e, Field(fi, o) ->
        self#pOffset
          ((self#pExpPrec arrowLevel () e) ++ text ("->" ^ fi.fname)) o
    | Mem e, o ->
        self#pOffset
          (text "(*" ++ self#pExpPrec derefStarLevel () e ++ text ")") o

  (** Offsets **)
  method pOffset (base: doc) = function
    | NoOffset -> base
    | Field (fi, o) -> 
        self#pOffset (base ++ text "." ++ text fi.fname) o
    | Index (e, o) ->
        self#pOffset (base ++ text "[" ++ self#pExp () e ++ text "]") o

  method private pLvalPrec (contextprec: int) () lv = 
    if getParenthLevel (Lval(lv)) >= contextprec then
      text "(" ++ self#pLval () lv ++ text ")"
    else
      self#pLval () lv

  (*** EXPRESSIONS ***)
  method pExp () (e: exp) : doc = 
    let level = getParenthLevel e in
    match e with
      Const(c) -> d_const () c
    | Lval(l) -> self#pLval () l
    | UnOp(u,e1,_) -> 
        (d_unop () u) ++ chr ' ' ++ (self#pExpPrec level () e1)
          
    | BinOp(b,e1,e2,_) -> 
        align 
          ++ (self#pExpPrec level () e1)
          ++ chr ' ' 
          ++ (d_binop () b)
          ++ break 
          ++ (self#pExpPrec level () e2)
          ++ unalign

    | CastE(t,e) -> 
        text "(" 
          ++ self#pType None () t
          ++ text ")"
          ++ self#pExpPrec level () e

    | SizeOf (t) -> 
        text "sizeof(" ++ self#pType None () t ++ chr ')'
    | SizeOfE (e) ->  
        text "sizeof(" ++ self#pExp () e ++ chr ')'

    | SizeOfStr s -> 
        text "sizeof(" ++ d_const () (CStr s) ++ chr ')'

    | AlignOf (t) -> 
        text "__alignof__(" ++ self#pType None () t ++ chr ')'
    | AlignOfE (e) -> 
        text "__alignof__(" ++ self#pExp () e ++ chr ')'
    | AddrOf(lv) -> 
        text "& " ++ (self#pLvalPrec addrOfLevel () lv)
          
    | StartOf(lv) -> self#pLval () lv

  method private pExpPrec (contextprec: int) () (e: exp) = 
    let thisLevel = getParenthLevel e in
    let needParens =
      if thisLevel >= contextprec then
	true
      else if contextprec == bitwiseLevel then
        (* quiet down some GCC warnings *)
	thisLevel == additiveLevel || thisLevel == comparativeLevel
      else
	false
    in
    if needParens then
      chr '(' ++ self#pExp () e ++ chr ')'
    else
      self#pExp () e

  method pInit () = function 
      SingleInit e -> self#pExp () e
    | CompoundInit (t, initl) -> 
      (* We do not print the type of the Compound *)
(*
      let dinit e = d_init () e in
      dprintf "{@[%a@]}"
        (docList ~sep:(chr ',' ++ break) dinit) initl
*)
        let printDesignator = 
          if not !msvcMode then begin
            (* Print only for union when we do not initialize the first field *)
            match unrollType t, initl with
              TComp(ci, _), [(Field(f, NoOffset), _)] -> 
                if not (ci.cstruct) && ci.cfields != [] && 
                  (List.hd ci.cfields).fname = f.fname then
                  true
                else
                  false
            | _ -> false
          end else 
            false 
        in
        let d_oneInit = function
            Field(f, NoOffset), i -> 
              (if printDesignator then 
                text ("." ^ f.fname ^ " = ") 
              else nil) ++ self#pInit () i
          | Index(e, NoOffset), i -> 
              (if printDesignator then 
                text "[" ++ self#pExp () e ++ text "] = " else nil) ++ 
                self#pInit () i
          | _ -> E.s (unimp "Trying to print malformed initializer")
        in
        chr '{' ++ (align 
                      ++ ((docList ~sep:(chr ',' ++ break) d_oneInit) () initl) 
                      ++ unalign)
          ++ chr '}'
(*
    | ArrayInit (_, _, il) -> 
        chr '{' ++ (align 
                      ++ ((docList (chr ',' ++ break) (self#pInit ())) () il) 
                      ++ unalign)
          ++ chr '}'
*)
  (* dump initializers to a file. *)
  method dInit (out: out_channel) (ind: int) (i: init) = 
    (* Dump an array *)
    let dumpArray (bt: typ) (il: 'a list) (getelem: 'a -> init) = 
      let onALine = (* How many elements on a line *)
        match unrollType bt with TComp _ | TArray _ -> 1 | _ -> 4
      in
      let rec outputElements (isfirst: bool) (room_on_line: int) = function
          [] -> output_string out "}"
        | (i: 'a) :: rest -> 
            if not isfirst then output_string out ", ";
            let new_room_on_line = 
              if room_on_line == 0 then begin 
                output_string out "\n"; output_string out (String.make ind ' ');
                onALine - 1
              end else 
                room_on_line - 1
            in
            self#dInit out (ind + 2) (getelem i);
            outputElements false new_room_on_line rest
      in
      output_string out "{ ";
      outputElements true onALine il
    in
    match i with 
      SingleInit e -> 
        fprint out 80 (indent ind (self#pExp () e))
    | CompoundInit (t, initl) -> begin 
        match unrollType t with 
          TArray(bt, _, _) -> 
            dumpArray bt initl (fun (_, i) -> i)
        | _ -> 
            (* Now a structure or a union *)
            fprint out 80 (indent ind (self#pInit () i))
    end
(*
    | ArrayInit (bt, len, initl) -> begin
        (* If the base type does not contain structs then use the pInit 
        match unrollType bt with 
          TComp _ | TArray _ -> 
            dumpArray bt initl (fun x -> x)
        | _ -> *)
            fprint out 80 (indent ind (self#pInit () i))
    end
*)
        
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
            

  (**** STATEMENTS ****)
  method pStmt () (s:stmt) =        (* control-flow statement *)
    self#pStmtNext invalidStmt () s

  method dStmt (out: out_channel) (ind: int) (s:stmt) : unit = 
    fprint out 80 (indent ind (self#pStmt () s))

  method dBlock (out: out_channel) (ind: int) (b:block) : unit = 
    fprint out 80 (indent ind (align ++ self#pBlock () b))

  method private pStmtNext (next: stmt) () (s: stmt) =
    (* print the labels *)
    ((docList ~sep:line (fun l -> self#pLabel () l)) () s.labels)
      (* print the statement itself. If the labels are non-empty and the
      * statement is empty, print a semicolon  *)
      ++ 
      (if s.skind = Instr [] && s.labels <> [] then
        text ";"
      else
        (if s.labels <> [] then line else nil) 
          ++ self#pStmtKind next () s.skind)

  method private pLabel () = function
      Label (s, _, true) -> text (s ^ ": ")
    | Label (s, _, false) -> text (s ^ ": /* CIL Label */ ")
    | Case (e, _) -> text "case " ++ self#pExp () e ++ text ": "
    | Default _ -> text "default: "

  (* The pBlock will put the unalign itself *)
  method pBlock () (blk: block) = 
    let rec dofirst () = function
        [] -> nil
      | [x] -> self#pStmtNext invalidStmt () x
      | x :: rest -> dorest nil x rest
    and dorest acc prev = function
        [] -> acc ++ (self#pStmtNext invalidStmt () prev)
      | x :: rest -> 
          dorest (acc ++ (self#pStmtNext x () prev) ++ line)
            x rest
    in
    (* Let the host of the block decide on the alignment. The d_block will 
     * pop the alignment as well  *)
    text "{" 
      ++ 
      (if blk.battrs <> [] then 
        self#pAttrsGen true blk.battrs
      else nil)
      ++ line
      ++ (dofirst () blk.bstmts)
      ++ unalign ++ line ++ text "}"

  
  (* Store here the name of the last file printed in a line number. This is 
   * private to the object *)
  val mutable lastFileName = ""
  (* Make sure that you only call self#pLineDirective on an empty line *)
  method pLineDirective ?(forcefile=false) l = 
    currentLoc := l;
    match !lineDirectiveStyle with
    | Some style when l.line > 0 ->
	let directive =
	  match style with
	  | LineComment -> text "//#line "
	  | LinePreprocessorOutput when not !msvcMode -> chr '#'
	  | _ -> text "#line"
	in
	let filename =
          if forcefile || l.file <> lastFileName then
	    begin
	      lastFileName <- l.file;
	      text " \"" ++ text l.file ++ text "\""
            end
	  else
	    nil
	in
	leftflush ++ directive ++ chr ' ' ++ num l.line ++ filename ++ line
    | _ ->
	nil


  method private pStmtKind (next: stmt) () = function
      Return(None, l) ->
        self#pLineDirective l
          ++ text "return;"

    | Return(Some e, l) ->
        self#pLineDirective l
          ++ text "return ("
          ++ self#pExp () e
          ++ text ");"
          
    | Goto (sref, l) -> begin
        (* Grab one of the labels *)
        let rec pickLabel = function
            [] -> None
          | Label (l, _, _) :: _ -> Some l
          | _ :: rest -> pickLabel rest
        in
        match pickLabel !sref.labels with
          Some l -> text ("goto " ^ l ^ ";")
        | None -> 
            ignore (error "Cannot find label for target of goto\n");
            text "goto __invalid_label;"
    end

    | Break l ->
        self#pLineDirective l
          ++ text "break;"

    | Continue l -> 
        self#pLineDirective l
          ++ text "continue;"

    | Instr il ->
        align
          ++ (docList ~sep:line (fun i -> self#pInstr () i) () il)
          ++ unalign

    | If(be,t,{bstmts=[];battrs=[]},l) when not !printCilAsIs ->
        self#pLineDirective l
          ++ text "if"
          ++ (align
                ++ text " ("
                ++ self#pExp () be
                ++ text ") "
                ++ self#pBlock () t)
          
    | If(be,t,{bstmts=[{skind=Goto(gref,_);labels=[]}];
                battrs=[]},l)
     when !gref == next && not !printCilAsIs ->
       self#pLineDirective l
         ++ text "if"
         ++ (align
               ++ text " ("
               ++ self#pExp () be
               ++ text ") "
               ++ self#pBlock () t)

    | If(be,{bstmts=[];battrs=[]},e,l) when not !printCilAsIs ->
        self#pLineDirective l
          ++ text "if"
          ++ (align
                ++ text " ("
                ++ self#pExp () (UnOp(LNot,be,intType))
                ++ text ") "
                ++ self#pBlock () e)

    | If(be,{bstmts=[{skind=Goto(gref,_);labels=[]}];
           battrs=[]},e,l)
      when !gref == next && not !printCilAsIs ->
        self#pLineDirective l
          ++ text "if"
          ++ (align
                ++ text " ("
                ++ self#pExp () (UnOp(LNot,be,intType))
                ++ text ") "
                ++ self#pBlock () e)
          
    | If(be,t,e,l) ->
        self#pLineDirective l
          ++ (align
                ++ text "if"
                ++ (align
                      ++ text " ("
                      ++ self#pExp () be
                      ++ text ") "
                      ++ self#pBlock () t)
                ++ text " "   (* sm: indent next code 2 spaces (was 4) *)
                ++ (align
                      ++ text "else "
                      ++ self#pBlock () e)
          ++ unalign)
          
    | Switch(e,b,_,l) ->
        self#pLineDirective l
          ++ (align
                ++ text "switch ("
                ++ self#pExp () e
                ++ text ") "
                ++ self#pBlock () b)
    | Loop(b, l, _, _) -> begin
        (* Maybe the first thing is a conditional. Turn it into a WHILE *)
        try
          let term, bodystmts =
            let rec skipEmpty = function
                [] -> []
              | {skind=Instr [];labels=[]} :: rest -> skipEmpty rest
              | x -> x
            in
            (* Bill McCloskey: Do not remove the If if it has labels *)
            match skipEmpty b.bstmts with
              {skind=If(e,tb,fb,_); labels=[]} :: rest 
                                              when not !printCilAsIs -> begin
                match skipEmpty tb.bstmts, skipEmpty fb.bstmts with
                  [], {skind=Break _; labels=[]} :: _  -> e, rest
                | {skind=Break _; labels=[]} :: _, [] 
                                     -> UnOp(LNot, e, intType), rest
                | _ -> raise Not_found
              end
            | _ -> raise Not_found
          in
          self#pLineDirective l
            ++ text "wh"
            ++ (align
                  ++ text "ile ("
                  ++ self#pExp () term
                  ++ text ") "
                  ++ self#pBlock () {bstmts=bodystmts; battrs=b.battrs})

        with Not_found ->
          self#pLineDirective l
            ++ text "wh"
            ++ (align
                  ++ text "ile (1) "
                  ++ self#pBlock () b)
    end
    | Block b -> align ++ self#pBlock () b
      
    | TryFinally (b, h, l) -> 
        self#pLineDirective l 
          ++ text "__try "
          ++ align 
          ++ self#pBlock () b
          ++ text " __fin" ++ align ++ text "ally "
          ++ self#pBlock () h

    | TryExcept (b, (il, e), h, l) -> 
        self#pLineDirective l 
          ++ text "__try "
          ++ align 
          ++ self#pBlock () b
          ++ text " __e" ++ align ++ text "xcept(" ++ line
          ++ align
          (* Print the instructions but with a comma at the end, instead of 
           * semicolon *)
          ++ (printInstrTerminator <- ","; 
              let res = 
                (docList ~sep:line (self#pInstr ())
                   () il) 
              in
              printInstrTerminator <- ";";
              res)
          ++ self#pExp () e
          ++ text ") " ++ unalign
          ++ self#pBlock () h


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

   method pFieldDecl () fi = 
     (self#pType
        (Some (text (if fi.fname = missingFieldName then "" else fi.fname)))
        () 
        fi.ftype)
       ++ text " "
       ++ (match fi.fbitfield with None -> nil 
       | Some i -> text ": " ++ num i ++ text " ")
       ++ self#pAttrs () fi.fattr
       ++ text ";"

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

  (***** PRINTING DECLARATIONS and TYPES ****)
    
  method pType (nameOpt: doc option) (* Whether we are declaring a name or 
                                      * we are just printing a type *)
               () (t:typ) =       (* use of some type *)
    let name = match nameOpt with None -> nil | Some d -> d in
    let printAttributes (a: attributes) = 
      let pa = self#pAttrs () a in
      match nameOpt with 
      | None when not !print_CIL_Input && not !msvcMode -> 
          (* Cannot print the attributes in this case because gcc does not 
           * like them here, except if we are printing for CIL, or for MSVC. 
           * In fact, for MSVC we MUST print attributes such as __stdcall *)
          if pa = nil then nil else 
          text "/*" ++ pa ++ text "*/"
      | _ -> pa
    in
    match t with 
      TVoid a ->
        text "void"
          ++ self#pAttrs () a 
          ++ text " " 
          ++ name

    | TInt (ikind,a) -> 
        d_ikind () ikind 
          ++ self#pAttrs () a 
          ++ text " "
          ++ name

    | TFloat(fkind, a) -> 
        d_fkind () fkind 
          ++ self#pAttrs () a 
          ++ text " " 
          ++ name

    | TComp (comp, a) -> (* A reference to a struct *)
        let su = if comp.cstruct then "struct" else "union" in
        text (su ^ " " ^ comp.cname ^ " ") 
          ++ self#pAttrs () a 
          ++ name
          
    | TEnum (enum, a) -> 
        text ("enum " ^ enum.ename ^ " ")
          ++ self#pAttrs () a 
          ++ name
    | TPtr (bt, a)  -> 
        (* Parenthesize the ( * attr name) if a pointer to a function or an 
         * array. However, on MSVC the __stdcall modifier must appear right 
         * before the pointer constructor "(__stdcall *f)". We push them into 
         * the parenthesis. *)
        let (paren: doc option), (bt': typ) = 
          match bt with 
            TFun(rt, args, isva, fa) when !msvcMode -> 
              let an, af', at = partitionAttributes ~default:AttrType fa in
              (* We take the af' and we put them into the parentheses *)
              Some (text "(" ++ printAttributes af'), 
              TFun(rt, args, isva, addAttributes an at)

          | TFun _ | TArray _ -> Some (text "("), bt

          | _ -> None, bt
        in
        let name' = text "*" ++ printAttributes a ++ name in
        let name'' = (* Put the parenthesis *)
          match paren with 
            Some p -> p ++ name' ++ text ")" 
          | _ -> name' 
        in
        self#pType 
          (Some name'')
          () 
          bt'

    | TArray (elemt, lo, a) -> 
        (* ignore the const attribute for arrays *)
        let a' = dropAttributes [ "const" ] a in 
        let name' = 
          if a' == [] then name else
          if nameOpt == None then printAttributes a' else 
          text "(" ++ printAttributes a' ++ name ++ text ")" 
        in
        self#pType 
          (Some (name'
                   ++ text "[" 
                   ++ (match lo with None -> nil | Some e -> self#pExp () e)
                   ++ text "]"))
          ()
          elemt
          
    | TFun (restyp, args, isvararg, a) -> 
        let name' = 
          if a == [] then name else 
          if nameOpt == None then printAttributes a else
          text "(" ++ printAttributes a ++ name ++ text ")" 
        in
        self#pType 
          (Some
             (name'
                ++ text "("
                ++ (align 
                      ++ 
                      (if args = Some [] && isvararg then 
                        text "..."
                      else
                        (if args = None then nil 
                        else if args = Some [] then text "void"
                        else 
                          let pArg (aname, atype, aattr) = 
                            let stom, rest = separateStorageModifiers aattr in
                            (* First the storage modifiers *)
                            (self#pAttrs () stom)
                              ++ (self#pType (Some (text aname)) () atype)
                              ++ text " "
                              ++ self#pAttrs () rest
                          in
                          (docList ~sep:(chr ',' ++ break) pArg) () 
                            (argsToList args))
                          ++ (if isvararg then break ++ text ", ..." else nil))
                      ++ unalign)
                ++ text ")"))
          ()
          restyp

  | TNamed (t, a) ->
      text t.tname ++ self#pAttrs () a ++ text " " ++ name

  | TBuiltin_va_list a -> 
      text "__builtin_va_list"
       ++ self#pAttrs () a 
        ++ text " " 
        ++ name


  (**** PRINTING ATTRIBUTES *********)
  method pAttrs () (a: attributes) = 
    self#pAttrsGen false a


  (* Print one attribute. Return also an indication whether this attribute 
   * should be printed inside the __attribute__ list *)
  method pAttr (Attr(an, args): attribute) : doc * bool =
    (* Recognize and take care of some known cases *)
    match an, args with 
      "const", [] -> text "const", false
          (* Put the aconst inside the attribute list *)
    | "aconst", [] when not !msvcMode -> text "__const__", true
    | "thread", [] when not !msvcMode -> text "__thread", false
(*
    | "used", [] when not !msvcMode -> text "__attribute_used__", false 
*)
    | "volatile", [] -> text "volatile", false
    | "restrict", [] -> text "__restrict", false
    | "missingproto", [] -> text "/* missing proto */", false
    | "cdecl", [] when !msvcMode -> text "__cdecl", false
    | "stdcall", [] when !msvcMode -> text "__stdcall", false
    | "fastcall", [] when !msvcMode -> text "__fastcall", false
    | "declspec", args when !msvcMode -> 
        text "__declspec(" 
          ++ docList (self#pAttrParam ()) () args
          ++ text ")", false
    | "w64", [] when !msvcMode -> text "__w64", false
    | "asm", args -> 
        text "__asm__(" 
          ++ docList (self#pAttrParam ()) () args
          ++ text ")", false
    (* we suppress printing mode(__si__) because it triggers an *)
    (* internal compiler error in all current gcc versions *)
    (* sm: I've now encountered a problem with mode(__hi__)... *)
    (* I don't know what's going on, but let's try disabling all "mode"..*)
    | "mode", [ACons(tag,[])] -> 
        text "/* mode(" ++ text tag ++ text ") */", false

    (* sm: also suppress "format" because we seem to print it in *)
    (* a way gcc does not like *)
    | "format", _ -> text "/* format attribute */", false

    (* sm: here's another one I don't want to see gcc warnings about.. *)
    | "mayPointToStack", _ when not !print_CIL_Input 
    (* [matth: may be inside another comment.]
      -> text "/*mayPointToStack*/", false 
    *)
      -> text "", false

    | _ -> (* This is the dafault case *)
        (* Add underscores to the name *)
        let an' = if !msvcMode then "__" ^ an else "__" ^ an ^ "__" in
        if args = [] then 
          text an', true
        else
          text (an' ^ "(") 
            ++ (docList (self#pAttrParam ()) () args)
            ++ text ")", 
          true

  method pAttrParam () = function 
    | AInt n -> num n
    | AStr s -> text ("\"" ^ escape_string s ^ "\"")
    | ACons(s, []) -> text s
    | ACons(s,al) ->
        text (s ^ "(")
          ++ (docList (self#pAttrParam ()) () al)
          ++ text ")"
    | ASizeOfE a -> text "sizeof(" ++ self#pAttrParam () a ++ text ")"
    | ASizeOf t -> text "sizeof(" ++ self#pType None () t ++ text ")"
    | ASizeOfS ts -> text "sizeof(<typsig>)"
    | AAlignOfE a -> text "__alignof__(" ++ self#pAttrParam () a ++ text ")"
    | AAlignOf t -> text "__alignof__(" ++ self#pType None () t ++ text ")"
    | AAlignOfS ts -> text "__alignof__(<typsig>)"
    | AUnOp(u,a1) -> 
        let d_unop () u =
          match u with
            Neg -> text "-"
          | BNot -> text "~"
          | LNot -> text "!"
        in
        (d_unop () u) ++ text " (" ++ (self#pAttrParam () a1) ++ text ")"

    | ABinOp(b,a1,a2) -> 
        align 
          ++ text "(" 
          ++ (self#pAttrParam () a1)
          ++ text ") "
          ++ (d_binop () b)
          ++ break 
          ++ text " (" ++ (self#pAttrParam () a2) ++ text ") "
          ++ unalign
    | ADot (ap, s) -> (self#pAttrParam () ap) ++ text ("." ^ s)
          
  (* A general way of printing lists of attributes *)
  method private pAttrsGen (block: bool) (a: attributes) = 
    (* Scan all the attributes and separate those that must be printed inside 
     * the __attribute__ list *)
    let rec loop (in__attr__: doc list) = function
        [] -> begin 
          match in__attr__ with
            [] -> nil
          | _ :: _->
              (* sm: added 'forgcc' calls to not comment things out
               * if CIL is the consumer; this is to address a case
               * Daniel ran into where blockattribute(nobox) was being
               * dropped by the merger
               *)
              (if block then 
                text (" " ^ (forgcc "/*") ^ " __blockattribute__(")
               else
                 text "__attribute__((")

                ++ (docList ~sep:(chr ',' ++ break)
                      (fun a -> a)) () in__attr__
                ++ text ")"
                ++ (if block then text (forgcc "*/") else text ")")
        end
      | x :: rest -> 
          let dx, ina = self#pAttr x in
          if ina then 
            loop (dx :: in__attr__) rest
          else
            dx ++ text " " ++ loop in__attr__ rest
    in
    let res = loop [] a in
    if res = nil then
      res
    else
      text " " ++ res ++ text " "

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

