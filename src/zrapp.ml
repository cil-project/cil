
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
module A = Cabs
module GA = GrowArray
module RCT = Rmciltmps
module DCE = Deadcodeelim

let doElimTemps = ref false
let debug = ref false
let printComments = ref false

(* Some(-1) => l1 < l2
   Some(0)  => l1 = l2
   Some(1)  => l1 > l2
   None => different files *)
let loc_comp l1 l2 =
  if String.compare l1.A.filename l2.A.filename != 0
  then None
  else if l1.A.lineno > l2.A.lineno
  then Some(1)
  else if l2.A.lineno > l1.A.lineno
  then Some(-1)
  else if l1.A.byteno > l2.A.byteno
  then Some(1)
  else if l2.A.byteno > l1.A.byteno
  then Some(-1)
  else Some(0)

let simpleGaSearch l =
  let hi = GA.max_init_index A.commentsGA in
  let rec loop i =
    if i < 0 then -1 else
    let (l',_,_) = GA.get A.commentsGA i in
    match loc_comp l l' with
      None -> loop (i-1)
    | Some(0) -> i
    | Some(-1) -> loop (i-1)
    | Some(1) -> i
    | _ -> E.s (E.error "simpleGaSearch: unexpected return from loc_comp\n")
  in
  loop hi

(* location -> string list *)
let get_comments l =
  let cabsl = {A.lineno = l.line; 
	       A.filename = l.file;
	       A.byteno = l.byte;} in
  let s = simpleGaSearch cabsl in

  let rec loop i cl =
    if i < 0 then cl else
    let (l',c,b) = GA.get A.commentsGA i in
    if String.compare cabsl.A.filename l'.A.filename != 0
    then loop (i - 1) cl
    else if b then cl
    else let _ = GA.set A.commentsGA i (l',c,true) in
    loop (i - 1) (c::cl)
  in
  List.rev (loop s [])

class zraCilPrinterClass : cilPrinter = object (self)
  inherit defaultCilPrinterClass

  val mutable currentFormals : varinfo list = []
  val genvHtbl : (string, varinfo) H.t = H.create 128
  val lenvHtbl : (string, varinfo) H.t = H.create 128
  method private getLastNamedArgument (s: string) : exp =
    match List.rev currentFormals with 
      f :: _ -> Lval (var f)
    | [] -> 
        E.s (warn "Cannot find the last named argument when priting call to %s." s)

  (*** VARIABLES ***)

  (* give the varinfo for the variable to be printed,
   * returns the varinfo for the varinfo with that name
   * in the current environment.
   * Returns argument and prints a warning if the variable 
   * isn't in the environment *)
  method private getEnvVi (v:varinfo) : varinfo =
    try
      if H.mem lenvHtbl v.vname
      then H.find lenvHtbl v.vname
      else H.find genvHtbl v.vname
    with Not_found ->
      ignore (warn "variable %s not in pp environment" v.vname);
      v

  (* True when v agrees with the entry in the environment for the name of v.
     False otherwise *)
  method private checkVi (v:varinfo) : bool =
    let v' = self#getEnvVi v in
    v.vid = v'.vid
(*    U.equals v (self#getEnvVi v w)*)

  method private checkViAndWarn (v:varinfo) =
    if not (self#checkVi v) then
      ignore (warn "mentioned variable %s and its entry in the current environment have different varinfo."
		v.vname)

  (** What terminator to print after an instruction. sometimes we want to 
   * print sequences of instructions separated by comma *)
  val mutable printInstrTerminator = ";"

  (** Get the comment out of a location if there is one *)
  method pLineDirective ?(forcefile=false) l =
      if !printComments then
	let c = String.concat "\n" (get_comments l) in
	match c with
	  "" -> nil
	| _ -> line ++ text "/*" ++ text c ++ text "*/" ++ line
      else nil

  (* variable use *)
  method pVar (v:varinfo) =
    (* warn about instances where a possibly unintentionally 
       conflicting name is used *)
     if IH.mem RCT.iioh v.vid then
       let rhso = IH.find RCT.iioh v.vid in
       match rhso with
	 Some(Call(_,e,el,l)) ->
	   (* print a call instead of a temp variable *)
	   let oldpit = printInstrTerminator in
	   let _ = printInstrTerminator <- "" in
	   let opc = !printComments in
	   let _ = printComments := false in
	   let c = match unrollType (typeOf e) with
	     TFun(rt,_,_,_) when not (Util.equals (typeSig rt) (typeSig v.vtype)) ->
	       text "(" ++ self#pType None () v.vtype ++ text ")"
	   | _ -> nil in
	   let d = self#pInstr () (Call(None,e,el,l)) in
	   let _ = printInstrTerminator <- oldpit in
	   let _ = printComments := opc in
	   c ++ d
       | _ -> 
	   if IH.mem RCT.incdecHash v.vid then
	     (* print an post-inc/dec instead of a temp variable *)
	     let redefid, rhsvi, b = IH.find RCT.incdecHash v.vid in
	     match b with
	       PlusA | PlusPI | IndexPI ->
		 text rhsvi.vname ++ text "++"
	     | MinusA | MinusPI ->
		 text rhsvi.vname ++ text "--"
	     | _ -> E.s (E.error "zraCilPrinterClass.pVar: unexpected op for inc/dec\n")
	   else (self#checkViAndWarn v;
		 text v.vname)
     else if IH.mem RCT.incdecHash v.vid then
       (* print an post-inc/dec instead of a temp variable *)
       let redefid, rhsvi, b = IH.find RCT.incdecHash v.vid in
       match b with
	 PlusA | PlusPI | IndexPI ->
	   text rhsvi.vname ++ text "++"
       | MinusA | MinusPI ->
	   text rhsvi.vname ++ text "--"
       | _ -> E.s (E.error "zraCilPrinterClass.pVar: unexpected op for inc/dec\n")
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
      (if !debug then ignore(E.log "zrapp: adding %s to local pp environment\n" v.vname);
      H.add lenvHtbl v.vname v)
    else
      (if !debug then ignore(E.log "zrapp: adding %s to global pp envirnoment\n" v.vname);
       H.add genvHtbl v.vname v) in
    let stom, rest = separateStorageModifiers v.vattr in
    (* First the storage modifiers *)
    self#pLineDirective v.vdecl ++
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

  method pFieldDecl () fi = 
    self#pLineDirective fi.floc ++
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
    let nf = 
      if !doElimTemps
      then RCT.eliminate_temps f
      else f in
    let decls = docList ~sep:line (fun vi -> self#pVDecl () vi ++ text ";")
	() nf.slocals in
    self#pVDecl () nf.svar
      ++  line
      ++ text "{ "
      ++ (align
	    (* locals. *)
	    ++ decls
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
    "Lots of debugging info for pretty printing and reaching definitions";
    "--zrapp_debug_fn",
    Arg.String (fun s -> RD.debug_fn := s),
    "Only output debugging info for one function";
    "--zrapp_comments",
    Arg.Unit (fun _ -> printComments := true),
    "Print comments from source file in output";];
    fd_doit = 
    (function (f: file) -> 
      lineDirectiveStyle := None;
      printerForMaincil := zraCilPrinter);
    fd_post_check = false
  }

