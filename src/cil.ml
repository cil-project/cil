open Pretty
open Trace      (* sm: 'trace' function *)
module E = Errormsg
module H = Hashtbl

(*
 * CIL: An intermediate language for analyzing C progams.
 *
 * Version Tue Dec 12 15:21:52 PST 2000 
 * Scott McPeak, George Necula, Wes Weimer
 *
 *)
(* CIL is intended to be an IL for source-to-source transformations of C 
 * programs. In the process of converting preprocessed C source to CIL most 
 * of the syntactic sugar is removed and type checking is performed. CIL has 
 * the following properties:

 - has a pretty printer that should print valid source. There is also a 
 * pretty printer that prints abstract syntax (for debugging)

 - all local variables are pulled to the start of a function. Their names are 
 * changed to be unique and to avoid conflicts with global variables

 - all forms of source-level loops are changed into a single Loop construct 
 * that loops forever except if a break, return or goto is encountered. In 
 * many cases the continue statement is turned into a goto. 

 - all implicit integer promotions, argument promotions and arithmetic 
 * conversions are turned into explicit casts.

 - all integer constants have the proper type attached to them

 - the function arguments with array type are changed into pointers.
*)
(*
 * Note: you may *NOT* change the order of the fields or the order in
 * which disjoint union choices are presented. The C translation code
 * has those values hard-wired.
 *)

(* TODO
   - inner struct/union/enum/typedef tags
   - clean up attributes
   - functions vs. function pointers
   - type of sizeof is hardwired to UInt
   - integerFits is hardwired to true
   - in cabs2cil we drop the volatile sometimes
*)

(* A few globals that control the interpretation of C source *)
let msvcMode = ref false              (* Whether the pretty printer should 
                                       * print output for the MS VC 
                                       * compiler. Default is GCC *)
let charIsUnsigned = ref false        (* Whether CHAR is unsigned. Default 
                                       * false *)
let ilongFitsUInt = ref false         (* Whether a signed long can fit an 
                                       * unsigned integer. True only if a 
                                       * long uses more bits than an int  *)

type location = { 
    line: int;				(* -1 means "do not know" *)
    col: int;
    file: string; 
}

let locUnknown = { line = -1; col = -1; file = ""; }

(* Information about a variable. Use one of the makeLocalVar, makeTempVar or 
 * makeGlobalVar to create instances of this data structure. These structures a
 * re shared by all references to the variable. So, you can change the name
 * easily, for example *)
type varinfo = { 
    mutable vid: int;	(* Unique integer indentifier. For globals this is a 
                         * hash of the name. Locals are numbered from 0 
                         * starting with the formal arguments. This field 
                         * will be set for you if you use one of the 
                         * makeLocalVar, makeTempVar or makeGlobalVar *)
    mutable vname: string;				
    mutable vglob: bool;	(* Is this a global variable? *)

    mutable vtype: typ;                 (* The declared type *)
    mutable vdecl: location;            (* where was this variable declared? *)
    mutable vattr: attribute list;
    mutable vstorage: storage;
    mutable vaddrof: bool;              (* Has its address taken *)

    (* sm: is this var referenced?  this is computed by removeUnusedVars *)
    mutable vreferenced: bool;
}

                                        (* Storage-class information *)
and storage = 
    NoStorage |                         (* The default storage *)
    Static | 
    StaticInitializer of exp |          (* static with an initializer *)
    Register | 
    Extern

(* Information about a struct/union field *)
and fieldinfo = { 
    mutable fcomp: compinfo;            (* The compinfo of the host. Note 
                                         * that this must be shared with the 
                                         * host since there can be only one 
                                         * compinfo for a given id *)
    mutable fname: string;              (* The name of the field. Might be 
                                         * "___missing_field_name" in which 
                                         * case it is not printed *)
    mutable ftype: typ;
    mutable fattr: attribute list;
}


(* Information about a composite type (a struct or a union). Use mkCompInfo 
 * to create non-recursive or (potentially) recursive versions of this  *)
and compinfo = {
    mutable cstruct: bool;              (* true if struct *)
    mutable cname: string;              (* the name. Always non-empty. If it 
                                         * starts with @ then it is not 
                                         * printed. Use compSetName to set 
                                         * the name and the key. Use 
                                         * compFullName to get the full name 
                                         * of a comp *)
    mutable ckey: int;                  (* A unique integer. Use Hashtbl.hash 
                                         * on the string returned by 
                                         * compFullName. All compinfo for a 
                                         * given key are shared.  *)
    mutable cfields: fieldinfo list;
    mutable cattr:   attribute list;    (* The attributes that are defined at 
                                         * the same time as the composite 
                                         * type *)
  } 
    
(* what is the type of an expression? Keep all attributes sorted. Use 
 * addAttribute and addAttributes to construct list of attributes *)
and typ =
    TVoid of attribute list
  | TInt of ikind * attribute list
  | TBitfield of ikind * int * attribute list
  | TFloat of fkind * attribute list
           (* name, tags with values, attributes. The tag list should be 
            * non-empty  *)
  | TEnum of string * (string * exp) list * attribute list

  | TPtr of typ * attribute list        (* Pointer type. The attributes refer 
                                         * to the  *)

              (* base type and length *)
  | TArray of typ * exp option * attribute list

               (* Structs and Unions: isstruct, name, fields, attributes, 
                * self cell.*)
  | TComp of compinfo
               (* The field list can be empty *)
               (* The name is never empty. mkCompInfo will create a unique 
                * name for anonymous types *)


               (* Composite types can be part of circular type structure. 
                * Thus a struct and a union can be referred in a TForward. 
                * But all compinfo for a given ckey are shared!. The 
                * attributes are in addition to the attributes contained in 
                * the compinfo *)
  | TForward of compinfo * attribute list

               (* result, args, isVarArg, attributes *)
  | TFun of typ * varinfo list * bool * attribute list

  | TNamed of string * typ * attribute list (* From a typedef. The attributes 
                                             * are in addition to the 
                                             * attributes of the named type  *)


(* kinds of integers *)
and ikind = 
    IChar | ISChar | IUChar
  | IInt | IUInt
  | IShort | IUShort
  | ILong | IULong
  | ILongLong | IULongLong 

and fkind = 
    FFloat | FDouble | FLongDouble

and attribute = 
    AId of string                       (* Atomic attributes *)
  | AInt of int
  | AStr of string 
  | AVar of varinfo
  | ACons of string * attribute list       (* Constructed attributes *)

(* literal constants *)
and constant =
  | CInt of int * ikind * string option  (* Give the ikind (see ISO9899 
                                          * 6.4.4.1) and the textual 
                                          * representation, if available. Use 
                                          * "integer" to create these  *)
  | CStr of string
  | CChr of char 
  | CReal of float * fkind * string option(* Give the fkind (see ISO 6.4.4.2) 
                                           * and also the textual 
                                           * representation, if available *)

(* unary operations *)
and unop =
    Neg                                 (* unary - *)
  | BNot                                (* ~ *)
  | LNot                                (* ! *)

(* binary operations *)
and binop =
    PlusA                               (* arithemtic + *)
  | PlusPI                              (* pointer + integer *)
  | IndexPI                             (* pointer[integer]. The integer is 
                                         * very likely positive *)
  | MinusA                              (* arithemtic - *)
  | MinusPI                             (* pointer - integer *)
  | MinusPP                             (* pointer - pointer *)
  | Mult
  | Div
  | Mod

  | Shiftlt                             (* shift left *)
  | Shiftrt                             (* shift right *)

  | Lt| Gt| Le| Ge| Eq | Ne             (* arithemtic comparisons *)

  | LtP| GtP| LeP| GeP| EqP| NeP        (* pointer comparisons *)

  | BAnd                                (* bitwise and *)
  | BXor                                (* exclusive-or *)
  | BOr                                 (* inclusive-or *)

                                        (* Comparison operations *)

(* expressions, no side effects *)
and exp =
    Const      of constant
  | Lval       of lval                  (* l-values *)
  | SizeOf    of typ                   (* Has UInt type ! (ISO 6.5.3.4). 
                                         * Only sizeof for types is 
                                         * available. This is not turned into 
                                         * a constant because some 
                                         * transformations might want to 
                                         * change types *) 

  | SizeOfE   of exp                    (* Like SizeOf *)

                                        (* Give the type of the result *)
  | UnOp       of unop * exp * typ

                                        (* Give the type of the result. The 
                                         * arithemtic conversions are made 
                                         * explicit for the arguments *)
  | BinOp      of binop * exp * exp * typ

  | Question   of exp * exp * exp      (* e1 ? e2 : e3. Sometimes we cannot 
                                        * turn this into a conditional 
                                        * statement (e.g. in global 
                                        * initializers)  *)
  | CastE      of typ * exp            (* Use doCast to make casts *)

                                        (* Used only for initializers of 
                                         * structures and arrays.  *) 
  | Compound   of typ * (offset option * exp) list
  | AddrOf     of lval

  | StartOf    of lval                  (* There is no C correspondent for 
                                         * this. C has implicit coercions 
                                         * from an array to the address of 
                                         * the first element and from a 
                                         * function to the start address of 
                                         * the function. StartOf is used in 
                                         * CIL to simplify type checking and 
                                         * is just an explicit form of the 
                                         * above mentioned implicit 
                                         * convertions *)


(* L-Values denote contents of memory addresses. A memory address is 
 * expressed as a base plus an offset. The base address can be the start 
 * address of storage for a local or global variable or, in general, any 
 * expression. We distinguish the two cases to avoid gratuituous introduction 
 * of the AddrOf operators on variables whose address would not be taken 
 * otherwise. *)

and lval =
    lbase * offset  

(* The meaning of an lval is expressed as a function "[lval] = (a, T)" that 
 * returns a memory address "a" and a type "T" of the object storred starting 
 * at the address "a".  *)

(* The meaning of an lbase is expressed as a similar function. *)

(* The meaning of an offset is expressed as a function "[offset](a, T) = (a', 
 * T')" whose result also depends on a base address "a" and a base type "T". 
 * The result is another address and another base type  *)

(* With this notation we define
  
      [(lbase, offset)] = [offset] [lbase]
*)
and lbase = 
  | Var        of varinfo               (* denotes the address & v, or if v 
                                         * is an array then just v *)
    (* [Var v] = (&v, typeOf(v)) *)


  | Mem        of exp                   (* denotes an address expressed as an 
                                         * expression. Use mkMem to make 
                                         * these  *)
    (* [Mem e] = (e, T) if typeOf(e) = Ptr(T) *)

and offset = 
  | NoOffset                            (* l *)
    (* [NoOffset](a, T) = (a, T) *)

  | Field      of fieldinfo * offset    (* l.f + offset. l must be a struct 
                                         * or an union and l.f is the element 
                                         * type *)
    (* [Field(f, off)](a, struct {f : T, ...}) = [off](a + offsetof(f), T) *)

  | Index    of exp * offset           (* l[e] + offset. l must be an array 
                                        * and l[e] has the element type *)
    (* [Index(e, off)](a, array(T)) = [off](a + e * sizeof(T), T) *)

(* the following equivalences hold *)
(* Index(0, off) = off                                                 *)
(* Mem(StartOf lv), NoOffset = StartOf (lv) if lv is a function *)
(* Mem(AddrOf(Mem a, aoff)), off   = Mem(a, aoff + off)                *)
(* Mem(AddrOf(Var v, aoff)), off   = Var(v, aoff + off)                *)

(**** INSTRUCTIONS. May cause effects directly but may not have control flow.*)
and instr =
    Set        of lval * exp             (* An assignment. A cast is present 
                                          * if the exp has different type 
                                          * from lval *)
  | Call       of (varinfo * bool) option * exp * exp list
 			 (* optional: result temporary variable and an 
                          * indication that a cast is necessary (the declared 
                          * type of the function is not the same as that of 
                          * the result), the function value, argument list, 
                          * location. Casts are inserted for arguments. If 
                          * the type of the result variable is not the same 
                          * as the declared type of the function result then 
                          * an implicit cast exists. *)

                         (* See the GCC specification for the meaning of ASM. 
                          * If the source is MS VC then only the templates 
                          * are used *)
  | Asm        of string list *         (* templates (CR-separated) *)
                  bool *                (* if it is volatile *)
                  (string * lval) list * (* outputs must be lvals with 
                                          * constraints. I would like these 
                                          * to be actually variables, but I 
                                          * run into some trouble with ASMs 
                                          * in the Linux sources  *)
                  (string * exp) list * (* inputs with constraints *)
                  string list           (* register clobbers *)

(**** STATEMENTS. Mostly structural information ****)
and stmt = 
  | Skip                                (* empty statement *)
  | Sequence of stmt list               (* Use mkSeq to make a Sequence. This 
                                         * will optimize the result and will 
                                         * make sure that there are no 
                                         * trailing Default or Label or Case *)
  | Loop of stmt                        (* A loop. When stmt is done the 
                                         * control starts back with stmt. 
                                         * Ends with break or a Goto outside.*)
  | IfThenElse of exp * stmt * stmt * location    (* if *)
  | Label of string 
  | Goto of string
  | Return of exp option * location
  | Switch of exp * stmt * location     (* no work done to break this appart *)
  | Case of int * location              (* The case expressions are resolved *)
  | Default 
  | Break
  | Continue
  | Instr of instr * location
        
type fundec = 
    { mutable svar:     varinfo;        (* Holds the name and type as a 
                                         * variable, so we can refer to it 
                                         * easily from the program *)
      mutable sformals: varinfo list;   (* These are the formals. These must 
                                         * be shared with the formals that 
                                         * appear in the type of the 
                                         * function. Do not make copies of 
                                         * these because the body refers to 
                                         * them. *)
      mutable slocals: varinfo list;    (* locals, DOES NOT include the 
                                         * sformals. Do not make copies of 
                                         * these because the body refers to 
                                         * them  *)
      mutable smaxid: int;              (* max local id. Starts at 0 *)
      mutable sbody: stmt;              (* the body *)
    } 

type global = 
    GFun of fundec * location           (* A function definition. Cannot have 
                                         * storage Extern *)
  | GType of string * typ * location    (* A typedef *)

  | GDecl of varinfo * location         (* A variable declaration. Might be a 
                                         * prototype. There might be at most 
                                         * one declaration and at most one 
                                         * definition for a given variable. 
                                         * If both forms appear then they 
                                         * must share the same varinfo. A 
                                         * prototype shares the varinfo with 
                                         * the fundec of the definition. 
                                         * Either has storage Extern or 
                                         * there must be a definition (Gvar 
                                         * or GFun) in this file  *)
  | GVar  of varinfo * exp option * location      
                                        (* A variable definition. Might have 
                                         * an initializer. There must be at 
                                         * most one definition for a variable 
                                         * in an entire program. Cannot have 
                                         * storage Extern *)
  | GAsm of string * location           (* Global asm statement. These ones 
                                         * can contain only a template *)
  | GPragma of attribute * location     (* Pragmas at top level. Use the same 
                                         * syntax as attributes *)
  | GText of string                     (* Some text (printed verbatim) at 
                                         * top level. E.g., this way you can 
                                         * put comments in the output.  *)
    

type file = 
    { mutable fileName: string;
      mutable globals: global list;
      mutable globinit: fundec option;  (* A global initializer. It is not 
                                         * part of globals and it is printed 
                                         * last *)
    } 
	(* global function decls, global variable decls *)

(* sm: cil visitor interface for traversing Cil trees *)
(* no provision for modifying trees at this time *)
class type cilVisitor = object
  method vvrbl : varinfo -> bool     (* variable *)
  method vvdec : varinfo -> bool     (* variable declaration *)
  method vexpr : exp -> bool         (* expression *)
  method vlval : lval -> bool        (* lval (base is 1st field) *)
  method voffs : offset -> bool      (* lval offset *)
  method vinst : instr -> bool       (* imperative instruction *)
  method vstmt : stmt -> bool        (* constrol-flow statement *)
  method vfunc : fundec -> bool      (* function definition *)
  method vfuncPost : fundec -> bool  (*   postorder version *)
  method vglob : global -> bool      (* global (vars, types, etc.) *)
  method vtype : typ -> bool         (* use of some type *)
  method vtdec : string -> typ -> bool    (* typedef *)
end

(* the default visitor does nothing at each node, but does *)
(* not stop; hence they return true *)
class nopCilVisitor = object
  method vvrbl (v:varinfo) = true     (* variable *)
  method vvdec (v:varinfo) = true     (* variable declaration *)
  method vexpr (e:exp) = true         (* expression *)
  method vlval (l:lval) = true        (* lval (base is 1st field) *)
  method voffs (o:offset) = true      (* lval offset *)
  method vinst (i:instr) = true       (* imperative instruction *)
  method vstmt (s:stmt) = true        (* constrol-flow statement *)
  method vfunc (f:fundec) = true      (* function definition *)
  method vfuncPost (f:fundec) = true  (*   postorder version *)
  method vglob (g:global) = true      (* global (vars, types, etc.) *)
  method vtype (t:typ) = true         (* use of some type *)
  method vtdec (s:string) (t:typ) = true    (* typedef *)
end

(* as an example, here is a visitor that visits expressions *)
(* note how objects capture constructor arguments for use later, *)
(* even though they are not stored explicitly in fields *)
class cilExprVisitor (ve : exp -> unit) = object
  inherit nopCilVisitor    (* get default nop actions *)
  method vexpr e =
    (ve e);                (* call the ctor arg *)
    true                   (* and keep going *)
end




let lu = locUnknown

    (* A special location that we use to mark that a BinOp was created from 
     * an index *)
let luindex = { line = -1000; col = -1; file = ""; }

let integerFits (i: int) (k: ikind) =  true (* We know that i is less than 31 
                                             * bits so it fits even in an 
                                             * IInt *)

let integerKinds (i: int) (posskinds: ikind list) (s: string option) = 
  let rec loop = function
      [] -> E.s (E.bug "integerkinds exhausted kinds")
    | k :: rest -> 
        if integerFits i k then
          CInt(i, k, s)
        else loop rest
  in
  loop posskinds
          

let integer i = Const (integerKinds i [IInt] None)(* For now only ints *)
let kinteger (k: ikind) (i: int) = Const (CInt(i, k,  None))
let hexinteger i = 
    Const (integerKinds i [IInt] (Some (Printf.sprintf "0x%08X" i)))
             
let zero      = integer 0
let one       = integer 1
let mone      = integer (-1)

        

let voidType = TVoid([])
let intType = TInt(IInt,[])
let uintType = TInt(IUInt,[])
let longType = TInt(ILong,[])
let ulongType = TInt(IULong,[])
let charType = TInt(IChar, [])
let charPtrType = TPtr(charType,[])
let charConstPtrType = TPtr(charType,[AId("const")])
let voidPtrType = TPtr(voidType, [])
let intPtrType = TPtr(intType, [])
let uintPtrType = TPtr(uintType, [])
let doubleType = TFloat(FDouble, [])

let structId = ref 0 (* Find a better way to generate new names *)
let newTypeName n = 
  incr structId;
  "@anon" ^ n ^ (string_of_int (!structId))


(** Construct sorted lists of attributes ***)
let rec addAttribute a al = 
    let an = match a with
        AId s -> s
      | ACons (s, _) -> s
      | _ -> E.s (E.unimp "Unexpected attribute at top level")
    in 
    let rec insertSorted = function
        [] -> [a]
      | (a0 :: rest) as l -> 
          let an0 =
            match a0 with
              AId s -> s
            | ACons (s, _) -> s
            | _ -> E.s (E.unimp "Unexpected attribute at top level")
          in 
          if an < an0 then a :: l
          else if an > an0 then a0 :: insertSorted rest
          else if a = a0 then l else a :: l
    in
    insertSorted al

and addAttributes al0 al = 
    if al0 == [] then al else
    if al  == [] then al0 else
    List.fold_left (fun acc a -> addAttribute a acc) al al0

and dropAttribute (al: attribute list) (a: attribute) = 
  let rec amatch a a' = 
    match a, a' with
      AId s, AId s' when s = s' -> true
    | AInt n, AInt n' when n = n' -> true
    | AStr s, AStr s' when s = s' -> true
    | AVar vi, AVar vi' when vi.vid = vi'.vid -> true
    | ACons (s, args), ACons(s', args') when
        s = s' (* && (List.for_all2 amatch args args') *) -> true
    | _ -> false
  in
  List.filter (fun a' -> not (amatch a a')) al

and dropAttributes (todrop: attribute list) (al : attribute list) =
  List.fold_left dropAttribute al todrop

and filterAttributes (s: string) (al: attribute list) = 
  let amatch = function
    AId s' when s = s' -> true
  | ACons (s', _) when s = s' -> true
  | _ -> false
  in
  List.filter amatch al

(* sm: *)
let hasAttribute s al =
  (filterAttributes s al <> [])

(* Get the full name of a comp *)
let compFullName comp = 
  (if comp.cstruct then "struct " else "union ") ^ comp.cname

(* Set the name of a composite type. Also changes the key *)
let compSetName comp n = 
  comp.cname <- n;
  comp.ckey <- H.hash (compFullName comp)

 
(** Creates a a (potentially recursive) composite type **)
let mkCompInfo
               (isstruct: bool) 
               (n: string)   (* empty for anonymous structures *)
               (* fspec is a function that when given a forward 
                * representation of the structure type constructs the type of 
                * the fields. The function can ignore this argument if not 
                * constructing a recursive type.  *)
               (mkfspec: typ -> (string * typ * attribute list) list) 
               (a: attribute list) : compinfo =
   (* make an new name for anonymous structs *)
   let n = if n = "" then 
     newTypeName (if isstruct then "struct" else "union") else n in
   (* Make a new self cell and a forward reference *)
   let comp = 
     { cstruct = isstruct; cname = ""; ckey = 0; cfields = [];
       cattr = a; } in
   compSetName comp n;  (* fix the name and the key *)
   let self = ref voidType in
   let tforward = TForward (comp, []) in
   let flds = 
       List.map (fun (fn, ft, fa) -> { fcomp = comp;
                                       ftype = ft;
                                       fname = fn;
                                       fattr = fa }) (mkfspec tforward) in
   comp.cfields <- flds;
   comp

(**** Utility functions ******)
let rec unrollType = function   (* Might drop some attributes !! *)
    TNamed (_, r, _) -> unrollType r
  | TForward (comp, _) -> TComp comp
  | x -> x



                                   
let var vi : lval = (Var vi, NoOffset)
let mkSet lv e = Instr(Set(lv,e), lu)
let assign vi e = mkSet (var vi) e
let call res f args = Instr(Call(res,f,args), lu)

let mkString s = Const(CStr s)

    (* Make a sequence out of a list of statements *)
let mkSeq sl = 
  let rec removeSkip = function 
      [] -> []
    | Skip :: rest -> removeSkip rest
    | Sequence (sl) :: rest -> removeSkip (sl @ rest)
    | ((Default | Label _ | Case _) as last) :: rest -> 
        let rest' = removeSkip rest in
        if rest' = [] then
          last :: [Skip]                (* Put a ; after default or a label*)
        else
          last :: rest'
    | s :: rest -> s :: removeSkip rest
  in
  match removeSkip sl with 
    [] -> Skip
  | [s] -> s
  | sl' -> Sequence(sl')



let mkWhile (guard:exp) (body: stmt list) : stmt = 
  (* Do it like this so that the pretty printer recognizes it *)
  Loop (Sequence (IfThenElse(guard, Skip, Break, lu) :: body))

let mkFor (start: stmt) (guard: exp) (next: stmt) (body: stmt list) : stmt = 
  mkSeq 
    (start ::
     mkWhile guard (body @ [next]) :: [])


let mkForIncr (iter: varinfo) (first: exp) (past: exp) (incr: exp) 
    (body: stmt list) : stmt = 
      (* See what kind of operator we need *)
      let compop, nextop = 
        match unrollType iter.vtype with
          TPtr _ -> LtP, PlusPI
        | _ -> Lt, PlusA
      in
      mkFor (mkSet (var iter) first)
        (BinOp(compop, Lval(var iter), past, intType))
        (mkSet (var iter) 
           (BinOp(nextop, Lval(var iter), incr, iter.vtype)))
        body
  


(* the name of the C function we call to get ccgr ASTs
external parse : string -> file = "cil_main"
*)
(* 
  Pretty Printing
 *)

(* location *)
let d_loc () l =
  dprintf "/*(%s:%d:%d)*/" l.file l.line l.col

let escape_char c = 
  let conv v = 
    String.make 1 
      (Char.chr (v + (if v < 10 then (Char.code '0') 
      else (Char.code 'a' - 10)))) 
  in
  match c with
    '\n' -> "\\n"
  | '\034' -> "\\\""   (* This is the doublequote in ASCII since otherwise it 
                          bothers the CAML fontification in emacs *)
  | '\'' -> "\\'"
  | '\r' -> "\\r"
  | '\t' -> "\\t"
  | '\b' -> "\\b"
  | '\000' -> "\\0"
  | '\\' -> "\\\\"
  | _ -> 
      let esc = String.make 1 c in
      if esc = Char.escaped c then esc
      else 
        let code = Char.code c in
        "\\"
        ^ (conv (code / 64))
        ^ (conv ((code mod 64) / 8))
        ^ (conv (code mod 8))

let escape_string str =
  let lng = String.length str in
  let conv v = 
    String.make 1 
      (Char.chr (v + (if v < 10 then (Char.code '0') 
      else (Char.code 'a' - 10)))) 
  in
  let rec build idx =
    if idx >= lng then ""
    else
      (escape_char (String.get str idx)) ^ (build (idx + 1)) 
  in
  build 0	

  
let d_ikind () = function
    IChar -> text "char"
  | ISChar -> text "signed char"
  | IUChar -> text "unsigned char"
  | IInt -> text "int"
  | IUInt -> text "unsigned int"
  | IShort -> text "short"
  | IUShort -> text "unsigned short"
  | ILong -> text "long"
  | IULong -> text "unsigned long"
  | ILongLong -> 
      if !msvcMode then text "__int64" else text "long long"
  | IULongLong -> 
      if !msvcMode then text "unsigned __int64" 
      else text "unsigned long long"

let d_fkind () = function
    FFloat -> text "float"
  | FDouble -> text "double"
  | FLongDouble -> text "long double"

let d_storage () = function
    NoStorage -> nil
  | Static -> text "static "
  | StaticInitializer e -> text "static "
  | Extern -> text "extern "
  | Register -> text "register "

(* constant *)
let d_const () c =
  match c with
    CInt(_, _, Some s) -> text s
  | CInt(i, _, None) -> num i
  | CStr(s) -> dprintf "\"%s\"" (escape_string s)
  | CChr(c) -> dprintf "'%s'" (escape_char c)
  | CReal(_, _, Some s) -> text s
  | CReal(f, _, None) -> dprintf "%f" f

(* Parentheses level. An expression "a op b" is printed parenthesized if its 
 * parentheses level is >= that that of its context. Identifiers have the 
 * lowest level and weakly binding operators (e.g. |) have the largest level 
 *)
let derefStarLevel = 20
let indexLevel = 20
let arrowLevel = 20
let addrOfLevel = 30
let bitwiseLevel = 75
let additiveLevel = 60
let getParenthLevel = function
  | Compound _ -> 100

  | Question _ -> 80
                                        (* Bit operations. *)
  | BinOp((BOr|BXor|BAnd),_,_,_) -> bitwiseLevel (* 75 *)

                                        (* Comparisons *)
  | BinOp((Eq|Ne|Gt|Lt|Ge|Le|EqP|NeP|GtP|LtP|GeP|LeP),_,_,_) -> 70
                                        (* Additive. Shifts can have higher 
                                         * level but I want parentheses 
                                         * around them *)
  | BinOp((MinusA|MinusPP|MinusPI|PlusA|
           PlusPI|IndexPI|Shiftlt|Shiftrt),_,_,_)  
    -> additiveLevel (* 60 *)

                                        (* Multiplicative *)
  | BinOp((Div|Mod|Mult),_,_,_) -> 40

                                        (* Unary *)
  | CastE(_,_) -> 30
  | AddrOf(_) -> 30
  | StartOf(_) -> 30
  | UnOp((Neg|BNot|LNot),_,_) -> 30

                                        (* Lvals *)
  | Lval(Mem _ , _) -> 20                   
  | Lval(Var _, (Field _|Index _)) -> 20
  | SizeOf _ | SizeOfE _ -> 20

  | Lval(Var _, NoOffset) -> 0        (* Plain variables *)
  | Const _ -> 0                        (* Constants *)


(* types. Call with a function that when invoked will fill-in the declared 
 * name  *)

  (* When we print types for consumption by another compiler we must be 
   * careful to avoid printing multiple type definitions *)
let noRedefinitions = ref false
let definedTypes : ((string * string), bool) H.t = H.create 17
let canPrintName n =
  (not !noRedefinitions) || 
  (try begin
    ignore (H.find definedTypes n); false
  end with Not_found -> begin
    H.add definedTypes n true;
    true
  end)


(* Some attributes are printed before and others after. The before ones are 
 * typically the qualifiers  *)
let rec separateAttributes (pre, post) = function
    [] -> pre,post
  | ((AId "const" | AId "volatile" | AId "inline" |
      AId "cdecl" | AId "stdcall") as a) :: rest -> 
      separateAttributes (a :: pre, post) rest
  | a :: rest ->
      separateAttributes (pre, a :: post) rest
let separateAttributes a = separateAttributes ([], []) a    

(* Print attributes in a custom way *)
let d_attrcustom : (attribute -> Pretty.doc option) ref = 
  let d_attrcustombase = function
    | AId("const") -> Some (text "const")
    | AId("inline") -> Some (text "inline")
    | AId("volatile") -> Some (text "volatile")
    | AId("cdecl") when !msvcMode -> Some (text "__cdecl")
    | AId("stdcall") when !msvcMode -> Some (text "__stdcall")
    | _ -> None
  in
  ref d_attrcustombase

let setCustomPrint custom f = 
  let ocustom = !d_attrcustom in
  let newPrint a = 
    match custom a with
      None -> ocustom a
    | x -> x
  in
  d_attrcustom := newPrint;
  fun x -> 
    let res = f x in
    d_attrcustom := ocustom;
    res

let printShortTypes = ref false

let rec d_storage_initializer () = function
    StaticInitializer e -> dprintf "= %a" d_exp e
  | _ -> nil
        
and d_decl (docName: unit -> doc) () this = 
  let parenth outer_t doc = 
    let typ_strength = function         (* binding strength of type 
                                         * constructors  *)
      | TArray _ -> 11
      | TPtr _ -> 10
      | TFun _ -> 12
      | _ -> 1
    in
    if typ_strength outer_t > typ_strength this then 
      dprintf "(%a)" insert doc
    else
      doc
  in match this with 
    TVoid a -> dprintf "void%a %t" d_attrlistpost a docName
  | TInt (ikind,a) -> dprintf "%a%a %t" d_ikind ikind d_attrlistpost a docName
  | TBitfield(ikind,i,a) -> 
      dprintf "%a %t : %d%a" d_ikind ikind docName i d_attrlistpost a
  | TFloat(fkind, a) -> dprintf "%a%a %t" d_fkind fkind 
        d_attrlistpost a docName
  | TComp comp -> 
      let n = comp.cname in
      let n' = 
        if String.length n >= 5 && String.sub n 0 5 = "@anon" then "" else n in
      let su, su1, su2 = 
        if comp.cstruct then "struct", "str", "uct"
                        else "union",  "uni", "on"
      in
      if not (!printShortTypes) && (n' = "" || canPrintName (su, n')) then
        dprintf "%s@[%s %s%a {@!%a@]@!} %t " su1 su2 n' 
          d_attrlistpost comp.cattr
          (docList line (d_fielddecl ())) comp.cfields docName
      else
        dprintf "%s%s %s %t " su1 su2 n' docName
  | TForward (comp, a) -> 
      let su = if comp.cstruct then "struct" else "union" in
      dprintf "%s %s %a%t" su comp.cname d_attrlistpre a docName

  | TEnum (n, kinds, a) -> 
      let n' = 
        if String.length n >= 5 && String.sub n 0 5 = "@anon" then "" else n in
      if not (!printShortTypes) && 
        (n' = "" || canPrintName ("enum", n')) then
        dprintf "enum@[ %s%a {%a@]@?} %t" n' d_attrlistpost a
          (docList line (fun (n,i) -> dprintf "%s = %a,@?" n d_exp i)) kinds
          docName
      else
        dprintf "enum %s %t" n' docName

  | TPtr (TFun(tres, args, isva, af) as t, ap) when !msvcMode ->  (* !!! *)
      let rec stripCallAttr (call, notcall) = function
          [] -> call, notcall
        | ((AId("cdecl")|AId("stdcall")) as a) :: rest ->
            stripCallAttr (a :: call, notcall) rest
        | a :: rest -> stripCallAttr (call, a :: notcall) rest
      in
      let call, notcall = stripCallAttr ([], []) ap in
      d_decl (fun _ -> parenth t (dprintf "%a* %a%t" 
                                    d_attrlistpre call
                                    d_attrlistpost notcall docName )) 
             () (TFun(tres, args, isva, []))

  | TPtr (t, a)  -> 
      d_decl (fun _ -> parenth t (dprintf "* %a%t" d_attrlistpre a docName )) 
             () t

  | TArray (t, lo, a) -> 
      d_decl (fun _ -> parenth t
          (dprintf "%t[%a]%a" 
             docName
             insert (match lo with None -> nil
             | Some e -> d_exp () e)
             d_attrlistpost a))
        ()
        t
  | TFun (restyp, args, isvararg, a) -> 
      let args' = 
        match args with 
            [] -> [ { vname = "";
                      vtype = if isvararg then voidPtrType else voidType;
                      vid   = 0;
                      vglob = false;
                      vattr = [];
                      vdecl = lu;
                      vaddrof = false; 
                      vreferenced = false;   (* sm *)
                      vstorage = NoStorage; } ] 
        | _ -> args
      in
      d_decl (fun _ -> 
        parenth restyp 
          (dprintf "%a %t(@[%a%a@])" 
             d_attrlistpost a
             docName
             (docList (chr ',' ++ break) (d_videcl ())) args'
             insert (if isvararg then text ", ..." else nil)))
        ()
        restyp

  | TNamed (n, _, a) -> dprintf "%a %s %t" d_attrlistpost a n docName


(* Only a type (such as for a cast) *)        
and d_type () t = d_decl (fun _ -> nil) () t


(* exp *)

                                        (* Rest *)

(* Print an expression assuming a precedence for the context. Use a small 
 * number to parenthesize the printed expression. 0 guarantees parentheses. 1 
 * will parenthesize everything but identifiers. *)
and d_expprec contextprec () e = 
  let thisLevel = getParenthLevel e in
                                 (* This is to quite down GCC warnings *)
  if thisLevel >= contextprec || (thisLevel = additiveLevel &&
                                  contextprec = bitwiseLevel) then
    dprintf "(%a)" d_exp e
  else
    d_exp () e

and d_exp () e = 
  let level = getParenthLevel e in
  match e with
    Const(c) -> dprintf "%a" d_const c
  | Lval(l) -> dprintf "%a" d_lval l
  | UnOp(u,e1,_) -> 
      let d_unop () u =
        match u with
          Neg -> text "-"
        | BNot -> text "~"
        | LNot -> text "!"
      in
      dprintf "%a %a" d_unop u (d_expprec level) e1

  | BinOp(b,e1,e2,_) -> 
      dprintf "@[%a %a@?%a@]" 
        (d_expprec level) e1 d_binop b (d_expprec level) e2
  | Question (e1, e2, e3) -> 
      dprintf "%a ? %a : %a"
        (d_expprec level) e1 (d_expprec level) e2 (d_expprec level) e3
  | CastE(t,e) -> dprintf "(%a)%a" d_type t (d_expprec level) e
  | SizeOf (t) -> dprintf "sizeof(%a)" d_type t
  | SizeOfE (e) -> dprintf "sizeof(%a)" d_exp e
  | Compound (t, initl) -> 
      (* We do not print the type of the Compound *)
      let dinit = function
          None, e -> d_exp () e
        | Some o, e -> 
            if !msvcMode then
              ignore (E.log "Warning: Printing designators in initializers. MS VC does not support them\n");
            let rec d_offset () = function
              | NoOffset -> dprintf "=%a" d_exp e
              | Field (fi, o) -> dprintf ".%s%a" fi.fname d_offset o
              | Index (e, o) -> dprintf "[%a]%a" d_exp e d_offset o
            in
            d_offset () o
      in
      dprintf "{@[%a@]}"
        (docList (chr ',' ++ break) dinit) initl
  | AddrOf(lv) -> 
      dprintf "& %a" (d_lvalprec addrOfLevel) lv

  | StartOf(lv) -> d_lval () lv

and d_binop () b =
  match b with
    PlusA | PlusPI | IndexPI -> text "+"
  | MinusA | MinusPP | MinusPI -> text "-"
  | Mult -> text "*"
  | Div -> text "/"
  | Mod -> text "%"
  | Shiftlt -> text "<<"
  | Shiftrt -> text ">>"
  | Lt | LtP -> text "<"
  | Gt | GtP -> text ">"
  | Le | LeP -> text "<="
  | Ge | GeP -> text ">="
  | Eq | EqP -> text "=="
  | Ne | NeP -> text "!="
  | BAnd -> text "&"
  | BXor -> text "^"
  | BOr -> text "|"
        
(* attributes *)
and d_attr () = function
    AId s -> text s
  | AInt n -> num n
  | AStr s -> dprintf "\"%s\"" (escape_string s)
  | AVar vi -> text vi.vname
  | ACons(s,al) -> dprintf "%s(%a)" s
        (docList (chr ',') (d_attr ())) al
          
and d_attrlist pre () al = (* Whether it comes before or after stuff *)
  (* Take out the special attributes *)
  let rec loop remaining = function
      [] -> begin
        match remaining with
          [] -> nil
        | _ -> dprintf "__attribute__((%a)) "
              (docList (chr ',' ++ break) 
                 (fun a -> dprintf "%a" d_attr a)) remaining 
      end
    | x :: rest -> begin
        match !d_attrcustom x with
          Some xd -> dprintf "%a %a" insert xd insert (loop remaining rest)
        | None -> loop (x :: remaining) rest
    end
  in
  let res = loop [] al in
  if res = nil then
    res
  else
    if pre then res ++ text " " else text " " ++ res
    
and d_attrlistpre () al = d_attrlist true () al
and d_attrlistpost () al = d_attrlist false () al

(* lvalue *)
and d_lvalprec contextprec () lv = 
  if getParenthLevel (Lval(lv)) >= contextprec then
    dprintf "(%a)" d_lval lv
  else
    d_lval () lv
  
and d_lval () lv = 
  let rec d_offset dobase = function
    | NoOffset -> dobase ()
    | Field (fi, o) -> 
        d_offset (fun _ -> dprintf "%t.%s" dobase fi.fname) o
    | Index (Const(CInt(0,_,_)), NoOffset) -> dprintf "(*%t)" dobase
    | Index (e, o) -> 
        d_offset (fun _ -> dprintf "%t[%a]" dobase d_exp e) o
  in
  match lv with
    Var vi, o -> d_offset (fun _ -> text vi.vname) o
  | Mem e, Field(fi, o) -> 
      d_offset (fun _ -> 
        dprintf "%a->%s" (d_expprec arrowLevel) e fi.fname) o
(*  | Mem e, NoOffset -> dprintf "(*%a)" (d_expprec derefStarLevel) e *)
  | Mem e, o -> 
      d_offset (fun _ -> dprintf "(*%a)" (d_expprec derefStarLevel) e) o
        
and d_instr () i =
  match i with
  | Set(lv,e) -> begin
      (* Be nice to some special cases *)
      match e with
        BinOp((PlusA|PlusPI|IndexPI),Lval(lv'),Const(CInt(1,_,_)),_) 
          when lv == lv' -> 
          dprintf "%a ++;" d_lval lv
      | BinOp((MinusA|MinusPI),Lval(lv'),
              Const(CInt(1,_,_)), _) when lv == lv' -> 
          dprintf "%a --;" d_lval lv
      | BinOp((PlusA|PlusPI|IndexPI|MinusA|MinusPP|MinusPI|BAnd|BOr|BXor|
               Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
              Lval(lv'),e,_) when lv == lv' -> 
          dprintf "%a %a= %a;" d_lval lv d_binop bop d_exp e
      | _ -> dprintf "%a = %a;" d_lval lv d_exp e
  end
  | Call(vio,e,args) ->
      dprintf "%a%a(@[%a@]);" 
        insert 
        (match vio with 
          None -> nil | 
          Some (vi, iscast) -> 
            if iscast then
              dprintf "%s = (%a)" vi.vname d_type vi.vtype 
            else 
              dprintf "%s = " vi.vname)
        insert 
        (match e with Lval(Var _, _) -> d_exp () e 
        | _ -> dprintf "(%a)" d_exp e)
	(docList (chr ',' ++ break) (d_exp ())) args

  | Asm(tmpls, isvol, outs, ins, clobs) ->
      if !msvcMode then
        dprintf "__asm {@[%a@]};@!"  (docList line text) tmpls
      else
        dprintf "__asm__ %a(@[%a%a%a%a@]);@!"
          insert (if isvol then text "__volatile__" else nil)
          (docList line 
             (fun x -> dprintf "\"%s\"" (escape_string x))) tmpls
          insert 
          (if outs = [] && ins = [] && clobs = [] then 
            nil
          else 
            dprintf ": %a" (docList (chr ',' ++ break) 
                              (fun (c, lv) -> dprintf "\"%s\" (%a)"
                                  (escape_string c) d_lval lv)) outs)
          insert
          (if ins = [] && clobs = [] then
            nil
          else
            dprintf ": %a" (docList (chr ',' ++ break) 
                              (fun (c, e) -> dprintf "\"%s\" (%a)"
                                  (escape_string c) d_exp e)) ins)
          insert 
          (if clobs = [] then nil
          else
            dprintf ": %a" (docList (chr ',' ++ break) 
                              (fun x -> dprintf "\"%s\"" (escape_string x))) 
              clobs)
       
and d_stmt () s =
  let doThen a = (* Want to print { } in the "then" branch to avoid 
                  dangling elses. ASM gives some problems with MSVC so 
                  bracket them as well  *) 
    match a with
      IfThenElse _ | Loop _ | Instr(Asm _, _) | Switch _ -> 
        Sequence [a]
    | _ -> a
  in
  match s with
    Skip -> dprintf ";"
  | Sequence(lst) -> dprintf "@[{ @[@!%a@]@!}@]" 
        (docList line (d_stmt ())) lst
  | Loop(Sequence(IfThenElse(e,Skip,Break,_) :: rest)) -> 
      dprintf "wh@[ile (%a)@!%a@]" d_exp e d_stmt (Sequence rest)
  | Loop(stmt) -> 
      dprintf "wh@[ile (1)@!%a@]" d_stmt stmt
  | IfThenElse(e,a,(Skip|Sequence([Skip])),_) -> 
      dprintf "if@[ (%a)@!%a@]" d_exp e d_stmt (doThen a)
  | IfThenElse(e,a,b,_) -> 
      dprintf "@[if@[ (%a)@!%a@]@!el@[se@!%a@]@]" 
        d_exp e d_stmt (doThen a) d_stmt b
  | Label(s) -> dprintf "%s:" s
  | Case(i,_) -> dprintf "case %d: " i
  | Goto(s) -> dprintf "goto %s;" s
  | Break  -> dprintf "break;"
  | Continue -> dprintf "continue;"
  | Return(None,_) -> text "return;"
  | Return(Some e,_) -> dprintf "return (%a);" d_exp e
  | Switch(e,s,_) -> dprintf "@[switch (%a)@!%a@]" d_exp e d_stmt s
  | Default -> dprintf "default:"
  | Instr(i,_) -> d_instr () i


        
and d_fun_decl () f = 
  let pre, post = separateAttributes f.svar.vattr in
  (* Now take out the inline *)
  let isinline, pre' = 
    match List.partition (fun a -> a = AId("inline")) pre with
      [], _ -> false, pre
    | _, pre' -> true, pre'
  in
  dprintf "%s%a%a %a@!{ @[%a@!@!%a@]@!}" 
    (if isinline then 
      if !msvcMode then "__inline " else "inline " 
     else "")
    d_storage f.svar.vstorage
    (* the prototype *)
    (d_decl (fun _ -> dprintf "%a%s" d_attrlistpre pre' f.svar.vname)) 
    f.svar.vtype
    d_attrlistpost post
    (* locals. *)
    (docList line (fun vi -> d_videcl () vi ++ text ";")) f.slocals
    (* the body *)
    d_stmt f.sbody

and d_videcl () vi = 
  let pre, post = separateAttributes vi.vattr in 
  (* Now take out the inline *)
  let isinline, pre' = 
    match List.partition (fun a -> a = AId("inline")) pre with
      [], _ -> false, pre
    | _, pre' -> true, pre'
  in
  dprintf "%s%a%a %a %a"
    (if isinline then 
      if !msvcMode then "__inline " else "inline " 
     else "")
    d_storage vi.vstorage
    (d_decl (fun _ -> dprintf "%a %s" d_attrlistpre pre' vi.vname)) vi.vtype
    d_attrlistpost post
    d_storage_initializer vi.vstorage
    
and d_fielddecl () fi = 
  dprintf "%a %a;"
    (d_decl (fun _ -> 
      text (if fi.fname = "___missing_field_name" then "" else fi.fname))) 
    fi.ftype
    d_attrlistpost fi.fattr
       

   (* Some plain pretty-printers. Unlike the above these expose all the 
    * details of the internal representation *)
let rec d_plainexp () = function
    Const(c) -> dprintf "Const(%a)" d_const c
  | Lval(lv) -> dprintf "Lval(@[%a@])" d_plainlval lv
  | CastE(t,e) -> dprintf "CastE(@[%a,@?%a@])" d_plaintype t d_plainexp e
  | StartOf lv -> dprintf "StartOf(%a)" d_plainlval lv
  | AddrOf (lv) -> dprintf "AddrOf(%a)" d_plainlval lv
  | e -> d_exp () e

and d_plainlval () = function
  | Var vi, o -> dprintf "Var(@[%s,@?%a@])" vi.vname d_plainoffset o
  | Mem e, o -> dprintf "Mem(@[%a,@?%a@])" d_plainexp e d_plainoffset o

and d_plainoffset () = function
    NoOffset -> text "NoOffset"
  | Field(fi,o) -> 
      dprintf "Field(@[%s:%a,@?%a@])" 
        fi.fname d_plaintype fi.ftype d_plainoffset o
  | Index(e, o) -> dprintf "Index(@[%a,@?%a@])" d_plainexp e d_plainoffset o

and d_plaintype () = function
    TVoid a -> dprintf "TVoid(@[%a@])" d_attrlistpost a
  | TInt(ikind, a) -> dprintf "TInt(@[%a,@?%a@])" 
        d_ikind ikind d_attrlistpost a
  | TFloat(fkind, a) -> 
      dprintf "TFloat(@[%a,@?%a@])" d_fkind fkind d_attrlistpost a
  | TBitfield(ikind,i,a) -> 
      dprintf "TBitfield(@[%a,@?%d,@?%a@])" d_ikind ikind i d_attrlistpost a
  | TNamed (n, t, a) ->
      dprintf "TNamed(@[%s,@?%a,@?%a@])" n d_plaintype t d_attrlistpost a
  | TForward(comp, a) -> 
      dprintf "TForward(%s %s, _, %a)" 
        (if comp.cstruct then "struct" else "union") comp.cname 
        d_attrlistpost comp.cattr
  | TPtr(t, a) -> dprintf "TPtr(@[%a,@?%a@])" d_plaintype t d_attrlistpost a
  | TArray(t,l,a) -> 
      let dl = match l with 
        None -> text "None" | Some l -> dprintf "Some(@[%a@])" d_plainexp l in
      dprintf "TArray(@[%a,@?%a,@?%a@])" 
        d_plaintype t insert dl d_attrlistpost a
  | TEnum(n,_,a) -> dprintf "Enum(%s,@[%a@])" n d_attrlistpost a
  | TFun(tr,args,isva,a) -> 
      dprintf "TFun(@[%a,@?%a%s,@?%a@])"
        d_plaintype tr 
        (docList (chr ',' ++ break) 
           (fun a -> dprintf "%s: %a" a.vname d_plaintype a.vtype)) args
        (if isva then "..." else "") d_attrlistpost a
  | TComp comp -> 
      dprintf "TComp(@[%s %s,@?%a,@?%a@])" 
        (if comp.cstruct then "struct" else "union") comp.cname
        (docList (chr ',' ++ break) 
           (fun f -> dprintf "%s : %a" f.fname d_plaintype f.ftype)) 
        comp.cfields
        d_attrlistpost comp.cattr



   (* Make a local variable and add it to a function *)
let makeLocalVar fdec name typ =
  fdec.smaxid <- 1 + fdec.smaxid;
  let vi = { vname = name;
             vid   = fdec.smaxid;
             vglob = false;
             vtype = typ;
             vdecl = lu;
             vattr = [];
             vstorage = NoStorage;
             vaddrof = false;
             vreferenced = false;    (* sm *)
           }  in
  fdec.slocals <- fdec.slocals @ [vi];
  vi

let makeTempVar fdec ?(name = "tmp") typ =
  let name = name ^ (string_of_int (1 + fdec.smaxid)) in
  makeLocalVar fdec name typ


   (* Make a global variable. Your responsibility to make sure that the name
    * is unique *)
let makeGlobalVar name typ =
  let vi = { vname = name;
             vid   = H.hash name;
             vglob = true;
             vtype = typ;
             vdecl = lu;
             vattr = [];
             vstorage = NoStorage;
             vaddrof = false;
             vreferenced = false;    (* sm *)
           }  in
  vi


   (* Make an empty function *)
let emptyFunction name = 
  { svar  = makeGlobalVar name (TFun(voidType, [], false,[]));
    smaxid = 0;
    slocals = [];
    sformals = [];
    sbody = Skip;
  } 


  (* Set the formals and make sure the function type shares them *)
let setFormals (f: fundec) (forms: varinfo list) = 
  f.sformals <- forms;
  match unrollType f.svar.vtype with
    TFun(rt, _, isva, fa) -> 
      f.svar.vtype <- TFun(rt, forms, isva, fa)
  | _ -> E.s (E.bug "Set formals. %s does not have function type\n"
                f.svar.vname)
      

    (* A dummy function declaration handy for initialization *)
let dummyFunDec = emptyFunction "@dummy"
let dummyFile = 
  { globals = [];
    fileName = "<dummy>";
    globinit = None}


let makeValidSymbolName (s: string) = 
  let s = String.copy s in (* So that we can update in place *)
  let l = String.length s in
  for i = 0 to l - 1 do
    let c = String.get s i in
    let isinvalid = 
      match c with
        '-' -> true
      | _ -> false
    in
    if isinvalid then 
      String.set s i '_';
  done;
  s
  
let getGlobInit (fl: file) = 
  match fl.globinit with 
    Some f -> f
  | None -> begin
      let f = emptyFunction 
          (makeValidSymbolName ("__globinit_" ^ 
                                (Filename.chop_extension
                                   (Filename.basename fl.fileName))))
      in
      ignore (E.warn "Added global initializer %s" f.svar.vname);
      fl.globinit <- Some f;
      f
  end
  

(* Iterate over all globals, including the global initializer *)
let iterGlobals (fl: file)
                (doone: global -> unit) : unit =
  List.iter doone fl.globals;
  (match fl.globinit with
    None -> ()
  | Some g -> doone (GFun(g, locUnknown)))

(* Fold over all globals, including the global initializer *)
let foldGlobals (fl: file) 
                (doone: 'a -> global -> 'a) 
                (acc: 'a) : 'a = 
  let acc' = List.fold_left doone acc fl.globals in
  (match fl.globinit with
    None -> acc'
  | Some g -> doone acc' (GFun(g, locUnknown)))


(* Fold over all globals, including the global initializer *)
let mapGlobals (fl: file) 
               (doone: global -> global) : unit = 
  fl.globals <- List.map doone fl.globals;
  (match fl.globinit with
    None -> ()
  | Some g -> begin
      match doone (GFun(g, locUnknown)) with
        GFun(g', _) -> fl.globinit <- Some g'
      | _ -> E.s (E.bug "mapGlobals: globinit is not a function")
  end)


let printFile (out : out_channel) file = 
  printDepth := 99999;  (* We don't want ... in the output *)
  (* If we are in RELEASE mode then we do not print indentation *)
  printIndent := false;
  assert (printIndent := true; true);
  let print x = fprint out 80 x in
  print (text "/* Generated by safecc */\n\n");
  H.clear definedTypes;
  noRedefinitions := true;
  let d_global () = function
      GFun (fundec, _) -> d_fun_decl () fundec ++ line
    | GType (str, typ, _) -> 
        if str = "" then
          dprintf "%a;@!" (d_decl (fun _ -> nil)) typ
        else 
          dprintf "typedef %a;@!" (d_decl (fun _ -> text str)) typ
            
    | GVar (vi, eo, _) -> dprintf "%a %a;"
          d_videcl vi 
          insert (match eo with None -> nil | Some e -> 
            dprintf " = %a" d_exp e)
    | GDecl (vi, _) -> 
        dprintf "%a;" d_videcl vi
          
    | GAsm (s, _) -> dprintf "__asm__(\"%s\");@!" (escape_string s)
    | GPragma (a, _) -> dprintf "#pragma %a@!" d_attr a
    | GText s  -> text s
  in
  iterGlobals file (fun g -> print (d_global () g ++ line));
  noRedefinitions := false;
  H.clear definedTypes

    
let printFileWithCustom (out: out_channel) 
                        (custom: attribute -> doc option) 
                        (f: file) = 
  let oldCustom = !d_attrcustom in
  let newCustom a = 
    match custom a with
      None -> oldCustom a
    | x -> x
  in
  d_attrcustom := newCustom;
  printFile out f;
  d_attrcustom := oldCustom


(******************
 ******************
 ******************)


(*** Define the visiting engine ****)
(* visit all the nodes in a Cil expression *)
let rec visitCilExpr (vis : cilVisitor) (e : exp) : unit =
begin
  (* visit the expression itself *)
  if (vis#vexpr e) then

  (* and visit its subexpressions *)
  let fExp e = (visitCilExpr vis e) in
  match e with
    (Const _|SizeOf _) -> ()
  | SizeOfE e -> fExp e
  | Lval lv -> (visitCilLval vis lv)
  | UnOp(_,e,_) -> fExp e
  | BinOp(_,e1,e2,_) -> fExp e1; fExp e2
  | Question (e1, e2, e3) -> fExp e1; fExp e2; fExp e3
  | CastE(_, e) -> fExp e
  | Compound (_, initl) ->
      List.iter
        (function
            (None, e) -> fExp e
          | (Some ofs, e) -> (visitCilOffset vis ofs); fExp e
        )
        initl
  | AddrOf (lv) -> (visitCilLval vis lv)
  | StartOf (lv) -> (visitCilLval vis lv)
end

and visitCilLval (vis: cilVisitor) (lv: lval) : unit =
begin
  if (vis#vlval lv) then

  match lv with
    Var v, off -> (
      (ignore (vis#vvrbl v));
      (visitCilOffset vis off)
    )
  | Mem e, off -> (
      (visitCilExpr vis e);
      (visitCilOffset vis off)
    )
end

and visitCilOffset (vis: cilVisitor) (off: offset) : unit =
begin
  if (vis#voffs off) then

  match off with
    Field (_, o) -> (visitCilOffset vis o)
  | Index (e, o) -> (visitCilExpr vis e); (visitCilOffset vis o)
  | NoOffset -> ()
end

and visitCilInstr (vis: cilVisitor) (i: instr) : unit =
begin
  if (vis#vinst i) then

  let fExp = visitCilExpr vis in
  let fLval = visitCilLval vis in

  match i with
  | Set(lv,e) -> fLval lv; fExp e
  | Call(None,f,args) -> fExp f; (List.iter fExp args)
  | Call((Some (v, _)),fn,args) -> (
      (ignore (vis#vvrbl v));
      (fExp fn);
      (List.iter fExp args)
    )
  | Asm(_,_,outs,ins,_) -> begin
      (List.iter (fun (_, lv) -> fLval lv) outs);
      (List.iter (fun (_, e) -> fExp e) ins)
    end
end

(* visit all nodes in a Cil statement tree in preorder *)
and visitCilStmt (vis: cilVisitor) (body: stmt) : unit =
begin
  let rec fExp e = (visitCilExpr vis e)
  and fLval lv = (visitCilLval vis lv)
  and fOff o = (visitCilOffset vis o)
  and fInst i = (visitCilInstr vis i)   (* compiler doesn't like this curried..? *)

  and fStmt s = if (vis#vstmt s) then fStmt' s
  and fStmt' = begin function
      (Skip|Break|Continue|Label _|Goto _|Case _|Default|Return (None,_)) -> ()
    | Sequence s -> List.iter fStmt s
    | Loop s -> fStmt s
    | IfThenElse (e, s1, s2, _) -> fExp e; fStmt s1; fStmt s2
    | Return(Some e, _) -> fExp e
    | Switch (e, s, _) -> fExp e; fStmt s
    | Instr(i, _) -> fInst i
  end

  in
  fStmt body
end

and visitCilType (vis : cilVisitor) (t : typ) : unit =
begin
  (*(trace "visitCilType" (dprintf "%a\n" d_type t));*)

  (* visit 't' itself *)
  if (vis#vtype t) then

  (* look for types referred to inside t's definition *)
  match t with
    TPtr(t, _) -> (visitCilType vis t)
  | TArray(t, None, _) -> (visitCilType vis t)
  | TArray(t, Some e, _) -> (
      (visitCilType vis t);
      (visitCilExpr vis e)
    )
  | TComp(cinfo) -> (
      (* iterate over fields *)
      (List.iter
        (fun (finfo : fieldinfo) ->
          (visitCilType vis finfo.ftype))
        cinfo.cfields)
    )
  | TFun(rettype, args, _, _) -> (
      (visitCilType vis rettype);

      (* iterate over formals *)
      (List.iter
        (fun (v : varinfo) ->
          (visitCilVarDecl vis v)      (* visit as a variable decl *)
        )
        args
      )
    )
  (* I choose not to recurse into TNamed since my present *)
  (* purpose doesn't need it, and it could lead to inf loop *)
  | _ -> ()
end

(* for declarations, we visit the types inside; but for uses, *)
(* we just visit the varinfo node *)
and visitCilVarDecl (vis : cilVisitor) (v : varinfo) : unit =
begin
  (* visit the variable as a decl *)
  if (vis#vvdec v) then

  (* visit the type it's declared as *)
  (visitCilType vis v.vtype)
end

let visitCilFunction (vis : cilVisitor) (f : fundec) : unit =
begin
  if (vis#vfunc f) then (            (* preorder visit *)
    (visitCilVarDecl vis f.svar);      (* hit the function name *)
    (List.iter
      (fun (v : varinfo) ->
        (visitCilVarDecl vis v))       (* visit local declarations *)
      f.slocals);
    (visitCilStmt vis f.sbody);        (* visit the body *)
    (ignore (vis#vfuncPost f))         (* postorder visit *)
  )
end

let visitCilGlobal (vis: cilVisitor) (g: global) : unit =
begin
  if (vis#vglob g) then

  match g with
  | GFun (f, _) -> (visitCilFunction vis f)
  | GType(s, t, _) -> (
      (*(trace "visitTypedef" (dprintf "%s = %a\n" s d_type t));*)
      if (vis#vtdec s t) then (visitCilType vis t)
    )
  | GDecl(v, _) -> (visitCilVarDecl vis v)
  | GVar (v, None, _) -> (ignore (vis#vvrbl v))
  | GVar (v, Some e, _) -> (
      (ignore (vis#vvrbl v));
      (ignore (vis#vexpr e))
    )
  | _ -> ()
end

let visitCilFile (vis : cilVisitor) (f : file) : unit =
begin
  (trace "visitCilFile" (dprintf "%s\n" f.fileName));

  let fGlob g = (visitCilGlobal vis g) in

  (* primary list of globals *)
  (List.iter fGlob f.globals);

  (* the global initializer *)
  (match f.globinit with
    None -> ()
  | Some g -> (fGlob (GFun(g, locUnknown))))
end

(* sm: I didn't end up using this (because I needed more control *)
(* over the iteration process than the visitor provides), but I *)
(* leave it here anyway *)
let visitCilFileInReverse (vis : cilVisitor) (f : file) : unit =
begin
  (trace "visitCilFileInReverse" (dprintf "%s\n" f.fileName));

  let fGlob g = (visitCilGlobal vis g) in

  (* first the global initializer *)
  (match f.globinit with
    None -> ()
  | Some g -> (fGlob (GFun(g, locUnknown))));

  (* then the primary list of globals, reversed *)
  (List.iter fGlob (List.rev f.globals))
end


(******************** OPTIMIZATIONS *****)
let peepHole1     (* Process one statement and possibly replace it *)
                    (doone: stmt -> stmt list option)
                    (ss: stmt list) : stmt list = 
  let rec loop = function
      [] -> []
    | Sequence sl :: rest -> loop (sl @ rest)
    | s :: rest -> begin
        match doone s with
          None -> s :: loop rest
        | Some sl -> loop (sl @ rest)
    end
  in
  loop ss

let peepHole2     (* Process two statements and possibly replace them both *)
                    (dotwo: stmt * stmt -> stmt list option)
                    (ss: stmt list) : stmt list = 
  let rec loop = function
      [] -> []
    | Sequence sl :: rest -> loop (sl @ rest)
    | [s] -> [s]
    | s1 :: ((s2 :: rest) as rest2) -> begin
        match dotwo (s1,s2) with
          None -> s1 :: loop rest2
          | Some sl -> loop (sl @ rest)
    end
  in
  loop ss



(**** Compute the type of an expression ****)
let rec typeOf (e: exp) : typ = 
  match e with
    Const(CInt (_, ik, _)) -> TInt(ik, [])
  | Const(CChr _) -> charType
  | Const(CStr _) -> charPtrType 
  | Const(CReal (_, fk, _)) -> TFloat(fk, [])
  | Lval(lv) -> typeOfLval lv
  | SizeOf _ | SizeOfE _ -> uintType
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | Question (_, e2, _) -> typeOf e2
  | CastE (t, _) -> t
  | Compound (t, _) -> t
  | AddrOf (lv) -> TPtr(typeOfLval lv, [])
  | StartOf (lv) -> begin
      match typeOfLval lv with
        TArray (t,_, _) -> TPtr(t, [])
      | TFun _ as t -> TPtr(t, [])
     | _ -> E.s (E.bug "typeOf: StartOf on a non-array or non-function")
  end
      
and typeOfLval = function
    Var vi, off -> typeOffset vi.vtype off
  | Mem addr, off -> begin
      match unrollType (typeOf addr) with
        TPtr (t, _) -> typeOffset t off
      | _ -> E.s (E.bug "typeOfLval: Mem on a non-pointer")
  end

and typeOffset basetyp = function
    NoOffset -> basetyp
  | Index (_, o) -> begin
      match unrollType basetyp with
        TArray (t, _, _) -> typeOffset t o
      | t -> E.s (E.bug "typeOffset: Index on a non-array: %a" d_plaintype t)
  end 
  | Field (fi, o) -> typeOffset fi.ftype o



let dExp : doc -> exp = 
  function d -> Const(CStr(sprint 80 d))

let dStmt : doc -> stmt = 
  function d -> Instr(Asm([sprint 80 d], false, [], [], []), lu)


let rec addOffset toadd (off: offset) : offset =
  match off with
    NoOffset -> toadd
  | Field(fid', offset) -> Field(fid', addOffset toadd offset)
  | Index(e, offset) -> Index(e, addOffset toadd offset)

 (* Add an offset at the end of an lv *)      
let addOffsetLval toadd (b, off) : lval =
 b, addOffset toadd off



  (* Make a Mem, while optimizing StartOf. The type of the addr must be 
   * TPtr(t) and the type of the resulting expression is t *)
let mkMem (addr: exp) (off: offset) : exp =  
  let isarray = (* Maybe the addr is the start of an array *)
    match addr with 
      StartOf(lv) when 
      (match unrollType (typeOfLval lv) with TArray _ -> true | _ -> false)
        -> Some lv
    | _ -> None
  in
  let res = 
    match isarray, off with
      Some lv, Index _ -> (* index on an array *)
        Lval(addOffsetLval off lv)
    | Some lv, _ -> (* non-index on an array *)
        Lval(addOffsetLval (Index(zero, off)) lv)
    | None, Index(ei, resto) -> (* index on a non-array *)
        Lval(Mem (BinOp(IndexPI, addr, ei, typeOf addr)), resto) 
    | None, _ -> (* non-index on a non-array *)
        Lval(Mem addr, off)
  in
(*  ignore (E.log "memof : %a:%a\nresult = %a\n" 
            d_plainexp addr d_plainoffset off d_plainexp res); *)
  res
          


let mkAddrOf ((b, off) as lval) : exp = 
  match unrollType (typeOfLval lval) with
    TArray _ -> StartOf lval
  | TFun _ -> StartOf lval
  | _ -> begin
      (match lval with
        Var vi, off when vi.vstorage = Register -> vi.vstorage <- NoStorage
      | _ -> ());
      AddrOf(lval)
  end

let isIntegralType t = 
  match unrollType t with
    (TInt _ | TEnum _ | TBitfield _) -> true
  | _ -> false

let isArithmeticType t = 
  match unrollType t with
    (TInt _ | TEnum _ | TBitfield _ | TFloat _) -> true
  | _ -> false
    

let isPointerType t = 
  match unrollType t with
    TPtr _ -> true
  | _ -> false

let isFunctionType t = 
  match unrollType t with
    TFun _ -> true
  | _ -> false


let rec typeAttrs = function
    TVoid a -> a
  | TInt (_, a) -> a
  | TFloat (_, a) -> a
  | TBitfield (_, _, a) -> a
  | TNamed (n, t, a) -> addAttributes a (typeAttrs t)
  | TPtr (_, a) -> a
  | TArray (_, _, a) -> a
  | TComp comp -> comp.cattr
  | TForward (comp, a) -> addAttributes a (typeAttrs (TComp comp))
  | TEnum (_, _, a) -> a
  | TFun (_, _, _, a) -> a


let setTypeAttrs t a =
  match t with
    TVoid _ -> TVoid a
  | TInt (i, _) -> TInt (i, a)
  | TFloat (f, _) -> TFloat (f, a)
  | TBitfield (i, s, _) -> TBitfield (i, s, a)
  | TNamed (n, t, _) -> TNamed(n, t, a)
  | TPtr (t', _) -> TPtr(t', a)
  | TArray (t', l, _) -> TArray(t', l, a)
  | TComp comp -> comp.cattr <- a; t
  | TForward (comp, _) -> TForward (comp, a)
  | TEnum (n, f, _) -> TEnum (n, f, a)
  | TFun (r, args, v, _) -> TFun(r,args,v,a)


let typeAddAttributes a0 t = 
  if a0 == [] then t else
  let add a = addAttributes a0 a in
  match t with 
    TVoid a -> TVoid (add a)
  | TInt (ik, a) -> TInt (ik, add a)
  | TFloat (fk, a) -> TFloat (fk, add a)
  | TBitfield (i, s, a) -> TBitfield (i, s, add a)
  | TEnum (n, t, a) -> TEnum (n, t, add a)
  | TPtr (t, a) -> TPtr (t, add a)
  | TArray (t, l, a) -> TArray (t, l, add a)
  | TFun (t, args, isva, a) -> TFun(t, args, isva, add a)
  | TComp comp -> comp.cattr <- add comp.cattr ; t
  | TForward (comp, a) -> TForward (comp, add a)
  | TNamed (n, t, a) -> TNamed (n, t, add a)

let typeRemoveAttributes (a0: attribute list) t = 
  let drop (al: attribute list) = dropAttributes a0 al in
  match t with 
    TVoid a -> TVoid (drop a)
  | TInt (ik, a) -> TInt (ik, drop a)
  | TFloat (fk, a) -> TFloat (fk, drop a)
  | TBitfield (i, s, a) -> TBitfield (i, s, drop a)
  | TEnum (n, t, a) -> TEnum (n, t, drop a)
  | TPtr (t, a) -> TPtr (t, drop a)
  | TArray (t, l, a) -> TArray (t, l, drop a)
  | TFun (t, args, isva, a) -> TFun(t, args, isva, drop a)
  | TComp comp -> comp.cattr <- drop comp.cattr ; t
  | TForward (comp, a) -> TForward (comp, drop a)
  | TNamed (n, t, a) -> TNamed (n, t, drop a)

     (* Type signatures. Two types are identical iff they have identical 
      * signatures *)
type typsig = 
    TSArray of typsig * exp option * attribute list
  | TSPtr of typsig * attribute list
  | TSComp of bool * string * attribute list
  | TSFun of typsig * (typsig * attribute list) list * bool * attribute list
  | TSBase of typ

(* Compute a type signature *)
let rec typeSigAttrs doattr t = 
  let typeSig = typeSigAttrs doattr in
  match t with 
  | (TInt _ | TFloat _ | TEnum _ | TBitfield _ | TVoid _) -> TSBase t
  | TPtr (t, a) -> TSPtr (typeSig t, doattr a)
  | TArray (t,l,a) -> TSArray(typeSig t, l, doattr a)
  | TComp comp -> TSComp (comp.cstruct, comp.cname, doattr comp.cattr)
  | TFun(rt,args,isva,a) -> TSFun(typeSig rt, 
                                  List.map (fun vi -> (typeSig vi.vtype, 
                                                       doattr vi.vattr)) args,
                                  isva, doattr a)
  | TNamed(_, t, a) -> typeSigAddAttrs (doattr a) (typeSig t)

  | TForward (comp, a) -> typeSigAddAttrs (doattr a) (typeSig (TComp comp))
      
and typeSigAddAttrs a0 t = 
  if a0 == [] then t else
  match t with 
    TSBase t -> TSBase (typeAddAttributes a0 t)
  | TSPtr (ts, a) -> TSPtr (ts, addAttributes a0 a)
  | TSArray (ts, l, a) -> TSArray(ts, l, addAttributes a0 a)
  | TSComp (iss, n, a) -> TSComp (iss, n, addAttributes a0 a)
  | TSFun(ts, tsargs, isva, a) -> TSFun(ts, tsargs, isva, addAttributes a0 a)


let typeSig t = typeSigAttrs (fun al -> al) t


let rec doCastT (e: exp) (oldt: typ) (newt: typ) = 
  (* Do not remove old casts because they are conversions !!! *)
  if typeSig oldt = typeSig newt then
    e
  else
    (* If the new type is a Bitfield then cast to the base type *)
    match newt with
      TBitfield (ik, _, a) -> doCastT e oldt (TInt(ik, a))
    | _ -> CastE(newt,e)

let doCast (e: exp) (newt: typ) = 
  doCastT e (typeOf e) newt

type existsAction = 
    ExistsTrue                          (* We have found it *)
  | ExistsFalse                         (* Stop processing this branch *)
  | ExistsMaybe                         (* This node is not what we are 
                                         * looking for but maybe its 
                                         * successors are *)
let existsType (f: typ -> existsAction) (t: typ) : bool = 
  let memo : (int, unit) H.t = H.create 17 in  (* Memo table *)
  let rec loop t = 
    match f t with 
      ExistsTrue -> true
    | ExistsFalse -> false
    | ExistsMaybe -> 
        (match t with 
          TNamed (_, t', _) -> loop t'
        | TForward (c, a) -> loopComp c
        | TComp c -> loopComp c
        | TArray (t', _, _) -> loop t'
        | TPtr (t', _) -> loop t'
        | TFun (rt, args, _, _) -> 
            (loop rt || List.exists (fun a -> loop a.vtype) args)
        | _ -> false)
  and loopComp c = 
    try
      H.find memo c.ckey; 
      (* We are looping, the answer must be false *)
      false
    with Not_found -> 
      H.add memo c.ckey ();
      List.exists (fun f -> loop f.ftype) c.cfields
  in
  loop t
          
(* Try to do an increment *)
let increm (e: exp) (i: int) =
  if i = 0 then e
  else 
    let rec tryIncrem e sign = 
      match e with
        Const(CInt(ei, eik, _)) -> 
          Some (Const(CInt(ei + i * sign, eik, None)))
      | BinOp(((PlusA|MinusA|PlusPI|IndexPI|MinusPI) as bop), 
              e1, e2, t) -> begin
          let newe2sign = 
            if bop = MinusA || bop = MinusPI then - sign else sign
          in
          match tryIncrem e2 newe2sign with
            None -> begin
              match tryIncrem e1 sign with
                None -> None
              | Some e1' -> Some (BinOp(bop, e1', e2, t))
            end
          | Some e2' -> Some (BinOp(bop, e1, e2', t))
      end
      | UnOp(Neg, e', t) -> begin
          match tryIncrem e' (- sign) with
            None -> None
          | Some e'' -> Some (UnOp(Neg, e'', t))
      end
      | _ -> None
    in
    match tryIncrem e 1 with
      None -> 
        let et = typeOf e in
        let bop = if isPointerType et then PlusPI else PlusA in
        BinOp(bop, e, integer i, et)
    | Some e' -> e'
  

  

(*** Make a compound initializer for zeroe-ing a data type ***)
let rec makeZeroCompoundInit t = 
  match unrollType t with
    TInt (ik, _) -> Const(CInt(0, ik, None))
  | TFloat(fk, _) -> Const(CReal(0.0, fk, None))
  | (TEnum _ | TBitfield _) -> zero
  | TComp comp as t' when comp.cstruct -> 
      Compound (t', 
                List.map (fun f -> None, makeZeroCompoundInit f.ftype) 
                  comp.cfields)
  | TArray(bt, Some (Const(CInt(n, _, _))), _) as t' -> 
      let initbt = makeZeroCompoundInit bt in
      let rec loopElems acc i = 
        if i >= n then acc
        else loopElems ((None, initbt) :: acc) (i + 1) 
      in
      Compound(t', loopElems [] 0)
  | TPtr _ as t -> CastE(t, zero)
  | _ -> E.s (E.unimp "makeZeroCompoundInit: %a" d_plaintype t)


(**** Fold over the list of initializers in a Compound ****)
let foldLeftCompound (doexp: offset -> exp -> typ -> 'a -> 'a)
    (ct: typ) 
    (initl: (offset option * exp) list)
    (acc: 'a) : 'a = 
  match unrollType ct with
    TArray(bt, _, _) -> 
      let rec foldArray  
          (nextidx: exp) 
          (initl: (offset option * exp) list)
          (acc: 'a) : 'a  =
        let incrementIdx = function
            Const(CInt(n, ik, _)) -> Const(CInt(n + 1, ik, None))
          | e -> BinOp(PlusA, e, one, intType)
        in
        match initl with
          [] -> acc
        | (io, ie) :: restinitl ->
            let nextidx', thisoff, thisexpt = 
              match io with
                None -> 
                  incrementIdx nextidx, Index(nextidx, NoOffset), bt
              | Some (Index(idxe, restof) as off) -> 
                  let t = typeOffset bt restof in
                  incrementIdx idxe, off, t
              | _ -> E.s (E.unimp "Unexpected offset in array initializer")
            in
            (* Now do the initializer expression *)
            let acc' = doexp thisoff ie thisexpt acc in
            foldArray nextidx' restinitl acc'
      in
      foldArray zero initl acc

  | TComp comp when comp.cstruct ->
      let rec foldFields 
          (allflds: fieldinfo list) 
          (nextflds: fieldinfo list) 
          (initl: (offset option * exp) list)
          (acc: 'a) : 'a = 
        match initl with 
          [] -> acc   (* We are done *)
        | (io, ie) :: restinitl ->
            let nextfields, thisoff, thisexpt = 
              match io with
                None -> begin
                  match nextflds with
                    [] -> E.s (E.unimp "Too many initializers")
                  | x :: xs -> xs, Field(x, NoOffset), x.ftype
                end
              | Some (Field(fld, restof) as off) -> 
                  let rec findField = function
                      [] -> E.s 
                          (E.unimp "Cannot find designated field %s" fld.fname)
                    | f :: restf when f.fname = fld.fname -> 
                        let t = typeOffset fld.ftype restof in
                        restf, off, t
                    | _ :: restf -> findField restf
                  in
                  findField allflds
              | _ -> E.s (E.unimp "Unexpected offset in structure initializer")
            in
            (* Now do the initializer expression *)
            let acc' = doexp thisoff ie thisexpt acc in
            foldFields allflds nextfields restinitl acc'
      in
      foldFields comp.cfields comp.cfields initl acc
  | _ -> E.s (E.unimp "Type of Compound is not array or struct")



let rec isCompleteType t =
  match unrollType t with
  | TArray(t, None, _) -> false
  | TComp comp -> (* Struct or union *)
      List.for_all (fun fi -> isCompleteType fi.ftype) comp.cfields
  | _ -> true


(* ------------- removing unused variables/types ----------- *)
(* simple visitor to clear the 'referenced' bits *)
class clearRefBitsVis = object
  inherit nopCilVisitor

  method vvdec (v: varinfo) = begin
    (* declared variables: clear the 'referenced' bits *)
    (* assume declaration preceed all uses *)
    v.vreferenced <- false;
    true
  end
end


(* encapsulate this since rumor has it Raymond has changed *)
(* how we represent 'inline' *)
let isInlineFunc (f: fundec) : bool = (
  (hasAttribute "inline" f.svar.vattr)
)


(* This visitor recursively marks all reachable types and variables as used. *)
(* You construct it with a hash table, which is already partially *)
(* marked; this visitor destructively updates the hash table. *)
class removeTempsVis (usedTypes : (typ,bool) H.t)
                     (usedTypedefs : (string,bool) H.t) = object (self)
  inherit nopCilVisitor

  method vvrbl (v : varinfo) = begin
    if (not (v.vglob)) then (
      (trace "usedLocal" (dprintf "local var ref: %s\n" v.vname))
    )
    else (
      (trace "usedVar" (dprintf "global var ref: %s\n" v.vname))
    );
    v.vreferenced <- true;
    true
  end

  method vtype (t : typ) = begin
    match t with
    | TNamed(s, t, _) -> (
        (* see if this typedef name has already been marked *)
        if (H.mem usedTypedefs s) then (
          (* already marked, don't recurse further *)
          true
        )
        else (
          (trace "usedType" (dprintf "marking used typedef: %s\n" s));

          (* not already marked; first mark the typedef name *)
          (H.add usedTypedefs s true);

          (* also recursively mark the type it refers to; *)
          (* even if we return true, the visitor does not *)
          (* automatically recurse since named types are the *)
          (* cut-points for circularity *)
          (visitCilType (self :> cilVisitor) t);

          (* like I say, this has no effect *)
          true
        )
      )

    | _ -> (
        (* for anything but a typedef, look only at its structure *)
        if (H.mem usedTypes t) then (
          (* this type has already been marked reachable, so *)
          (* stop recursing into it *)
          true
        )
        else (
          (trace "usedType" (dprintf "marking used type: %a\n"
                                     d_type t));

          (* it is reachable; mark it as such *)
          (H.add usedTypes t true);

          (* then recurse into types reachable from here *)
          true
        )
      )
  end
  
  method vfuncPost (f : fundec) = begin
    (* check the 'referenced' bits on the locals *)
    f.slocals <- (List.filter
      (fun (v : varinfo) ->
        if (not v.vreferenced) then begin
          (trace "usedLocal" (dprintf "removing unused: var decl: %s\n" v.vname));
          if ((String.length v.vname) < 3 ||
              (String.sub v.vname 0 3) <> "tmp") then
            (* sm: if I'd had this to begin with, it would have been
             * a little easier to track down the bug where I didn't
             * check the function return-value destination *)
            (ignore (printf "WARNING: removing unused source variable %s\n"
                            v.vname));
          false   (* remove it *)
        end
        else
          true    (* keep it *)
      )
      f.slocals);

    true
  end
end

(* this traces which variables are used, from the set    *)
(* of root declarations, which are:                      *)
(*   - non-inline function definitions                   *)
(*   - non-extern global variable declarations           *)
(*   - inline funcs & extern globals that are referenced *)
(* it only works if we visit toplevel decls in reverse order *)
let removeUnusedTemps (file : file) =
begin
  (* associate with every 'typ' whether it is referred-to *)
  (* by a global variable or function *)
  let usedTypes : (typ, bool) H.t = H.create 17 in

  (* and similarly for every typedef name *)
  let usedTypedefs : (string, bool) H.t = H.create 17 in

  (* begin by clearing all the 'referenced' bits *)
  (visitCilFile (new clearRefBitsVis) file);

  (* create the visitor object *)
  let vis = (new removeTempsVis usedTypes usedTypedefs) in

  (* this was here before, and it might still be useful later *)
  (*
    iterGlobals file
      (function
          GDecl (v, _) -> ignore (E.log " %s referenced=%b\n"
                                    v.vname v.vreferenced)
        | _ -> ());
  *)

  (* iterate over the list of globals in reverse, marking things *)
  (* as reachable if they are reachable from the roots, and *)
  (* removing ultimately unreachable toplevel constructs *)
  let rec revLoop (lst : global list) : global list =
    match lst with
    | hd::tl -> (
        (* process the tail first; going backwards is the key *)
        (* to keeping this dependency analysis simple, since it *)
        (* means we will always process all of the uses of a *)
        (* variable or type, before we finally see the declaration *)
        let processedTail = (revLoop tl) in

        (* the 'trace' calls below are actually quite inexpensive *)
        (* (when the corresponding flag is off), because they are only *)
        (* encountered at once per toplevel construct; so, I leave *)
        (* them uncommented *)

        (* now examine the head *)
        let retainHead = match hd with
          | GFun(f,_) -> (
              if (f.svar.vreferenced ||
                  (not (isInlineFunc f))) then (
                (trace "usedVar" (dprintf "keeping func: %s\n" f.svar.vname));
                (visitCilFunction vis f);    (* root: trace it *)
                true
              )
              else (
                (* this will be deleted; don't trace *)
                (trace "usedVar" (dprintf "removing func: %s\n" f.svar.vname));
                false
              )
            )

          | GType(s, t, _) -> (
              if (s = "") then (
                (* it's a normal type declaration *)
                if (H.mem usedTypes t) then (
                  (trace "usedType" (dprintf "keeping type %a\n"
                                             d_type t));

                  (* also trace from here *)
                  (visitCilType vis t);

                  (* and retain this type definition *)
                  true
                )
                else (
                  (* not used, remove it *)
                  (trace "usedType" (dprintf "removing type %a\n"
                                             d_type t));
                  false
                )
              )
              else (
                (* this is a typedef *)
                if (H.mem usedTypedefs s) then (
                  (trace "usedType" (dprintf "keeping typedef %s\n" s));
                  (visitCilType vis t);           (* root; trace it *)
                  true                            (* used; keep it *)
                )
                else (
                  (* not used, remove it *)
                  (trace "usedType" (dprintf "removing typedef %s\n" s));
                  false
                )
              )
            )

          | GDecl(v, _) -> (
              if (not v.vreferenced) then (
                (trace "usedVar" (dprintf "removing global: %s\n" v.vname));
                false
              )
              else (
                (trace "usedVar" (dprintf "keeping global: %s\n" v.vname));

                (* since it's referenced, use it as a root for the type dependency *)
                (visitCilVarDecl vis v);

                (* it's referenced: keep it *)
                true
              )
            )

          (* something else: keep it *)
          | _ -> (
              (visitCilGlobal vis hd);
              true
            )
        in

        if (retainHead) then
          hd :: processedTail
        else
          processedTail
      )

    | [] -> []
  in

  file.globals <- (revLoop file.globals)
end


(**
 **
 ** MACHINE DEPENDENT PART
 **
 **)

     
type offsetAcc = 
    { oaFirstFree: int;   (* The first free bit *)
      oaLastFieldStart: int;   (* Where the previous field started *)
      oaLastFieldWidth: int;   (* The width of the previous field. Might not 
                                * be same as FirstFree - FieldStart because 
                                * of internal padding *)
      oaPrevBitPack: (int * ikind * int) option; (* If the previous fields 
                                                   * were packed bitfields, 
                                                   * the bit where packing 
                                                   * has started, the ikind 
                                                   * of the bitfield and the 
                                                   * width of the ikind *)
    } 

let rec offsetOfFieldAcc (fi: fieldinfo) 
                         (sofar: offsetAcc) : offsetAcc = 
  (* field type *)
  let ftype = unrollType fi.ftype in
  match ftype, sofar.oaPrevBitPack with (* Check for a bitfield that fits in 
                                         * the current pack after some other 
                                         * bitfields  *)
    TBitfield(ikthis, wdthis, _), Some (packstart, ikprev, wdpack)
      when ((not !msvcMode || ikthis = ikprev) && 
            packstart + wdpack >= sofar.oaFirstFree + wdthis) ->
              { oaFirstFree = sofar.oaFirstFree + wdthis;
                oaLastFieldStart = sofar.oaFirstFree; 
                oaLastFieldWidth = wdthis;
                oaPrevBitPack = sofar.oaPrevBitPack
              } 

  | _, Some (packstart, _, wdpack) -> (* Finish up the bitfield pack and 
                                       * restart *)
      offsetOfFieldAcc fi
        { oaFirstFree = packstart + wdpack;
          oaLastFieldStart = sofar.oaLastFieldStart;
          oaLastFieldWidth = sofar.oaLastFieldWidth;
          oaPrevBitPack = None }
  | _ -> 
      (* no active bitfield pack. Compute the internalPadding. Returns the 
       * alignment boundary for internal padding for the current field  *)
      let rec internalPaddingAlign = function 
          TInt((IChar|ISChar|IUChar), _) -> 1
        | TInt((IShort|IUShort), _) -> 2
        | TInt((IInt|IUInt), _) -> 4
        | TInt((ILong|IULong), _) -> 4
        | TInt((ILongLong|IULongLong), _) -> 4  (* !!! is this correct *)
        | TEnum _ -> 4 (* !!! Is this correct? *)
        | TBitfield(ik, _, a) -> 
            internalPaddingAlign (TInt(ik, a)) (* Is this correct ? *)
        | TFloat(FFloat, _) -> 4
        | TFloat((FDouble|FLongDouble), _) -> 8
        | TNamed (_, t, _) -> internalPaddingAlign t
        | TForward (comp, _) -> internalPaddingAlign (TComp comp)
        | TComp _ -> 4 (* Is this correct ? *)
        | TArray _ -> 4 (* Is this correct ? *)
        | TPtr _ -> 4
        | (TVoid _ | TFun _) -> E.s (E.bug "internalPaddingAlign")
      in
      let internPad = (internalPaddingAlign ftype) lsl 3 in
      let newStart = 
        (sofar.oaFirstFree + internPad - 1) land (lnot (internPad - 1)) in
      (* ignore (E.log "firstFree = %d, internPad = %d, newStart = %d\n"
                sofar.oaFirstFree internPad newStart); *)
      (* Now compute the width of this field *)
      let mkRes thiswd btpack = 
        { oaFirstFree = newStart + thiswd;
          oaLastFieldStart = sofar.oaFirstFree;
          oaLastFieldWidth = thiswd;
          oaPrevBitPack = btpack }
      in
      match unrollType ftype with
        TBitfield(ik, wd, a) -> 
          let wdpack = bitsSizeOf (TInt(ik, a)) in
          { oaFirstFree = newStart + wd;
            oaLastFieldStart = newStart;
            oaLastFieldWidth = wd;
            oaPrevBitPack = Some (newStart, ik, wdpack); }
      | _ ->
          let wd = bitsSizeOf ftype in
          (* ignore (E.log "non-bitfield (%a): wd=%d\n"
                    d_type ftype wd); *)
          { oaFirstFree = newStart + wd;
            oaLastFieldStart = newStart;
            oaLastFieldWidth = wd;
            oaPrevBitPack = None;
          } 
        
(* The size of a type, in bits. If struct or array then trailing padding is 
 * added *)
and bitsSizeOf t = 
  match t with 
    TInt((IChar|ISChar|IUChar), _) -> 8
  | TInt((IShort|IUShort), _) -> 16
  | TInt((IInt|IUInt), _) -> 32
  | TInt((ILong|IULong), _) -> 32
  | TInt((ILongLong|IULongLong), _) -> 64
  | TEnum _ -> 32 (* !!! is this correct ? *)
  | TBitfield(ik, wd, a) -> wd
  | TFloat(FFloat, _) -> 32
  | TFloat((FDouble|FLongDouble), _) -> 64
  | TNamed (_, t, _) -> bitsSizeOf t
  | TForward (comp, _) -> bitsSizeOf (TComp comp)
  | TPtr _ -> 32
  | TComp comp when comp.cfields = [] -> raise Not_found (* abstract type *)
  | TComp comp when comp.cstruct -> (* Struct *)
        (* Go and get the last offset *)
      let startAcc = 
        { oaFirstFree = 0;
          oaLastFieldStart = 0;
          oaLastFieldWidth = 0;
          oaPrevBitPack = None;
        } in
      let lastoff = 
        List.fold_left (fun acc fi -> offsetOfFieldAcc fi acc) 
          startAcc comp.cfields 
      in
      addTrailing lastoff.oaFirstFree
        
  | TComp comp -> (* when not comp.cstruct *)
        (* Get the maximum of all fields *)
      let startAcc = 
        { oaFirstFree = 0;
          oaLastFieldStart = 0;
          oaLastFieldWidth = 0;
          oaPrevBitPack = None;
        } in
      let max = 
        List.fold_left (fun acc fi -> 
          let lastoff = offsetOfFieldAcc fi startAcc in
          if lastoff.oaFirstFree > acc then
            lastoff.oaFirstFree else acc) 0 comp.cfields in
        (* Add trailing by simulating adding an extra field *)
      addTrailing max

  | TArray(t, Some (Const(CInt(l,_,_))),_) -> 
      addTrailing ((bitsSizeOf t) * l)

  | TArray(t, None, _) -> raise Not_found
        
  | TArray _ -> 
      E.s (E.unimp "sizeOfInt for non-constant length array:@!%a" d_type t)
	| TFun _ -> 32
  | TVoid _ -> E.s (E.bug "bitsSizeOf void")


and addTrailing nrbits = 
    let roundto = 32 in
    (nrbits + roundto - 1) land (lnot (roundto - 1))

and sizeOf t = 
    match unrollType t with
      TBitfield _ -> E.s (E.bug "sizeOf(bitfield) not allowed")
    | t' -> begin
        try
          integer ((bitsSizeOf t') lsr 3)
        with Not_found -> SizeOf(t')
    end
            

 

let offsetOf (fi: fieldinfo) (startcomp: int) : int * int = 
  (* Construct a list of fields preceeding and including this one *)
  let prevflds = 
    let rec loop = function
        [] -> E.s (E.bug "Cannot find field %s\n" fi.fname)
      | fi' :: _ when fi' == fi -> [fi']
      | fi' :: rest -> fi' :: loop rest
    in
    loop fi.fcomp.cfields
  in
  let lastoff = 
    List.fold_left (fun acc fi' -> offsetOfFieldAcc fi' acc)
      { oaFirstFree = startcomp;
        oaLastFieldStart = 0;
        oaLastFieldWidth = 0;
        oaPrevBitPack = None } prevflds
  in
  (lastoff.oaLastFieldStart, lastoff.oaLastFieldWidth)
      
 
