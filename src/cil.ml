(*


									     
		 Scott's C/C++ Parser 
 Construct a CFG from Scott's AST. 
*)
open Pretty
module E = Errormsg
module H = Hashtbl

(*
 * CIL: An intermediate language for analyzing C progams.
 *
 * Version Tue Dec 12 15:21:52 PST 2000 
 * Scott McPeak, George Necula, Wes Weimer
 *
 * This version hacked up by Wes: this is what ocaml people see after Scott 
 * creates his C version.
 *
 * Note: you may *NOT* change the order of the fields or the order in
 * which disjoint union choices are presented. The C translation code
 * has those values hard-wired.
 *)

(* where did some construct originally appear in the source code? *)
type location = { 
    line: int;				(* -1 means "do not know" *)
    col: int;
    file: string; 
}

let locUnknown = { line = -1; col = -1; file = ""; }

(* information about a variable *)
type varinfo = { 
    vid: int;		(* unique integer indentifier, one per decl *)
    vname: string;				
    vglob: bool;	(* is this a global variable? *)

    mutable vtype: typ; 			
    mutable vdecl: location;	(* where was this variable declared? *)
    mutable vattr: attribute list;
    mutable vstorage: storage;
    mutable vaddrof: bool;              (* Has its address taken *)
} 

                                        (* Storage-class information *)
and storage = 
    NoStorage | Static | Register | Extern

(* information about a field access *)
and fieldinfo = { 
    fstruct: string;                    (* "CilLval *record"-> *)
    fname: string;                      (* "Variable *field"->name *)
    mutable ftype: typ;
    mutable fattr: attribute list;
}

(* what is the type of an expression? *)
and typ =
    TVoid of attribute list
  | TInt of ikind * attribute list
  | TBitfield of ikind * int * attribute list
  | TFloat of fkind * attribute list
  | TNamed of string * typ              (* from a typedef *)
  | TPtr of typ * attribute list

              (* base type and length *)
  | TArray of typ * exp option * attribute list

               (* name, fields, id, attributes *) 

  | TStruct of string * fieldinfo list * attribute list 
  | TUnion of string * fieldinfo list * attribute list

           (* A reference to a struct or a union. The argument is "struct x" 
            * or "union x". The reference is resolved using the hash table 
            * "forwardTypeMap"  *)
  | TForward of string


  | TEnum of string * (string * int) list * attribute list
               (* result, args, isVarArg, attributes *)
  | TFun of typ * varinfo list * bool * attribute list

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
    CInt of int * string option          (* Also the textual representation *)
  | CLInt of int * int * string option   (* LInt(l,h) = l +signed (h << 31) *)
  | CStr of string
  | CChr of char 
  | CReal of float * string option       (* Also give the textual 
                                        * representation *)

(* unary operations *)
and unop =
    Neg                                 (* unary - *)
  | BNot                                (* ~ *)

(* binary operations *)
and binop =
    Plus
  | Advance                             (* Just like Plus but the first 
                                         * operand is a pointer and the next 
                                         * is an integer. Only so that we can 
                                         * print it using [ ... ] when it 
                                         * appears in a Mem lvalue *)
  | Minus
  | Mult
  | Div
  | Mod

  | Shiftlt                             (* shift left *)
  | Shiftrt

  | Lt
  | Gt
  | Le
  | Ge

  | Eq
  | Ne

  | BAnd
  | BXor                                (* exclusive-or *)
  | BOr                                 (* inclusive-or *)

                                        (* Comparison operations *)

(* expressions, no side effects *)
and exp =
    Const      of constant * location
  | Lval       of lval                  (* l-values *)
  | SizeOf     of typ * location
  | UnOp       of unop * exp * typ * location
  | BinOp      of binop * exp * exp * typ * location (* also have the type of 
                                                      * the result *)
  | CastE      of typ * exp * location
  | Compound   of typ * exp list        (* Used for initializers of *)
  | AddrOf     of lval * location
  | StartOf    of lval                  (* To be used for lval's that denote 
                                         * array. The expression denotes the 
                                         * address of the first element *)

                                         * structured types *)
(* L-Values denote contents of memory addresses *)
and lval =
  | Var        of varinfo * offset * location (* denotes * (& v + offset) *)
  | Mem        of exp * offset * location     (* denotes * (e + offset) *)

and offset = 
  | NoOffset
  | Field      of fieldinfo * offset    (* l.f + offset *)
  | Index      of exp * offset          (* l + e + offset *)
  | CastO      of typ * offset          (* ((t)l) + offset *)

(**** INSTRUCTIONS. May cause effects directly but may not have control flow.*)
and instr =
    Set        of lval * exp * location  (* An assignment. *)
  | Call       of varinfo option * exp * exp list * location
			 (* result temporary variable, 
                            function, argument list, location *)

  | Asm        of string list *         (* templates (CR-separated) *)
                  bool *                (* if it is volatile *)
                  (string * varinfo) list * (* outputs must be variables with 
                                             * constraints  *)
                  (string * exp) list * (* inputs with constraints *)
                  string list           (* clobbers *)

(**** STATEMENTS. Mostly structural information ****)
and stmt = 
  | Skip                                (* empty statement *)
  | Sequence of stmt list 
  | Loop of stmt                        (* A loop. Ends with break or a Goto 
                                         * outside *)
  | IfThenElse of exp * stmt * stmt     (* if *)
  | Label of string 
  | Goto of string
  | Return of exp option
  | Switch of exp * stmt                (* no work done by scott, sigh*)
  | Case of int 
  | Default 
  | Break
  | Continue
  | Instruction of instr
        
type fundec = 
    { svar:     varinfo;                (* Hold the name and type as a 
                                         * variable, so we can refer to it 
                                         * easily from the program *)
      mutable slocals: varinfo list;    (* locals, includes the arguments 
                                         * included in the type of svar *)
      mutable smaxid: int;              (* max local id. Starts at 0 *)
      mutable sbody: stmt;              (* the body *)
    } 

type global = 
    GFun of fundec
  | GType of string * typ               (* A typedef *)
  | GVar of varinfo * exp option        (* A global variable with 
                                         * initializer. Includes function 
                                         * prototypes  *)
  | GAsm of string                      (* Global asm statement *)
    
type file = global list
	(* global function decls, global variable decls *)


let forwardTypeMap : (string, typ) H.t = H.create 113
let clearForwardMap () = H.clear forwardTypeMap
let resolveForwardType n = 
  try
    H.find forwardTypeMap n
  with Not_found -> 
    E.s (E.unimp "Cannot resolve forward type %s\n" n)
let addForwardType key t = 
  H.add forwardTypeMap key t

    (* Use this every time you redefine a struct or a union, to make sure all 
     * the TForward see the change *)
let replaceForwardType key t = 
  try
    H.remove forwardTypeMap key;
    H.add forwardTypeMap key t
  with Not_found -> ()                  (* Do nothing if not already in *)


(**** Utility functions ******)
let rec unrollType = function
    TNamed (_, r) -> unrollType r
  | TForward n -> unrollType (resolveForwardType n)
  | x -> x

let lu = locUnknown
let integer i = Const (CInt(i, None), lu)
let zero = integer 0

let voidType = TVoid([])
let intType = TInt(IInt,[])
let charType = TInt(IChar, [])
let charPtrType = TPtr(charType,[])
let voidPtrType = TPtr(voidType, [])
let doubleType = TFloat(FDouble, [])

let var vi = Var(vi,NoOffset,locUnknown)
let mkSet lv e = Instruction(Set(lv,e,lu))
let assign vi e = mkSet (var vi) e
let call res f args = Instruction(Call(res,f,args,lu))

let mkString s = Const(CStr s, lu)

let mkSeq sl = 
  let rec removeSkip = function 
      [] -> []
    | Skip :: rest -> removeSkip rest
    | Sequence (sl) :: rest -> removeSkip (sl @ rest)
    | s :: rest -> s :: removeSkip rest
  in
  match removeSkip sl with 
    [] -> Skip
  | [s] -> s
  | sl' -> Sequence(sl')



(* the name of the C function we call to get ccgr ASTs
external parse : string -> file = "cil_main"
*)
(* 
  Pretty Printing
 *)

(* location *)
let d_loc () l =
  dprintf "/*(%s:%d:%d)*/" l.file l.line l.col

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
      let sub = String.sub str idx 1 in
      let res = match sub with
	"\n" -> "\\n"
      | "\"" -> "\\\""
      | "'" -> "\\'"
      | "\r" -> "\\r"
      | "\t" -> "\\t"
      | "\b" -> "\\b"
      | "\000" -> "\\0"
      | _ -> if sub = (Char.escaped (String.get sub 0))
      then sub
      else let code = Char.code (String.get sub 0) in
      "\\"
      ^ (conv (code / 64))
      ^ (conv ((code mod 64) / 8))
      ^ (conv (code mod 8)) in
      res ^ (build (idx + 1)) in
  build 0	

let d_ikind () = function
    IChar -> dprintf "char"
  | ISChar -> dprintf "signed char"
  | IUChar -> dprintf "unsigned char"
  | IInt -> dprintf "int"
  | IUInt -> dprintf "unsigned int"
  | IShort -> dprintf "short"
  | IUShort -> dprintf "unsigned short"
  | ILong -> dprintf "long"
  | IULong -> dprintf "unsigned long"
  | ILongLong -> dprintf "long long"
  | IULongLong -> dprintf "unsigned long"

let d_fkind () = function
    FFloat -> dprintf "float"
  | FDouble -> dprintf "double"
  | FLongDouble -> dprintf "long double"

let d_storage () = function
    NoStorage -> nil
  | Static -> text "static "
  | Extern -> text "extern "
  | Register -> text "register "

        
(* constant *)
let d_const () c =
  match c with
    CInt(_, Some s) -> text s
  | CInt(i, None) -> num i
  | CLInt(l,h, Some s) -> text s
  | CStr(s) -> dprintf "\"%s\"" (escape_string s)
  | CChr(c) -> dprintf "'%s'" (Char.escaped c)
  | CReal(_, Some s) -> text s
  | CReal(f, None) -> dprintf "%f" f
  | _ -> E.s (E.unimp "constant")

(* types. Call with a function that when invoked will fill-in the declared name *)
let rec d_decl (fulltype: bool) (docName: unit -> doc) () this = 
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
    TVoid a -> dprintf "void%a %t" d_attrlist a docName
  | TInt (ikind,a) -> dprintf "%a%a %t" d_ikind ikind d_attrlist a docName
  | TBitfield(ikind,i,a) -> 
      dprintf "%a %t : %d%a" d_ikind ikind docName i d_attrlist a
  | TFloat(fkind, a) -> dprintf "%a%a %t" d_fkind fkind d_attrlist a docName
  | TStruct (n,fi,a) -> 
      let n' = 
        if String.length n >= 5 && String.sub n 0 5 = "@anon" then "" else n in
      if fulltype || n' = "" then
        dprintf "str@[uct %s%a {@!%a@]@!} %t" n' d_attrlist a
          (docList line (d_fielddecl ())) fi docName
      else
        dprintf "struct %s %t" n' docName
  | TUnion (n,fi,a) -> 
      let n' = 
        if String.length n >= 5 && String.sub n 0 5 = "@anon" then "" else n in
      if fulltype || n' = "" then
        dprintf "uni@[on %s%a {@!%a@]@!} %t" n' d_attrlist a
          (docList line (d_fielddecl ())) fi docName
      else
        dprintf "union %s %t" n' docName

  | TForward n -> dprintf "%s %t" n docName

  | TEnum (n, kinds, a) -> 
      let n' = 
        if String.length n >= 5 && String.sub n 0 5 = "@anon" then "" else n in
      if fulltype || n' = "" then
        dprintf "enum@[ %s%a {%a@]@?} %t" n' d_attrlist a
          (docList line (fun (n,i) -> dprintf "%s = %d,@?" n i)) kinds
          docName
      else
        dprintf "enum %s" n'

  | TPtr (t, a)  -> 
      d_decl fulltype 
             (fun _ -> parenth t (dprintf "*%a %t" d_attrlist a docName )) 
             () t

  | TArray (t, lo, a) -> 
      d_decl fulltype
        (fun _ -> parenth t
                    (dprintf "%t[%a]%a" 
                       docName
                       insert (match lo with None -> nil 
                                          | Some e -> d_exp () e)
                       d_attrlist a))
        ()
        t
  | TFun (restyp, args, isvararg, a) -> 
      let args' = 
        match args with 
            [] -> [ { vname = "";
                      vtype = voidType;
                      vid   = 0;
                      vglob = false;
                      vattr = [];
                      vdecl = lu;
                      vaddrof = false; 
                      vstorage = NoStorage; } ] 
        | _ -> args
      in
      d_decl fulltype
        (fun _ -> 
          parenth restyp 
            (dprintf "%t(@[%a%a@])" 
               docName
               (docList (chr ',' ++ break) (d_videcl ())) args'
               insert (if isvararg then text ", ..." else nil)))
        ()
        restyp

  | TNamed (n, _) -> dprintf "%s %t" n docName


(* Only a type (such as for a cast) *)        
and d_type () t = d_decl false (fun _ -> nil) () t

and d_fieldinfo () f =
  dprintf "/* %s:%s:%a */" f.fstruct f.fname d_type f.ftype
    

(* exp *)

(* Parentheses level. An expression "a op b" is printed parenthesized if its 
 * parentheses level is >= that that of its context. Identifiers have the 
 * lowest level and weakly binding operators (e.g. |) have the largest level 
 *)
and getParenthLevel = function
  | Compound _ -> 10

                                        (* Comparisons *)
  | BinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_,_,_) -> 8
                                        (* Bit operations. Technically, they 
                                         * could have level 9,but I like 
                                         * parentheses around them in 
                                         * comparisons  *)
  | BinOp((BOr|BXor|BAnd|Shiftlt|Shiftrt),_,_,_,_) -> 7
                                        (* Additive *)
  | BinOp((Minus|Plus|Advance),_,_,_,_)  -> 6
                                        (* Multiplicative *)
  | BinOp((Div|Mod|Mult),_,_,_,_) -> 4

                                        (* Unary *)
  | AddrOf(_,_) -> 3
  | UnOp((Neg|BNot),_,_,_) -> 3
  | CastE(_,_,_) -> 3

                                        (* Lvals *)
  | Lval(Mem(_,_,_)) -> 2                   
  | Lval(Var(_,(Field _|CastO _),_)) -> 2
  | SizeOf _ -> 2

  | Lval(Var(_,NoOffset,_)) -> 1        (* Plain variables *)
  | Const _ -> 1                        (* Constants *)

                                        (* Rest *)

(* Print an expression assuming a precedence for the context. Use a small 
 * number to parenthesize the printed expression. 0 guarantees parentheses. 1 
 * will parenthesize everything but identifiers. *)
and d_expprec contextprec () e = 
  if getParenthLevel e >= contextprec then
    dprintf "(%a)" d_exp e
  else
    d_exp () e

and d_exp () e = 
  let level = getParenthLevel e in
  match e with
    Const(c,l) -> dprintf "%a" d_const c
  | Lval(l) -> dprintf "%a" d_lval l
  | UnOp(u,e1,_,l) -> 
      let d_unop () u =
        match u with
          Neg -> text "-"
        | BNot -> text "~"
      in
      dprintf "%a %a" d_unop u (d_expprec level) e1

  | BinOp(b,e1,e2,_,l) -> 
      dprintf "@[%a %a@?%a@]" 
        (d_expprec level) e1 d_binop b (d_expprec level) e2
  | CastE(t,e,l) -> dprintf "(%a)%a" d_type t (d_expprec level) e
  | SizeOf (t, l) -> dprintf "sizeof(%a)" d_type t
  | Compound (t, el) -> dprintf "(%a) {@[%a@]}" d_type t
        (docList (chr ',' ++ break) (d_exp ())) el
  | AddrOf(lv,lo) -> dprintf "& %a" (d_lvalprec 2) lv
  

and d_binop () b =
  match b with
    Plus -> text "+"
  | Advance -> text "+"
  | Minus -> text "-"
  | Mult -> text "*"
  | Div -> text "/"
  | Mod -> text "%"
  | Shiftlt -> text "<<"
  | Shiftrt -> text ">>"
  | Lt -> text "<"
  | Gt -> text ">"
  | Le -> text "<="
  | Ge -> text ">="
  | Eq -> text "=="
  | Ne -> text "!="
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
          
and d_attrlist () al =
  (* Take out the const and volatile *)
  let rec loop remaining = function
      [] -> begin
        match remaining with
          [] -> nil
        | _ -> 
            dprintf " __attribute__(%a)"
              (docList (chr ',' ++ break) 
                 (fun a -> dprintf "(%a)" d_attr a)) al
      end
    | (AId("const")) :: rest -> 
        dprintf " const%a" insert (loop remaining rest)
    | (AId("volatile")) :: rest -> 
        dprintf " volatile%a" insert (loop remaining rest)
    | x :: rest -> loop (x :: remaining) rest
  in
  loop [] al
    

and d_plainexp () = function
    Const(c,l) -> dprintf "Const(%a)" d_const c
  | Lval(lv) -> dprintf "Lval(@[%a@])" d_plainlval lv
  | e -> d_exp () e

and d_plainlval () = function
  | Var(vi,o,l) -> dprintf "Var(@[%s,@?%a@])" vi.vname d_plainoffset o
  | Mem(BinOp(Advance,e1,e2,_,_),o,l) -> 
      dprintf "Mem(@[Idx(@[%a,@?%a@],@?%a@])" 
        d_plainexp e1 d_plainexp e2 d_plainoffset o
  | Mem(e,o,l) -> dprintf "Mem(@[%a,@?%a@])" d_plainexp e d_plainoffset o

and d_plainoffset () = function
    NoOffset -> text "NoOffset"
  | Field(fi,o) -> 
      dprintf "Field(@[%s:%a,@?%a@])" 
        fi.fname d_plaintype fi.ftype d_plainoffset o
  | CastO(t, o) -> dprintf "CastO(@[%a,@?%a@])" d_plaintype t d_plainoffset o

and d_plaintype () = function
    TVoid a -> dprintf "TVoid(@[%a@])" d_attrlist a
  | TInt(ikind, a) -> dprintf "TInt(@[%a,@?%a@])" d_ikind ikind d_attrlist a
  | TFloat(fkind, a) -> 
      dprintf "TFloat(@[%a,@?%a@])" d_fkind fkind d_attrlist a
  | TBitfield(ikind,i,a) -> 
      dprintf "TBitfield(@[%a,@?%d,@?%a@])" d_ikind ikind i d_attrlist a
  | TNamed (n, t) ->
      dprintf "TNamed(@[%s,@?%a@])" n d_plaintype t
  | TForward n -> dprintf "TForward(%s)" n
  | TPtr(t, a) -> dprintf "TPtr(@[%a,@?%a@])" d_plaintype t d_attrlist a
  | TArray(t,l,a) -> 
      let dl = match l with 
        None -> text "None" | Some l -> dprintf "Some(@[%a@])" d_plainexp l in
      dprintf "TArray(@[%a,@?%a,@?%a@])" 
        d_plaintype t insert dl d_attrlist a
  | TEnum(n,_,a) -> dprintf "Enum(%s,@[%a@])" n d_attrlist a
  | TFun(tr,args,isva,a) -> 
      dprintf "TFun(@[%a,@?%a%s,@?%a@])"
        d_plaintype tr 
        (docList (chr ',' ++ break) 
           (fun a -> dprintf "%s: %a" a.vname d_plaintype a.vtype)) args
        (if isva then "..." else "") d_attrlist a
  | TStruct(n,flds,a) -> 
      dprintf "TStruct(@[%s,@?%a,@?%a@])" n
        (docList (chr ',' ++ break) 
           (fun f -> dprintf "%s : %a" f.fname d_plaintype f.ftype)) flds
        d_attrlist a
  | TUnion(n,flds,a) -> 
      dprintf "TUnion(@[%s,@?%a,@?%a@])" n
        (docList (chr ',' ++ break) 
           (fun f -> dprintf "%s : %a" f.fname d_plaintype f.ftype)) flds
        d_attrlist a


(* lvalue *)
and d_lvalprec contextprec () lv = 
  if getParenthLevel (Lval(lv)) >= contextprec then
    dprintf "(%a)" d_lval lv
  else
    d_lval () lv
  
and d_lval () lv = 
  let rec d_offset dobase = function
    | NoOffset -> dprintf "%t" dobase
    | Field (fi, o) -> 
        d_offset (fun () -> dprintf "%t.%s" dobase fi.fname) o
    | CastO (t, o) ->
        d_offset (fun () -> dprintf "((%a)%t)" d_type t dobase) o
  in
  match lv with
    Var(vi,o,_) -> d_offset (fun _ -> text vi.vname) o
  | Mem(BinOp(Advance,e1,e2,_,_),o,_) -> 
      d_offset (fun _ -> dprintf "%a[%a]" (d_expprec 3) e1 d_exp e2) o
  | Mem(e,Field(fi, o),_) -> 
      d_offset (fun _ -> dprintf "%a->%s" (d_expprec 3) e fi.fname) o
  | Mem(e,NoOffset,_) -> dprintf "*%a" (d_expprec 3) e

  | Mem(e,o,_) -> d_offset (fun _ -> d_exp () e) o
        
and d_instr () i =
  match i with
  | Set(lv,e,lo) -> begin
      (* Be nice to some special cases *)
      match e with
        BinOp((Plus|Advance),Lval(lv'),Const(CInt(1,_),_),_,_) when lv == lv' -> 
          dprintf "%a ++;" d_lval lv
      | BinOp(Minus,Lval(lv'),Const(CInt(1,_),_),_,_) when lv == lv' -> 
          dprintf "%a --;" d_lval lv
      | BinOp((Plus|Advance|Minus|BAnd|BOr|BXor|
               Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
              Lval(lv'),e,_,_) when lv == lv' -> 
          dprintf "%a %a= %a;" d_lval lv d_binop bop d_exp e
      | _ -> dprintf "%a = %a;" d_lval lv d_exp e
  end
  | Call(vio,e,args,loc) ->
      dprintf "%s%a(@[%a@]);" 
        (match vio with None -> "" | Some vi -> vi.vname ^ " = ") 
        insert (match e with Lval(Var _) -> d_exp () e 
                             | _ -> dprintf "(%a)" d_exp e)
	(docList (chr ',' ++ break) (d_exp ())) args
  | Asm(tmpls, isvol, outs, ins, clobs) ->
      dprintf "__asm__ %a(@[%a%a%a%a@]);@!"
        insert (if isvol then text "__volatile__" else nil)
        (docList (chr ',' ++ line) 
           (fun x -> dprintf "\"%s\"" (escape_string x))) tmpls
        insert 
        (if outs = [] && ins = [] && clobs = [] then 
          nil
        else 
          dprintf ": %a" (docList (chr ',' ++ break) 
                            (fun (c, vi) -> dprintf "\"%s\" (%s)"
                                (escape_string c) vi.vname)) outs)
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
  match s with
    Skip -> dprintf ";"
  | Sequence(lst) -> dprintf "@[{ @[@!%a@]@!}@]" 
        (docList line (d_stmt ())) lst
  | Loop(Sequence(IfThenElse(e,Skip,Break) :: rest)) -> 
      dprintf "whi@[le (%a)@!%a@]" d_exp e d_stmt (Sequence rest)
  | Loop(stmt) -> 
      dprintf "whi@[le (1)@!%a@]" d_stmt stmt
  | IfThenElse(e,a,(Skip|Sequence([Skip]))) -> 
      dprintf "if @[(%a)@!%a@]" d_exp e d_stmt a
  | IfThenElse(e,a,b) -> 
      dprintf "@[if @[(%a)@!%a@]@!els@[e@!%a@]@]" d_exp e d_stmt a d_stmt b
  | Label(s) -> dprintf "%s:" s
  | Case(i) -> dprintf "case %d: " i
  | Goto(s) -> dprintf "goto %s;" s
  | Break  -> dprintf "break;"
  | Continue -> dprintf "continue;";
  | Return(None) -> text "return;"
  | Return(Some e) -> dprintf "return (%a);" d_exp e
  | Switch(e,s) -> dprintf "@[switch (%a)@!%a@]" d_exp e d_stmt s
  | Default -> dprintf "default:"
  | Instruction(i) -> d_instr () i
        
and d_fun_decl () f = 
  dprintf "%a%a %a@!{ @[%a@!%a@]@!}" 
    d_storage f.svar.vstorage
    (* the prototype *)
    (d_decl false (fun _ -> text f.svar.vname)) f.svar.vtype
    (* attributes *)
    d_attrlist f.svar.vattr
    (* locals. But eliminate first the formal arguments *)
    (docList line (fun vi -> d_videcl () vi ++ text ";"))
       (let nrArgs = 
         match f.svar.vtype with
           TFun(_, args, _, _) -> List.length args
         | _ -> E.s (E.bug "non-function type")
       in
       let rec drop n = function
           l when n = 0 -> l
         | [] -> E.s (E.bug "Too few locals")
         | _ :: rest -> drop (n - 1) rest
       in
       drop nrArgs f.slocals)
    (* the body *)
    d_stmt f.sbody

and d_videcl () vi = 
  dprintf "%a%a %a" d_storage vi.vstorage
    (d_decl false (fun _ -> text vi.vname)) vi.vtype
    d_attrlist vi.vattr
    
and d_fielddecl () fi = 
  dprintf "%a %a;"
    (d_decl false (fun _ -> text fi.fname)) fi.ftype
    d_attrlist fi.fattr

let printFile (out : out_channel) (globs : file) = 
  let print x = fprint out 80 x in
  print (text "/* Generated by safecc */\n\n");
  let definedTypes : (string, bool) H.t = H.create 17 in
  let d_global () = function
      GFun fundec -> d_fun_decl () fundec ++ line
    | GType (str, typ) -> 
        let doName n = 
          try begin
            ignore (H.find definedTypes n); false
          end with Not_found -> begin
            H.add definedTypes n true;
            true
          end
        in
        let fulltype = 
          match typ with
            TStruct(n, _, _) -> doName ("struct " ^ n)
          | TUnion(n,_,_) -> doName ("union " ^ n)
          | _ -> true
        in
        if str = "" then
          if fulltype then
            dprintf "%a;@!" (d_decl fulltype (fun _ -> nil)) typ
          else
            nil                         (* Already defined *)
        else 
          dprintf "typedef %a;@!" (d_decl fulltype (fun _ -> text str)) typ

  | GVar (vi, eo) -> dprintf "%a %a;"
        d_videcl vi 
        insert (match eo with None -> nil | Some e -> 
                dprintf " = %a" d_exp e)
  | GAsm s -> dprintf "__asm__(\"%s\")@!" (escape_string s)
  in
  List.iter (fun g -> print (d_global () g ++ line)) globs

    


(******************
 ******************
 ******************)



  (* Compute some sizeOf to help with constant folding *)
let rec intSizeOf = function            (* Might raise Not_found *)
    TInt((IUChar|IChar|ISChar), _) -> 1
  | TInt((IShort|IUShort), _) -> 2
  | TInt((IInt|IUInt), _) -> 4
  | TInt((ILong|IULong), _) -> 4
  | TInt((ILongLong|IULongLong), _) ->  8
  | TFloat(FFloat, _) ->  4
  | TFloat(FDouble, _) ->  8
  | TFloat(FLongDouble, _) ->  10
  | TEnum _ ->  4
  | TPtr _ ->  4
  | TArray(t, Some (Const(CInt(l,_),_)),_) -> (intSizeOf t) * l
  | TNamed(_, r) -> intSizeOf r
  | TForward r -> intSizeOf (resolveForwardType r)
  | TStruct(_,flds,_) -> 
      let rec loop = function
          [] -> 0
        | f :: flds -> align (intSizeOf f.ftype) + loop flds
      in
      loop flds
  | _ -> raise Not_found

and intSizeOfNoExc t = 
  try
    intSizeOf t
  with Not_found -> 
    E.s (E.unimp "Cannot compute the sizeof(%a)\n" d_type t)

and sizeOf t = 
  try
    integer (intSizeOf t) 
  with Not_found -> SizeOf(t, lu)

and align n = ((n + 3) lsr 2) lsl 2


 (* Scan all the expressions in a statement *)
let iterExp (f: exp -> unit) (body: stmt) : unit = 
  let rec fExp e = f e; fExp' e
  and fExp' = function
      (Const _|SizeOf _) -> ()
    | Lval lv -> fLval lv
    | UnOp(_,e,_,_) -> fExp e
    | BinOp(_,e1,e2,_,_) -> fExp e1; fExp e2
    | CastE(_, e,_) -> fExp e
    | Compound (_, el) -> List.iter fExp el
    | AddrOf (lv,_) -> fLval lv

  and fLval = function
      Var(_,off,_) -> fOff off
    | Mem(e,off,_) -> fExp e; fOff off
  and fOff = function
      Field (_, o) -> fOff o
    | CastO (_, o) -> fOff o
    | NoOffset -> ()
  and fStmt = function
      (Skip|Break|Continue|Label _|Goto _|Case _|Default|Return None) -> ()
    | Sequence s -> List.iter fStmt s
    | Loop s -> fStmt s
    | IfThenElse (e, s1, s2) -> fExp e; fStmt s1; fStmt s2
    | Return(Some e) -> fExp e
    | Switch (e, s) -> fExp e; fStmt s
    | Instruction(Set(lv,e,_)) -> fLval lv; fExp e
    | Instruction(Call(_,f,args,_)) -> fExp f; List.iter fExp args
    | Instruction(Asm(_,_,_,ins,_)) -> 
        List.iter (fun (_, e) -> fExp e) ins
  in
  fStmt body



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
           }  in
  fdec.slocals <- fdec.slocals @ [vi];
  vi

let makeTempVar fdec typ = 
  let name = "tmp" ^ (string_of_int (1 + fdec.smaxid)) in
  makeLocalVar fdec name typ


   (* Make a global variable *) 
let makeGlobalVar name typ = 
  let vi = { vname = name; 
             vid   = H.hash name;
             vglob = true;
             vtype = typ;
             vdecl = lu;
             vattr = [];
             vstorage = NoStorage;
             vaddrof = false;
           }  in
  vi


   (* Make an empty function *)
let emptyFunction name = 
  { svar  = makeGlobalVar name (TFun(voidType, [], false,[]));
    smaxid = 0;
    slocals = [];
    sbody = Skip;
  } 

    (* A dummy function declaration handy for initialization *)
let dummyFunDec = emptyFunction "@dummy"




  (* Type comparison *
let rec sameType t1 t2 = 
  match t1, t2 with
    TInt _, TInt _ -> t1 = t2
  | TVoid _, TVoid _ -> t1 = t2
  | TFloat _, TFloat _ -> t1 = t2
  | TEnum _, TEnum _ -> t1 = t2
  | TBitfield _, TBitfield _ -> t1 = t2
  | TPtr(t1,a1), TPtr(t2,a2) -> sameType t1 t2 && a1 = a2
  | TArray(t1,l1,a1), TArray(t2,l2,a2) -> sameType t1 t2 && l1 = l2 && a1 = a2
  | TStruct(n1,_,a1), TStruct(n2,_,a2) -> n1 = n2 && a1 = a2
  | TUnion(n1,_,a1), TUnion(n2,_,a2) -> n1 = n2 && a1 = a2
  | TNamed(_, t1), _ -> sameType t1 t2
  | TFun(t1,args1,isva1,a1), TFun(t2,args2,isva2,a2) -> 
      sameType t1 t2 && a1 = a2 && isva1 = isva2 &&
      let rec sameArgs = function
          [], [] -> true
        | arg1 :: args1, arg2 :: args2 -> 
            sameType arg1.vtype arg2.vtype && arg1.vattr = arg2.vattr
              && sameArgs (args1, args2)
        | _ -> false
      in
      sameArgs (args1, args2)

  | _, TNamed(_, t2) -> sameType t1 t2
  | TIncomplete tr1, _ -> sameType !tr1 t2
  | _, TIncomplete tr2 -> sameType t1 !tr2
  | _, _ -> false
*)
 (* A hash code function for types. Guaranteed to return the same thing for 
  * two types that compare with sameType *
let rec hashType (t: typ) : int =
  match t with
    (TVoid _|TInt _|TFloat _|TEnum _|TBitfield _) -> H.hash t
  | TStruct (n, _, _, a) -> H.hash ("s", n, a)
  | TUnion (n, _, _, a) -> H.hash ("u", n, a)
  | TPtr (t', a) -> H.hash ("p", hashType t', a)
  | TArray (t',l,a) -> H.hash ("a", hashType t', l, a)
  | TFun (tr, args, isva, a) -> 
      H.hash ("f", hashType tr, isva, a, 
              List.map (fun arg -> (hashType arg.vtype, arg.vattr)) args)
  | TNamed (_, t) -> hashType t
  | TIncomplete tr -> hashType !tr

*)

     (* Type signatures. Two types are identical iff they have identical 
      * signatures *)
type typsig = 
    TSArray of typsig * exp option * attribute list
  | TSPtr of typsig * attribute list
  | TSStruct of string * attribute list
  | TSUnion of string * attribute list
  | TSFun of typsig * (typsig * attribute list) list * bool * attribute list
  | TSBase of typ

(* Compute a type signature *)
let rec typeSig t = 
  match t with 
  | (TInt _ | TFloat _ | TEnum _ | TBitfield _ | TVoid _) -> TSBase t
  | TPtr (t, a) -> TSPtr (typeSig t, a)
  | TArray (t,l,a) -> TSArray(typeSig t, l, a)
  | TStruct (n, _, a) -> TSStruct (n, a)
  | TUnion (n, _, a) -> TSUnion (n, a)
  | TFun(rt,args,isva,a) -> TSFun(typeSig rt, 
                                  List.map (fun vi -> (typeSig vi.vtype, 
                                                       vi.vattr)) args,
                                  isva, a)
  | TNamed(_, t) -> typeSig t
  | TForward n -> begin
      let l = String.length n in
      try
        if String.sub n 0 1 = "s" then 
          TSStruct(String.sub n 7 (l - 7), [])
        else
          TSUnion(String.sub n 6 (l - 6), [])
      with _ -> E.s (E.bug "Invalid TForward(%s)" n)
  end
      
let dExp : doc -> exp = 
  function d -> Const(CStr(sprint 80 d),lu)

let dStmt : doc -> stmt = 
  function d -> Instruction(Asm([sprint 80 d], false, [], [], []))

