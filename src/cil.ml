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
                        (* FIXME: currently always false *)
    mutable vtype: typ; 			
    mutable vdecl: location;	(* where was this variable declared? *)
    mutable vattr: attribute list;
    mutable vstorage: storage;
} 

                                        (* Storage-class information *)
and storage = 
    NoStorage | Static | Register | Extern

(* information about a field access *)
and fieldinfo = { 
    fstruct: string;                    (* "CilLval *record"-> *)
    fname: string;                      (* "Variable *field"->name *)
    ftype: typ;
    mutable fattr: attribute list;
}

(* what is the type of an expression? *)
and typ =
    TVoid of attribute list
  | TInt of ikind * attribute list
  | TBitfield of ikind * int * attribute list
  | TFloat of fkind * attribute list
  | Typedef of string * int * typ ref * attribute list
  | TPtr of typ * attribute list

              (* base type and length *)
  | TArray of typ * exp option * attribute list

               (* name, fields, id, attributes *) 
  | TStruct of string * fieldinfo list * int * attribute list 
  | TUnion of string * fieldinfo list * int * attribute list
  | TEnum of string * (string * int) list * int * attribute list
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
(* !!! int only holds 31 bits. We need long constants as well *)

(* unary operations *)
and unop =
    Neg                                 (* unary - *)
  | LNot                                (* ! *)
  | BNot                                (* ~ *)

(* binary operations *)
and binop =
    Plus
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
  | AddrOf     of lval * location
  | Compound   of typ * exp list        (* Used for initializers of 
                                         * structured types *)
(* L-Values *)
and lval =
  | Var        of varinfo * offset * location(* variable + offset *)
  | Mem        of exp * offset * location(* memory location + offset *)

and offset = 
  | NoOffset
  | Field      of fieldinfo * offset    (* l.f + offset *)
  | Index      of exp * offset          (* l[e] + offset *)
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
    { sname: string;                    (* function name *)
      slocals: varinfo list;            (* locals *)
      smaxid: int;                      (* max local id. Starts at 0 *)
      sbody: stmt;                      (* the body *)
      stype: typ;                       (* the function type *)
      sstorage: storage;
      sattr: attribute list;
    } 

type global = 
    GFun of fundec
  | GType of string * typ               (* A typedef *)
  | GVar of varinfo * exp option        (* A global variable with 
                                         * initializer. Includes function prototypes *)
  | GAsm of string                      (* Global asm statement *)
    
type file = global list
	(* global function decls, global variable decls *)






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
  | CChr(c) -> dprintf "'%c'" c
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
  | TStruct (n,fi,_,a) -> 
      if fulltype || n = "" then
        dprintf "str@[uct %s%a {@!%a@]@!} %t" n d_attrlist a
          (docList line (d_fielddecl ())) fi docName
      else
        dprintf "struct %s %t" n docName
  | TUnion (n,fi,_,a) -> 
      if fulltype || n = "" then
        dprintf "uni@[on %s%a {@!%a@]@!} %t" n d_attrlist a
          (docList line (d_fielddecl ())) fi docName
      else
        dprintf "union %s %t" n docName
  | TEnum (n, kinds, _, a) -> 
      if fulltype || n = "" then
        dprintf "enum@[ %s%a {%a@]@?} %t" n d_attrlist a
          (docList line (fun (n,i) -> dprintf "%s = %d,@?" n i)) kinds
          docName
      else
        dprintf "enum %s" n

  | TPtr (t, a)  -> 
      d_decl fulltype 
             (fun _ -> parenth t (dprintf "*%t%a" docName d_attrlist a)) 
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
      d_decl fulltype
        (fun _ -> 
          parenth restyp 
            (dprintf "%t(%a%a)" 
               docName
               (docList (chr ',') (d_videcl ())) args
               insert (if isvararg then text ", ..." else nil)))
        ()
        restyp
  | Typedef (n, _, _, _) -> 
      dprintf "%s %t" n docName


(* Only a type (such as for a cast) *)        
and d_type () t = d_decl false (fun _ -> nil) () t

and d_fieldinfo () f =
  dprintf "/* %s:%s:%a */" f.fstruct f.fname d_type f.ftype
    

(* exp *)
and d_exp () e =
  let getPrec = function
                                        (* Comparisons *)
      BinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_,_,_) -> 4
                                        (* Bit operations. Technically, they 
                                         * could have precedence 3,but I like 
                                         * parentheses around them in 
                                         * comparisons  *)
    | BinOp((Shiftlt|Shiftrt|BAnd|BOr|BXor),_,_,_,_) -> 4
                                        (* Additive *)
    | BinOp((Minus|Plus),_,_,_,_)  -> 2
                                        (* Multiplicative *)
    | BinOp((Div|Mod|Mult),_,_,_,_) -> 1
                                        (* Rest *)
    | _ -> 0
  in
  match e with
    Const(c,l) -> dprintf "%a" d_const c
  | Lval(l) -> dprintf "%a" d_lval l
  | UnOp(u,e,_,l) -> 
      let d_unop () u =
        match u with
          Neg -> text "-"
        | LNot -> text "!"
        | BNot -> text "~"
      in
      dprintf "%a %a" d_unop u d_exp e

  | BinOp(b,e1,e2,_,l) -> 
      let topprec = getPrec e in
      let docArg () e = 
        let d = d_exp () e in
        if getPrec e >= topprec then 
          dprintf "(%a)" insert d 
        else d
      in
      dprintf "@[%a %a@?%a@]" docArg e1 d_binop b docArg e2
  | CastE(t,e,l) -> dprintf "(%a)(%a)" d_type t d_exp e
  | SizeOf (t, l) -> dprintf "sizeof(%a)" d_type t
  | AddrOf(lv,lo) -> dprintf "& (%a)" d_lval lv
  | Compound (t, el) -> dprintf "(%a) {@[%a@]}" d_type t
        (docList (chr ',' ++ break) (d_exp ())) el

and d_binop () b =
  match b with
    Plus -> text "+"
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
    

(* lvalue *)
and d_lval () lv = 
  let rec d_offset dobase = function
    | NoOffset -> dobase ()
    | Index (e, o) -> 
        d_offset (fun () -> dprintf "%t[@[%a@]]" dobase d_exp e) o
    | Field (fi, o) -> 
        d_offset (fun () -> dprintf "%t.%s" dobase fi.fname) o
    | CastO (t, o) ->
        d_offset (fun () -> dprintf "((%a)%t)" d_type t dobase) o
  in
  match lv with
    Var(vi,o,_) -> d_offset (fun _ -> text vi.vname) o
  | Mem(e,Field(fi, o),_) -> 
      d_offset (fun _ -> dprintf "%a->%s" d_exp e fi.fname) o
  | Mem(e,NoOffset,_) -> dprintf "*%a" d_exp e
  | Mem(e,o,_) -> d_offset (fun _ -> d_exp () e) o
        
and d_instr () i =
  match i with
  | Set(lv,e,lo) -> begin
      (* Be nice to some special cases *)
      match e with
        BinOp(Plus,Lval(lv'),Const(CInt(1,_),_),_,_) when lv == lv' -> 
          dprintf "%a ++;" d_lval lv
      | BinOp(Minus,Lval(lv'),Const(CInt(1,_),_),_,_) when lv == lv' -> 
          dprintf "%a --;" d_lval lv
      | BinOp((Plus|Minus|BAnd|BOr|BXor|Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
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
        (docList (chr ',' ++ line) (fun x -> dprintf "\"%s\"" (escape_string x))) tmpls
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
                            (fun x -> dprintf "\"%s\"" (escape_string x))) clobs)
       
and d_stmt () s =
  match s with
    Skip -> dprintf ";"
  | Sequence(lst) -> dprintf "@[{ @[@!%a@]@!}@]" (docList line (d_stmt ())) lst
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
    d_storage f.sstorage
    (* the prototype *)
    (d_decl false (fun _ -> text f.sname)) f.stype
    (* attributes *)
    d_attrlist f.sattr
    (* locals. But eliminate first the formal arguments *)
    (docList line (fun vi -> d_videcl () vi ++ text ";"))
       (let nrArgs = 
         match f.stype with
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

let printFile (out : out_channel) (f: file) = 
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
            TStruct(n, _, _, _) when n <> "" -> doName ("struct " ^ n)
          | TUnion(n,_,_,_) when n <> ""  -> doName ("union " ^ n)
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
  List.iter (fun g -> print (d_global () g ++ line)) f
    


(******************
 ******************
 ******************)

(**** Utility functions ******)
let rec unrollType = function
    Typedef (_, _, r, _) -> unrollType !r
  | x -> x
