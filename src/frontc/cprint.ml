(* cprint -- pretty printer of C program from abstract syntax
**
** Project:	FrontC
** File:	cprint.ml
** Version:	2.1e
** Date:	9.1.99
** Author:	Hugues Cassé
**
**	1.0		2.22.99	Hugues Cassé	First version.
**	2.0		3.18.99	Hugues Cassé	Compatible with Frontc 2.1, use of CAML
**									pretty printer.
**	2.1		3.22.99	Hugues Cassé	More efficient custom pretty printer used.
**	2.1a	4.12.99	Hugues Cassé	Correctly handle:
**									char *m, *m, *p; m + (n - p)
**	2.1b	4.15.99	Hugues Cassé	x + (y + z) stays x + (y + z) for
**									keeping computation order.
**	2.1c	7.23.99	Hugues Cassé	Improvement of case and default display.
**	2.1d	8.25.99	Hugues Cassé	Rebuild escape sequences in string and
**									characters.
**	2.1e	9.1.99	Hugues Cassé	Fix, recognize and correctly display '\0'.
*)

(* George Necula: I changed this pretty dramatically since CABS changed *)
open Cil
open Cabs
let version = "Cprint 2.1e 9.1.99 Hugues Cassé"

let lu = {line = -1; file = "loc unknown";}
let cabslu = {lineno = -10; filename = "cabs loc unknown";}

let curLoc = ref cabslu

let msvcMode = ref false
let printLines = ref true
(*
** FrontC Pretty printer
*)
let out = ref stdout
let width = ref 80
let tab = ref 8
let max_indent = ref 60

let line = ref ""
let line_len = ref 0
let current = ref ""
let current_len = ref 0
let spaces = ref 0
let follow = ref 0
let roll = ref 0

let print_tab size =
	for i = 1 to size / 8 do
		output_char !out '\t'
	done;
	for i  = 1 to size mod 8 do
		output_char !out ' '
	done

let flush _ =
	if !line <> "" then begin
		print_tab (!spaces + !follow);
		output_string !out !line;
		line := "";
		line_len := 0
	end

let commit _ =
  if !current <> "" then begin
    if !line = "" then begin
      line := !current;
      line_len := !current_len
    end else begin
      line := (!line ^ " " ^ !current);
      line_len := !line_len + 1 + !current_len
    end;
    current := "";
    current_len := 0
  end


let addline () =
  curLoc := {lineno = !curLoc.lineno+1;
              filename = !curLoc.filename}
       
       
let new_line _ =
  commit ();
  if !line <> "" then begin
    flush ();
    addline();
    output_char !out '\n'
  end;
  follow := 0
       
let force_new_line _ =
  commit ();
  flush ();
  addline();
  output_char !out '\n';
  follow := 0
       
let indent _ =
  new_line ();
  spaces := !spaces + !tab;
  if !spaces >= !max_indent then begin
    spaces := !tab;
    roll := !roll + 1
  end
      
let indentline _ =
  new_line ();
  if !spaces >= !max_indent then begin
    spaces := !tab;
    roll := !roll + 1
  end
      
let unindent _ =
  new_line ();
  spaces := !spaces - !tab;
  if (!spaces <= 0) && (!roll > 0) then begin
    spaces := ((!max_indent - 1) / !tab) * !tab;
    roll := !roll - 1
  end
      
let space _ = commit ()

let print str =
  current := !current ^ str;
  current_len := !current_len + (String.length str);
  if (!spaces + !follow + !line_len + 1 + !current_len) > !width
  then begin
    if !line_len = 0 then commit ();
    flush ();
    addline();
    output_char !out '\n';
    if !follow = 0 then follow := !tab
  end

let setLoc (l : cabsloc) =
  let tempcur = current in
  if !printLines then 
    if (l.lineno <> !curLoc.lineno) || l.filename <> !curLoc.filename then 
      begin
        let oldspaces = !spaces in
(*        spaces := 0;
        new_line(); *)
        print "#";
        if !msvcMode then print "line";
        print " ";
        print (string_of_int l.lineno);
        if (l.filename <> !curLoc.filename) then begin
          print (" \"" ^ l.filename ^ "\"")
        end;
        spaces := oldspaces;
        new_line();
        curLoc := l
      end



(*
** Useful primitives
*)
let print_list print_sep print_elt lst = 
  let _ = List.fold_left
      (fun com elt ->
	if com then print_sep ();
	print_elt elt;
	true)
      false
      lst in
  ()

let print_commas nl fct lst =
  print_list (fun () -> print ","; if nl then new_line() else space()) fct lst
	

let escape_string str =
  let lng = String.length str in
  let conv value = 
    String.make 1 
      (Char.chr (value + 
		   (if value < 10 then (Char.code '0') 
                   else (Char.code 'a' - 10)))) in
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
	
let print_string s = 
  print ("\"" ^ escape_string s ^ "\"")

(* 
** Base Type Printing
*)

let rec print_specifiers (specs: spec_elem list) = 
  let print_spec_elem = function
      SpecTypedef -> print "typedef "
    | SpecInline -> print "__inline "
    | SpecStorage sto -> 
        print (match sto with
          NO_STORAGE -> ""
        | AUTO -> "auto "
        | STATIC -> "static "
        | EXTERN -> "extern "
        | REGISTER -> "register ")
    | SpecAttr al -> print_attribute al; space ()
    | SpecType bt -> print_type_spec bt
  in
  List.iter print_spec_elem specs

and print_type_spec = function
    Tvoid -> print "void "
  | Tchar -> print "char "
  | Tshort -> print "short "
  | Tint -> print "int "
  | Tlong -> print "long "
  | Tint64 -> print "__int64 "
  | Tfloat -> print "float "
  | Tdouble -> print "double "
  | Tsigned -> print "signed "
  | Tunsigned -> print "unsigned "
  | Tnamed s -> print s; space ();
  | Tstruct (n, None) -> print ("struct " ^ n ^ " ")
  | Tstruct (n, Some flds) -> 
      if flds = [] then print "struct { } " 
      else print_fields ("struct " ^ n) flds
  | Tunion (n, None) -> print ("union " ^ n ^ " ")
  | Tunion (n, Some flds) -> 
      if flds = [] then print "union { } " 
      else print_fields ("union " ^ n) flds
  | Tenum (n, None) -> print ("enum " ^ n ^ " ")
  | Tenum (n, Some enum_items) -> print_enum n enum_items
  | TtypeofE e -> print "__typeof__("; print_expression e 1; print ") "
  | TtypeofT (s,d) -> print "__typeof__("; print_onlytype (s, d); print ") "


(* This is the main printer for declarations. It is easy bacause the 
 * declarations are laid out as they need to be printed. *)
and print_decl (n: string) = function
    JUSTBASE -> print n
  | PARENTYPE (al1, d, al2) -> 
      print "("; 
      print_attributes al1; space ();
      print_decl n d; space ();
      print_attributes al2; print ")"
  | BITFIELD e -> 
      if n <> "___missing_field_name" then print n;
      print " : ";
      print_expression e 1
  | PTR (al, d) -> 
      print "* ";
      print_attributes al; space ();
      print_decl n d
  | ARRAY (d, e) -> 
      print_decl n d;
      print "[";
      if e <> NOTHING then print_expression e 1;
      print "]"
  | PROTO(d, args, isva) -> 
      print_decl n d;
      print "(";
      print_params args isva;
      print ")"


and print_fields  id (flds : name_group list) =
  print id;
  if flds = [] then ()
  else begin
    print " {";
    indent ();
    List.iter
      (fun fld -> print_name_group fld; print ";"; new_line ())
      flds;
    unindent ();
    print "} "
  end
      
and print_enum id items =
  print ("enum " ^ id);
  if items = []
  then ()
  else begin
    print " {";
    indent ();
    print_commas
      true
      (fun (id, exp) -> print id;
	if exp = NOTHING then ()
	else begin
	  space ();
	  print "= ";
	  print_expression exp 1
	end)
      items;
    unindent ();
    print "} ";
  end

  
and print_onlytype (specs, dt) =
  print_specifiers specs;
  print_decl "" dt
    
and print_name ((n, decl, attrs) : name) =
  print_decl n decl;
  space ();
  print_attributes attrs

and print_init_name ((n, i) : init_name) =
  print_name n;
  if i <> NO_INIT then begin
    space ();
    print "= ";
    print_init_expression i
  end
            
and print_name_group (specs, names) =
  print_specifiers specs;
  print_commas false print_name names
    

and print_init_name_group (specs, names) =
  print_specifiers specs;
  print_commas false print_init_name names
    
and print_single_name (specs, name) =
  print_specifiers specs;
  print_name name

and print_params (pars : single_name list) (ell : bool) =
  print_commas false print_single_name pars;
  if ell then print (if pars = [] then "..." else ", ...") else ()
    
and print_old_params pars ell =
  print_commas false (fun id -> print id) pars;
  if ell then print (if pars = [] then "..." else ", ...") else ()
    

(*
** Expression printing
**		Priorities
**		16	varaibles
**		15	. -> [] call()
**		14  ++, -- (post)
**		13	++ -- (pre) ~ ! - + & *(cast)
**		12	* / %
**		11	+ -
**		10	<< >>
**		9	< <= > >=
**		8	== !=
**		7	&
**		6	^
**		5	|
**		4	&&
**		3	||
**		2	? :
**		1	= ?=
**		0	,				
*)
and get_operator exp =
  match exp with
    NOTHING -> ("", 16)
  | UNARY (op, _) ->
      (match op with
	MINUS -> ("-", 13)
      | PLUS -> ("+", 13)
      | NOT -> ("!", 13)
      | BNOT -> ("~", 13)
      | MEMOF -> ("*", 13)
      | ADDROF -> ("&", 13)
      | PREINCR -> ("++", 13)
      | PREDECR -> ("--", 13)
      | POSINCR -> ("++", 14)
      | POSDECR -> ("--", 14))
  | BINARY (op, _, _) ->
      (match op with
	MUL -> ("*", 12)
      | DIV -> ("/", 12)
      | MOD -> ("%", 12)
      | ADD -> ("+", 11)
      | SUB -> ("-", 11)
      | SHL -> ("<<", 10)
      | SHR -> (">>", 10)
      | LT -> ("<", 9)
      | LE -> ("<=", 9)
      | GT -> (">", 9)
      | GE -> (">=", 9)
      | EQ -> ("==", 8)
      | NE -> ("!=", 8)
      | BAND -> ("&", 7)
      | XOR -> ("^", 6)
      | BOR -> ("|", 5)
      | AND -> ("&&", 4)
      | OR -> ("||", 3)
      | ASSIGN -> ("=", 1)
      | ADD_ASSIGN -> ("+=", 1)
      | SUB_ASSIGN -> ("-=", 1)
      | MUL_ASSIGN -> ("*=", 1)
      | DIV_ASSIGN -> ("/=", 1)
      | MOD_ASSIGN -> ("%=", 1)
      | BAND_ASSIGN -> ("&=", 1)
      | BOR_ASSIGN -> ("|=", 1)
      | XOR_ASSIGN -> ("^=", 1)
      | SHL_ASSIGN -> ("<<=", 1)
      | SHR_ASSIGN -> (">>=", 1))
  | QUESTION _ -> ("", 2)
  | CAST _ -> ("", 13)
  | CALL _ -> ("", 15)
  | COMMA _ -> ("", 0)
  | CONSTANT _ -> ("", 16)
  | VARIABLE name -> ("", 16)
  | EXPR_SIZEOF exp -> ("", 16)
  | TYPE_SIZEOF _ -> ("", 16)
  | INDEX (exp, idx) -> ("", 15)
  | MEMBEROF (exp, fld) -> ("", 15)
  | MEMBEROFPTR (exp, fld) -> ("", 15)
  | GNU_BODY _ -> ("", 17)
        
and print_comma_exps exps =
  print_commas false (fun exp -> print_expression exp 1) exps
    
and print_init_expression (iexp: init_expression) : unit = 
  match iexp with 
    NO_INIT -> ()
  | SINGLE_INIT e -> print_expression e 1
  | COMPOUND_INIT  initexps ->
      let doinitexp = function
          NEXT_INIT, e -> print_init_expression e
        | i, e -> 
            let rec doinit = function
                NEXT_INIT -> ()
              | INFIELD_INIT (fn, i) -> print ("." ^ fn); doinit i
              | ATINDEX_INIT (e, i) -> 
                  print "[";
                  print_expression e 1;
                  print "]";
                  doinit i
                in
            doinit i; print " = "; 
            print_init_expression e
      in
      print "{";
      print_commas false doinitexp initexps;
      print "}"


and print_expression (exp : expression) (lvl : int) =
  let (txt, lvl') = get_operator exp in
  let _ = if lvl > lvl' then print "(" else () in
  let _ = match exp with
    NOTHING -> ()
  | UNARY (op, exp') ->
      (match op with
	POSINCR | POSDECR ->
	  print_expression exp' lvl';
	  print txt
      | _ ->
	  print txt; space (); (* Print the space to avoid --5 *)
	  print_expression exp' lvl')
  | BINARY (op, exp1, exp2) ->
			(*if (op = SUB) && (lvl <= lvl') then print "(";*)
      print_expression exp1 lvl';
      space ();
      print txt;
      space ();
			(*print_expression exp2 (if op = SUB then (lvl' + 1) else lvl');*)
      print_expression exp2 (lvl' + 1)
			(*if (op = SUB) && (lvl <= lvl') then print ")"*)
  | QUESTION (exp1, exp2, exp3) ->
      print_expression exp1 2;
      space ();
      print "? ";
      print_expression exp2 2;
      space ();
      print ": ";
      print_expression exp3 2;
  | CAST (typ, exp) ->
      print "(";
      print_onlytype typ;
      print ")";
      print_expression exp 15
  | CALL (exp, args) ->
      print_expression exp 16;
      print "(";
      print_comma_exps args;
      print ")"
  | COMMA exps ->
      print_comma_exps exps
  | CONSTANT cst ->
      (match cst with
	CONST_INT i -> print i
      | CONST_FLOAT r -> print r
      | CONST_CHAR c -> print ("'" ^ (escape_string c) ^ "'")
      | CONST_STRING s -> print_string s)

  | VARIABLE name ->
      print name
  | EXPR_SIZEOF exp ->
      print "sizeof(";
      print_expression exp 0;
      print ")"
  | TYPE_SIZEOF (bt,dt) ->
      print "sizeof(";
      print_onlytype (bt, dt);
      print ")"
  | INDEX (exp, idx) ->
      print_expression exp 16;
      print "[";
      print_expression idx 0;
      print "]"
  | MEMBEROF (exp, fld) ->
      print_expression exp 16;
      print ("." ^ fld)
  | MEMBEROFPTR (exp, fld) ->
      print_expression exp 16;
      print ("->" ^ fld)
  | GNU_BODY (labs, blk) ->
      print "(";
      print_block labs blk;
      print ")" in
  if lvl > lvl' then print ")" else ()
    

(*
** Statement printing
*)
and print_statement stat =
  match stat with
    NOP (loc) ->
      setLoc(loc);
      print ";";
      new_line ()
  | COMPUTATION (exp, loc) ->
      setLoc(loc);
      print_expression exp 0;
      print ";";
      new_line ()
  | BLOCK (blk, loc) -> print_block [] blk

  | SEQUENCE (s1, s2, loc) ->
      setLoc(loc);
      print_statement s1;
      print_statement s2;
  | IF (exp, s1, s2, loc) ->
      setLoc(loc);
      print "if(";
      print_expression exp 0;
      print ")";
      print_substatement s1;
      (match s2 with
      | NOP(_) -> ()
      | _ -> begin
          print "else";
          print_substatement s2;
        end)
  | WHILE (exp, stat, loc) ->
      setLoc(loc);
      print "while(";
      print_expression exp 0;
      print ")";
      print_substatement stat
  | DOWHILE (exp, stat, loc) ->
      setLoc(loc);
      print "do";
      print_substatement stat;
      print "while(";
      print_expression exp 0;
      print ");";
      new_line ();
  | FOR (exp1, exp2, exp3, stat, loc) ->
      setLoc(loc);
      print "for(";
      print_expression exp1 0;
      print ";";
      space ();
      print_expression exp2 0;
      print ";";
      space ();
      print_expression exp3 0;
      print ")";
      print_substatement stat
  | BREAK (loc)->
      setLoc(loc);
      print "break;"; new_line ()
  | CONTINUE (loc) ->
      setLoc(loc);
      print "continue;"; new_line ()
  | RETURN (exp, loc) ->
      setLoc(loc);
      print "return";
      if exp = NOTHING
      then ()
      else begin
	print " ";
	print_expression exp 1
      end;
      print ";";
      new_line ()
  | SWITCH (exp, stat, loc) ->
      setLoc(loc);
      print "switch(";
      print_expression exp 0;
      print ")";
      print_substatement stat
  | CASE (exp, stat, loc) ->
      setLoc(loc);
      unindent ();
      print "case ";
      print_expression exp 1;
      print ":";
      indent ();
      print_substatement stat
  | CASERANGE (expl, exph, stat, loc) ->
      setLoc(loc);
      unindent ();
      print "case ";
      print_expression expl 1;
      print " ... ";
      print_expression exph 1;
      print ":";
      indent ();
      print_substatement stat
  | DEFAULT (stat, loc) ->
      setLoc(loc);
      unindent ();
      print "default :";
      indent ();
      print_substatement stat
  | LABEL (name, stat, loc) ->
      setLoc(loc);
      print (name ^ ":");
      space ();
      print_substatement stat
  | GOTO (name, loc) ->
      setLoc(loc);
      print ("goto " ^ name ^ ";");
      new_line ()
  | ASM (tlist, isvol, outs, ins, clobs, loc) ->
      setLoc(loc);
      let print_asm_operand (cnstr, e) =
        print_string cnstr; space (); print_expression e 100
      in
      if !msvcMode then begin
        print "__asm {";
        print_list (fun () -> new_line()) print tlist; (* templates *)
        print "};"
      end else begin
        print "__asm__ "; if isvol then print "__volatile__ ";
        print "(";
        print_list (fun () -> new_line()) print_string tlist;(* templates *)
          print ":"; space ();
        print_commas false print_asm_operand outs;
        print ":"; space ();
        print_commas false print_asm_operand ins;
        if clobs <> [] then begin
          print ":"; space ();
          print_commas false print_string clobs
        end;                                
        print ");"
      end

and print_block (labs: string list) (blk: body) = 
  new_line();
  print "{";
  indent ();
  if labs <> [] then begin
    print "__label__ ";
    print_commas false print labs;
    print ";";
    new_line ();
  end;
  let printBlkElem = function
      BDEF d -> print_def d
    | BSTM s -> print_statement s
  in
  List.iter printBlkElem blk;
  unindent ();
  print "}";
  new_line ()
  
and print_substatement stat =
  match stat with
    IF _
  | SEQUENCE _
  | DOWHILE _ ->
      new_line ();
      print "{";
      indent ();
      print_statement stat;
      unindent ();
      print "}";
      new_line ();
  | BLOCK _ ->
      print_statement stat
  | _ ->
      indent ();
      print_statement stat;
      unindent ()


(*
** GCC Attributes
*)
and print_attribute (name,args) = 
  if args = [] then print name
  else begin
    print name;
    print "("; if name = "__attribute__" then print "(";
    (match args with
      [VARIABLE "aconst"] -> print "const"
    | _ -> print_commas false (fun e -> print_expression e 1) args);
    print ")"; if name = "__attribute__" then print ")"
  end

(* Print attributes. *)
and print_attributes attrs = 
  List.iter (fun a -> print_attribute a; space ()) attrs

(*
** Declaration printing
*)
and print_defs defs =
  let prev = ref false in
  List.iter
    (fun def ->
      (match def with
	DECDEF _ -> prev := false
      | _ ->
	  if not !prev then force_new_line ();
	  prev := true);
      print_def def)
    defs

and print_def def =
  match def with
    FUNDEF (proto, body, loc) ->
      setLoc(loc);
      print_single_name proto;
      print_block [] body;
      force_new_line ();

  | DECDEF (names, loc) ->
      setLoc(loc);
      print_init_name_group names;
      print ";";
      new_line ()

  | TYPEDEF (names, loc) ->
      setLoc(loc);
      print_name_group names;
      print ";";
      new_line ();
      force_new_line ()

  | ONLYTYPEDEF (specs, loc) ->
      setLoc(loc);
      print_specifiers specs;
      print ";";
      new_line ();
      force_new_line ()

  | GLOBASM (asm, loc) ->
      setLoc(loc);
      print "__asm__ (";  print_string asm; print ");";
      new_line ();
      force_new_line ()

  | PRAGMA (a,loc) ->
      setLoc(loc);
      force_new_line ();
      print "#pragma ";
      let oldwidth = !width in
      width := 1000000;  (* Do not wrap pragmas *)
      print_expression a 1;
      width := oldwidth;
      force_new_line ()


(*  print abstrac_syntax -> ()
**		Pretty printing the given abstract syntax program.
*)
let print (result : out_channel) (defs : file) =
  out := result;
  print_defs defs

let set_tab t = tab := t
let set_width w = width := w

