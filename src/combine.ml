(* Things may need to be changed
   ATTRIBUTE --- For now, Raymond is ignoring them, and actually,
                 attributes are not printed at all.
   Struct tag --- Since it is diffcult to rename struct tag without
                  scanning ahead, I am not renaming struct tag at all
   *)

(* cprint -- pretty printer of C program from abstract syntax
**
** Project: FrontC
** File:  cprint.ml
** Version: 2.1e
** Date:  9.1.99
** Author:  Hugues Cassé
**
**  1.0   2.22.99 Hugues Cassé  First version.
**  2.0   3.18.99 Hugues Cassé  Compatible with Frontc 2.1, use of CAML
**                  pretty printer.
**  2.1   3.22.99 Hugues Cassé  More efficient custom pretty printer used.
**  2.1a  4.12.99 Hugues Cassé  Correctly handle:
**                  char *m, *m, *p; m + (n - p)
**  2.1b  4.15.99 Hugues Cassé  x + (y + z) stays x + (y + z) for
**                  keeping computation order.
**  2.1c  7.23.99 Hugues Cassé  Improvement of case and default display.
**  2.1d  8.25.99 Hugues Cassé  Rebuild escape sequences in string and
**                  characters.
**  2.1e  9.1.99  Hugues Cassé  Fix, recognize and correctly display '\0'.
*)

module E = Errormsg

open Cabs
let version = "Cprint 2.1e 9.1.99 Hugues Cassé"

(* Hash tables for combiner *)

(* Tables that used to keep track of re-definitions 
 - store definitions in tables *)
let gDefTable = Hashtbl.create 307 (* global definition *)
let fDefTable = Hashtbl.create 107 (* static global definition (file scope) *)

let gAlphaTable = Hashtbl.create 307 (* id names that have been used *)

(* Table that maps id to renamed id *)
let gMap = Hashtbl.create 107 (* global id *)
let fMap = Hashtbl.create 107 (* file scope id *)
let lMap = Hashtbl.create 107 (* function scope id *)

let gTagUsedTable = Hashtbl.create 107 (* keep track tag names that have been used *)
let fTag = Hashtbl.create 107 (* tags that have appeared in file *)
let gTag = Hashtbl.create 107 (* tags that have appeared in ALL files *)

let structTag = Hashtbl.create 107
let unionTag = Hashtbl.create 107

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

let new_line _ =
  commit ();
  if !line <> "" then begin
    flush ();
    output_char !out '\n'
  end;
  follow := 0

let force_new_line _ =
  commit ();
  flush ();
  output_char !out '\n';
  follow := 0

let indent _ =
  new_line ();
  spaces := !spaces + !tab;
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
    output_char !out '\n';
    if !follow = 0 then follow := !tab
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
  let conv value = String.make 1 (Char.chr (value + 
      (if value < 10 then (Char.code '0') else (Char.code 'a' - 10)))) in
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
let get_sign si =
  match si with
  NO_SIGN -> ""
  | SIGNED -> "signed "
  | UNSIGNED -> "unsigned "

let get_size siz =
  match siz with
      NO_SIZE -> ""
        | CHAR -> "char "
  | SHORT -> "short "
  | LONG -> "long "
  | LONG_LONG -> "long long "

let rec print_base_type  typ =
  match typ with
    NO_TYPE -> ()
  | VOID -> print "void"
  | INT (size, sign) -> 
      print ((get_sign sign) ^ (get_size size) ^ 
             (if size <> CHAR then "int" else ""))
  | BITFIELD (bt, _) -> print_base_type bt
  | FLOAT size -> print ((if size then "long " else "") ^ "float")
  | DOUBLE size -> print ((if size then "long " else "") ^ "double")
  | NAMED_TYPE id -> print (lookup_id id)
  | ENUM id -> print ("enum " ^ (lookup_tag id))
  | ENUMDEF (id, items) -> print_enum (lookup_tag id) items

  | STRUCT id -> print ("struct " ^ (lookup_tag id))
  | STRUCTDEF (id, flds) ->
      if flds = [] then print "struct { }" 
      else print_fields ("struct " ^ (lookup_tag id)) flds

  | UNION id -> print ("union " ^ (lookup_tag id))
  | UNIONDEF (id, flds) -> 
      if flds = [] then print "union { }" 
      else print_fields ("union " ^ (lookup_tag id)) flds

  | PROTO (typ, _, _, _) -> print_base_type typ
  | OLD_PROTO (typ, _, _, _) -> print_base_type typ
  | PTR typ -> print_base_type  typ
  | ARRAY (typ, _) -> print_base_type  typ
(*
  | CONST typ -> print_base_type  typ
  | VOLATILE typ -> print_base_type  typ
*)
  | ATTRTYPE (typ, _) -> print_base_type typ

  | TYPEOF e -> print "__typeof__("; print_expression e 1; print ")"

(* keep adding "_" until we find one that has not been used *)
and find_newTag tag = begin
  let tmp_tag = ref "" and flag = ref true in
  begin
    tmp_tag := tag;
    while !flag do
      (try 
         Hashtbl.find gTagUsedTable !tmp_tag;
         tmp_tag := (!tmp_tag ^ "_");
       with Not_found -> flag := false;)
    done;
    Hashtbl.add gTagUsedTable !tmp_tag tag;
    !tmp_tag
  end
end

(* return empty string if tag begins with __anon
   lookup from hashtable otherwise  *)
and lookup_tag tag = begin
  if ((String.length tag) >= 6 && (String.sub tag 0 6) = "__anon") then 
    ((*prerr_endline "Found __anon tag";*) "")
  else
    try
      Hashtbl.find fTag tag
    with Not_found ->
      (try
         Hashtbl.find gTag tag
       with Not_found -> (* tag has not been used at all *)
         (Hashtbl.add gTag tag tag;
          Hashtbl.add fTag tag tag;
          tag))
  
end

(* declare a tag. NOTE: this is not being used at all right now *)
and declare_tag tag = begin
   prerr_endline ("declare tag: " ^ tag);
   try
     ignore(Hashtbl.find fTag tag)
   with Not_found ->
     let newTag = find_newTag tag in 
     begin
       Hashtbl.add fTag tag newTag;
       Hashtbl.add gTag tag newTag;
     end     
end

(* We dont want to rename fields, so add them to local and remove them right away. 
   We need to rename TAG if necessary *)  
and print_fields  id (flds : name_group list) = begin
  (*declare_tag id;*)
 
  print id;
  if flds = [] then ()
  else begin
    print " {";
    indent ();
    let ids = List.flatten (List.map (fun name_group -> 
                match name_group with (typ, sto, names) -> 
                (List.map (fun (id, typ, attr, exp) -> id) names)) flds) in
    begin
      List.iter (fun id -> Hashtbl.add lMap id id) ids;
      List.iter (fun fld -> print_name_group fld; print ";"; new_line ())
      flds;
      List.iter (fun id -> Hashtbl.remove lMap id) ids;
    end;
    unindent ();
    print "}"
  end
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
    print "}";
  end


(*
** Declaration Printing 
*)
and get_base_type typ =
  match typ with
    PTR typ -> get_base_type typ
(*
  | CONST typ -> get_base_type typ
  | VOLATILE typ -> get_base_type typ
*)
  | ATTRTYPE (typ, _) -> get_base_type typ
  | ARRAY (typ, _) -> get_base_type typ
  | _ -> typ
  
and print_pointer typ =
  match typ with
    PTR typ -> print_pointer typ; print "*"
(*
  | CONST typ -> print_pointer typ; print " const "
  | VOLATILE typ -> print_pointer typ; print " volatile "
*)
  | ATTRTYPE (typ, a) -> begin 
      print_pointer typ;
        (* Extract the const and volatile attributes *)
      let rec doconstvol = function
          [] -> []
        | ("const", []) :: rest -> print " const "; doconstvol rest
        | ("volatile", []) :: rest -> print " volatile "; doconstvol rest
        | a :: rest -> a :: doconstvol rest
      in
      let rest = doconstvol a in
      if rest <> [] then begin
        print " __attribute__((";
        let printOne (s, el) =
          print s;
          if el <> [] then
            print_commas false (fun e -> print_expression e 1) el
        in
        print_commas false printOne rest;
        print ")) "
      end
  end

  | ARRAY (typ, _) -> print_pointer typ
  | _ -> ()
        
and print_array typ =
  match typ with
    ARRAY (typ, dim) ->
      print_array typ; 
      print "[";
      print_expression dim 0;
      print "]"
  | _ -> ()

and print_type (fct : unit -> unit) (typ : base_type ) =
  let base = get_base_type typ in
  match base with
    BITFIELD (_, exp) -> fct (); print " : "; print_expression exp 1
  | PROTO (typ', pars, ell, _) ->
      print_type
  (fun _ ->
    if base <> typ then print "(";
    print_pointer typ;
    fct ();
    print_array typ;
    if base <> typ then print ")";
    print "(";
    print_params pars ell;
    print ")")
  typ'
  
  | OLD_PROTO (typ', pars, ell, _) ->
      print_type
  (fun _ ->
    if base <> typ then print "(";
    print_pointer typ;
    fct ();
    print_array typ;
    if base <> typ then print ")";
    print "(";
    print_old_params pars ell;
    print ")")
  typ'
  | _ -> print_pointer typ; fct (); print_array typ
        
and print_onlytype typ =
  print_base_type typ;
  print_type (fun _ -> ()) typ

(* Raymond added lookup_id here *)    
and print_name ((id, typ, attr, exp) : name) =
  if id = "___missing_field_name" then () 
  else begin
    print_type (fun _ -> print (lookup_id id)) typ;
    print_attributes attr;
    if exp <> NOTHING then begin
      space ();
      print "= ";
      print_expression exp 1
    end else ()
  end
      
and get_storage sto =
  match sto with
    NO_STORAGE -> ""
  | AUTO -> "auto"
  | STATIC i -> "static" ^ (if i then " __inline__" else "")
  | EXTERN i -> "extern" ^ (if i then " __inline__" else "")
  | INLINE -> "__inline__"
  | REGISTER -> "register"
        
and print_name_group (typ, sto, names) =
  if sto <> NO_STORAGE then begin
    print (get_storage sto);
    space ()
  end;
  print_base_type typ;
  space ();
  print_commas false print_name names
    
and print_single_name (typ, sto, name) =
  if sto <> NO_STORAGE then begin
    print (get_storage sto);
    space ()
  end;
  print_base_type typ;
  space ();
  print_name name

(* Raymond added declare_id lookup_id *)
and print_params (pars : single_name list) (ell : bool) =
  List.iter (fun name -> declare_id name false) pars;
  print_commas false print_single_name pars;
  if ell then print (if pars = [] then "..." else ", ...") else ()

(* Raymond added declare_id and lookup_id here *)    
and print_old_params pars ell =
  List.iter (fun id -> declare_id (NO_TYPE, NO_STORAGE, (id, NO_TYPE, [], NOTHING)) false) pars;
  print_commas false (fun id -> print (lookup_id id)) pars;
  if ell then print (if pars = [] then "..." else ", ...") else ()
    

(*
** Expression printing
**    Priorities
**    16  varaibles
**    15  . -> [] call()
**    14  ++, -- (post)
**    13  ++ -- (pre) ~ ! - + & *(cast)
**    12  * / %
**    11  + -
**    10  << >>
**    9 < <= > >=
**    8 == !=
**    7 &
**    6 ^
**    5 |
**    4 &&
**    3 ||
**    2 ? :
**    1 = ?=
**    0 ,       
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
  | TYPE_SIZEOF typ -> ("", 16)
  | INDEX (exp, idx) -> ("", 15)
  | MEMBEROF (exp, fld) -> ("", 15)
  | MEMBEROFPTR (exp, fld) -> ("", 15)
  | GNU_BODY _ -> ("", 17)
        
and print_comma_exps exps =
  print_commas false (fun exp -> print_expression exp 1) exps
    
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
    print txt;
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
      | CONST_STRING s -> print_string s
      | CONST_COMPOUND initexps ->
          let doinitexp = function
              NO_INIT, e -> print_expression e 1
            | i, e -> 
                let rec doinit = function
                    NO_INIT -> ()
                  | FIELD_INIT (fn, i) -> print ("." ^ fn); doinit i
                  | INDEX_INIT (e, i) -> 
                      print "[";
                      print_expression e 1;
                      print "]";
                      doinit i
                in
                doinit i; print " = "; 
                print_expression e 1
          in
    print "{";
          print_commas false doinitexp initexps;
    print "}")

  | VARIABLE name -> (* Raymond changed the code to lookup_id *)
      print (lookup_id name)
  | EXPR_SIZEOF exp ->
      print "sizeof(";
      print_expression exp 0;
      print ")"
  | TYPE_SIZEOF typ ->
      print "sizeof(";
      print_onlytype typ;
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
  | GNU_BODY blk ->
      print "(";
      print_statement (BLOCK blk);
      print ")" in
  if lvl > lvl' then print ")" else ()
    

(*
** Statement printing
*)
and print_statement stat =
  match stat with
    NOP ->
      print ";";
      new_line ()
  | COMPUTATION exp ->
      print_expression exp 0;
      print ";";
      new_line ()
  | BLOCK blk ->
      new_line ();
      print "{";
      indent ();
      let printBlkElem = function
          BDEF d -> print_def d false
        | BSTM s -> print_statement s
      in
      List.iter printBlkElem blk;
      unindent ();
      print "}";
      new_line ();
  | SEQUENCE (s1, s2) ->
      print_statement s1;
      print_statement s2;
  | IF (exp, s1, s2) ->
      print "if(";
      print_expression exp 0;
      print ")";
      print_substatement s1;
      if s2 = NOP
      then ()
      else begin
  print "else";
  print_substatement s2;
      end
  | WHILE (exp, stat) ->
      print "while(";
      print_expression exp 0;
      print ")";
      print_substatement stat
  | DOWHILE (exp, stat) ->
      print "do";
      print_substatement stat;
      print "while(";
      print_expression exp 0;
      print ");";
      new_line ();
  | FOR (exp1, exp2, exp3, stat) ->
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
  | BREAK ->
      print "break;"; new_line ()
  | CONTINUE ->
      print "continue;"; new_line ()
  | RETURN exp ->
      print "return";
      if exp = NOTHING
      then ()
      else begin
  print " ";
  print_expression exp 1
      end;
      print ";";
      new_line ()
  | SWITCH (exp, stat) ->
      print "switch(";
      print_expression exp 0;
      print ")";
      print_substatement stat
  | CASE (exp, stat) ->
      unindent ();
      print "case ";
      print_expression exp 1;
      print ":";
      indent ();
      print_substatement stat
  | DEFAULT stat ->
      unindent ();
      print "default :";
      indent ();
      print_substatement stat
  | LABEL (name, stat) ->
      print (name ^ ":");
      space ();
      print_substatement stat
  | GOTO name ->
      print ("goto " ^ name ^ ";");
      new_line ()
  | ASM (tlist, isvol, outs, ins, clobs) ->
      let print_asm_operand (cnstr, e) = 
        print_string cnstr; space (); print_expression e 100
      in
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
    print "(";
    print_commas false (fun e -> print_expression e 1) args;
    print ")"
  end

(* DO NOT PRINT OUT ANY ATTRIBUTE FOR NOW *)
and print_attributes attrs = 
  ();  
(*  if attrs = [] then ()
  else
    begin
      space (); print "__attribute__((";
      print_commas false print_attribute attrs;
      print "))"
    end
*)

(*
** Declaration printing
*)
and print_defs defs global = begin

  let prev = ref false in
  List.iter
    (fun def ->
      (match def with
  DECDEF _ -> prev := false
      | _ ->
    if not !prev then force_new_line ();
    prev := true);
      print_def def global)
    defs
end

(* A set of remove_anon functions to remove _anon from 
   TYPEDEF and ONLYTYPEDEF for the purposes of checking
   duplicate definition.
   If adding tag _anon is removed from Cparser, then we don't
   need these functions *)

and remove_anon_name_group (typ, sto, names) = begin
  (remove_anon_type typ, sto, List.map remove_anon_name names)
end

and remove_anon_id id = begin
  
  if (((String.length id) >= 6) && ((String.sub id 0 6) = "__anon")) then
    ""
  else
    id
end

and remove_anon_type typ = begin
  match typ with
    STRUCTDEF (id, flds) ->
      STRUCTDEF (remove_anon_id id,
                 List.map remove_anon_name_group flds)
  | UNIONDEF (id, flds) ->
      UNIONDEF (remove_anon_id id,
              List.map remove_anon_name_group flds)
  | ENUMDEF (id, items) -> (* Occur in onlytypedef, not typedef *)
      ENUMDEF (remove_anon_id id, items)
  | _ -> typ
end    

and remove_anon_name (id, typ, attr, exp) = begin
  (id, remove_anon_type typ, attr, exp)
end  
      
and remove_anon def = begin
  match def with
    TYPEDEF (typ, sto, names) ->
      TYPEDEF (remove_anon_type typ, sto, List.map remove_anon_name names)
  | ONLYTYPEDEF (typ, sto, names) ->
      ONLYTYPEDEF (remove_anon_type typ, sto, List.map remove_anon_name names)
  | _ -> def
 
end        

and tag_defined (typ, sto, names) = begin
  match typ with
    STRUCTDEF (id, _) ->
      begin
      if id = "" then false
      else
      (try
         ignore(Hashtbl.find structTag id);
         true
       with Not_found ->
         (Hashtbl.add structTag id id;
          false))
      end
  | UNIONDEF (id, _) ->
      if id = "" then false
      else 
      (try
         ignore(Hashtbl.find unionTag id);
         true
       with Not_found ->
         (Hashtbl.add unionTag id id;
          false))
  | _ -> false
end

(* returns true check if def is not declared
   Could make another function to clean this code up *)
and already_declared def = begin
  match def with
    FUNDEF ((typ, sto, name), body) ->
      (match sto with
        STATIC i ->
          (try 
            Hashtbl.find fDefTable def;
            true
          with Not_found ->
              (Hashtbl.add fDefTable def 1;
              false))            
      | _ -> 
          (try
            Hashtbl.find gDefTable def;
            true
          with Not_found -> 
              (Hashtbl.add gDefTable def 1;
              false)))
              
  | OLDFUNDEF ((typ, sto, name), decs, body) ->
      (match sto with
        STATIC i ->
          (try 
            ignore(Hashtbl.find fDefTable def);
            true
          with Not_found ->
              (Hashtbl.add fDefTable def 1;
              false))
      | _ -> 
          (try
            ignore(Hashtbl.find gDefTable def);
            true
          with Not_found ->
              (Hashtbl.add gDefTable def 1;
              false)))
  
  | DECDEF (typ, sto, names) ->
      (match sto with
        STATIC i ->
          (try 
            ignore(Hashtbl.find fDefTable def);
            true
          with Not_found ->
              (Hashtbl.add fDefTable def 1;
              false))
      | _ -> 
          (try
            ignore(Hashtbl.find gDefTable def);
            true
          with Not_found -> 
              (Hashtbl.add gDefTable def 1;
              false)))
              
  | TYPEDEF names ->
      if tag_defined names then
        true
      else
        (try
          ignore(Hashtbl.find gDefTable def);
          true
        with Not_found ->
            (Hashtbl.add gDefTable def 1;
            false))
          
  | ONLYTYPEDEF names ->
      if tag_defined names then
        true
      else 
        (try
          ignore(Hashtbl.find gDefTable def);
          true
        with Not_found ->
            (Hashtbl.add gDefTable def 1;
            false))
      
  | _ -> false (* ASM and others are always considered to be false *)
end

(* Find a id name that hasn't been used *)
and find_newId id = begin
  let tmp_id = ref "" and flag = ref true in 
  begin
    tmp_id := id;
    while !flag do
      try
        Hashtbl.find gAlphaTable !tmp_id;
        tmp_id := (!tmp_id ^ "_");
      with Not_found -> flag := false;
    done;
    !tmp_id
  end
end

(* declare an id and add that to the mapping table *)
and declare_id (typ, sto, (id, typ', attr, exp)) global = begin
  (*prerr_endline ("declare id: " ^ id);*)
  if global then 
    begin
      match sto with
        STATIC i ->
          (try
             Hashtbl.find fMap id; ();
           with Not_found -> (* new declaration *)
             (try
                begin
                  Hashtbl.find gAlphaTable id;
                  (* needs to be renamed *)
                  let newId = find_newId id in 
                  begin
                    Hashtbl.add gAlphaTable newId id;
                    Hashtbl.add fMap id newId;
                  end
                end
                with Not_found ->
                  (Hashtbl.add gAlphaTable id id;
                   Hashtbl.add fMap id id;)))
      | _ -> 
          (try 
             Hashtbl.find gMap id; ();
           with Not_found -> (* new declaration *)
             (try
                ignore(Hashtbl.find gAlphaTable id);
                let newId = find_newId id in
                begin
                  Hashtbl.add gAlphaTable newId id;
                  Hashtbl.add gMap id newId;
                end
                with Not_found ->
                  (Hashtbl.add gAlphaTable id id;
                   Hashtbl.add gMap id id;)))
    end
  else
    begin (* try to reuse the names as many times as possible *)
      try 
        ignore(Hashtbl.find lMap id);
      with Not_found -> 
        (try
           ignore(Hashtbl.find fMap id);
         with Not_found ->
           (try
              ignore(Hashtbl.find gMap id);
            with Not_found ->
              (try
                 ignore(Hashtbl.find gAlphaTable id);
                 (* needs to be renamed *)
                 Hashtbl.add lMap id (find_newId id);
               with Not_found ->
                 Hashtbl.add lMap id id;)))
    end
end

(* declare_ids are used for a name_group. 
   For simplicity, it is calling declare_id *)  
and declare_ids (typ, sto, names) global = begin      
  List.iter (fun name -> declare_id (typ, sto, name) global) names
end

(* if global, we do a check on duplicate definition.
   Also, declare id (variable, function, and type) *)
and print_def def global = begin
  
  (* clear function scope mapping table if this is a global def *)
  if global then 
    Hashtbl.clear lMap; 
  
  if global && (already_declared (remove_anon def)) then 
    begin
      ()
    end
  else
    begin
  
    match def with
      FUNDEF (name, body) ->
        declare_id name global;
        print_single_name name;
        print_statement (BLOCK body);
        force_new_line ();
        
    | OLDFUNDEF (name, decs, body) ->
        declare_id name global;
        print_single_name name;
        force_new_line ();
        List.iter (fun dec -> print_name_group dec; print ";";
        new_line ()) decs;
        print_statement (BLOCK body);
        force_new_line ();
       
    | DECDEF names ->
        declare_ids names global;
        print_name_group names;
        print ";";
        new_line ()
       
    | TYPEDEF (typ, sto, names) ->
        declare_ids (typ, STATIC true, names) global;
        print "typedef ";
        print_name_group (typ, sto, names);
        print ";";
        new_line ();
        force_new_line ()
      
    | ONLYTYPEDEF (typ, sto, names) ->
        declare_ids (typ, STATIC true, names) global;
        print_name_group (typ, sto, names);
        print ";";
        new_line ();
        force_new_line ()
        
    | GLOBASM asm -> 
        print "__asm__ (";  print_string asm; print ");";
        new_line ();
        force_new_line ()
(*
    | PRAGMA (s, el) -> 
        force_new_line ();
        print "#pragma "; print_string s;
        if el <> [] then begin
          print "("; 
          print_commas false (fun e -> print_expression e 1) el;
          print ")"
        end;
        force_new_line ()
*)
    | PRAGMA a -> 
        force_new_line ();
        print "#pragma ";
        print_attribute a;
        force_new_line ()
end
end

(* look up id from Mapping tables *)
and lookup_id id = begin
  try
    Hashtbl.find lMap id
  with Not_found ->
    try
      Hashtbl.find fMap id
    with Not_found ->
      try
        Hashtbl.find gMap id
      with Not_found ->
        (*prerr_endline ("Undeclared id: " ^ id);*)
        id
end
        
(*  print abstrac_syntax -> ()
**    Pretty printing the given abstract syntax program.
*)
let print (result : out_channel) (defs : file) = begin

  (* clear file scope tables *)
  Hashtbl.clear fDefTable;
  Hashtbl.clear fMap;
  Hashtbl.clear fTag; 

  out := result;
  print_defs defs true (* global definition *)
end

let set_tab t = tab := t
let set_width w = width := w

let combine (files: string list) 
            (out: string) =
  Hashtbl.add gAlphaTable "" "";
  Hashtbl.add gMap "" "";
  Hashtbl.add fMap "" "";
  Hashtbl.add lMap "" "";
  
  let list_of_parsed_files =
    List.map (fun file_name -> Frontc.parse_to_cabs file_name) files in
  
  let outchan = 
    try open_out out with e -> E.s (E.error "Cannot open output file %s" out)
  in
  List.iter (fun file -> print outchan file) list_of_parsed_files;
  close_out outchan

      
