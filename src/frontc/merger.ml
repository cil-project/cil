(* Implementation of whole-program merging *)

open Cabs
open Cabsvisitor
open Pretty
module E = Errormsg
module H = Hashtbl


(* Global names are split into a prefix, a ___ (3 underscores) separator and 
 * an integer suffix. For each prefix we keep the maximum integer suffix with 
 * which the prefix was encountered. This suffix is -1 when only the version 
 * without the suffix was encountered.  *)
let alphaTable: (string, int ref) H.t = H.create 511


(* To keep different name scopes different, we add prefixes to names 
 * specifying the kind of name: the kind can be one of "" for variables or 
 * enum tags, "struct" for structures, "enum" for enumerations, "union" for 
 * union types, or "type" for types *)
let kindPlusName (kind: string)
                 (origname: string) : string =
  if kind = "" then origname else
  kind ^ " " ^ origname
                

let stripKind (kind: string) (kindplusname: string) : string = 
  let l = 1 + String.length kind in
  if l > 1 then 
    String.sub kindplusname l (String.length kindplusname - l)
  else
    kindplusname

(* Keep track of the current location *)
let currentLoc: cabsloc ref = ref { lineno = -1; filename = "<no file>" }


(***** Collect the globals in all of the files *********)
let globals : (string, bool) H.t = H.create 1111
let collectGlobals (fl: file list) = 
  let collectOneGobals = function
      FUNDEF ((specs, (n, decl, attrs)), _, _) -> 
        let islocal = isStatic specs || isInline specs in
        if not islocal then H.add globals n
        
    | DECDEF ((specs, inl), _) -> 
        let islocal = isStatic specs in
        if not islocal then 
          List.iter (fun ((n, _, _), _) -> H.add globals n) nl

    | _ -> ()
  in
  H.clear globals;
  List.iter (fun f -> List.iter collectOneGlobal f) fl


type nameKind = 
    NType                               (* A typedef *)
  | NStruct
  | NUnion
  | NEnum
  | NVar

let processDeclaration
    (orignames: string list) (* The names as they appear in the original 
                              * definition *)
    (filescope: bool) (* Whether this is a file-scope identifier *)
    (kind: nameKind) (* The kind of the names being declared *)
    (def: 'a)        (* The definition *)
    (alpha: 'a -> 'a) (* Apply the alpha-conversion *)
    (lookup: 'a -> string list option) (* Lookup a definition are return the 
                                        * names with which it appears *)
    : 'a option (* return whether we should keep a definition or not *)
    = 
  if not filescope then 
    (* Check to see if we have already seen this one *)
    let def' = alpha def in
    match lookup def' with
      None -> Some def'
    | Some [] -> None
    | Some _ -> E.s (E.bug "processDeclaration: global was renamed")
  else begin
    (* It is a file scope *)
    try
      let 
    with Not_found -> 

  end
    


(*** Apply alpha-conversion to a definition ****)

class alphaConversionClass : cabsVisitor
  inherit nopCabsVisitor

  method vvar s = try H.find env s with Not_found -> s

        (* Keep track of scopes *)
  method vEnterScope () = enterScope ()
  method vExitScope () = exitScope ()

        (* Declared names *)
  method vname (sn, _, _) = 
    addToEnv (if "" sn; (* This is a local name *)
    DoChildren

        (* And declared structure, union and enumeration tags *)
  method vtypespec ts = 
    try
      match ts with 
        Tstruct (n, Some _) -> addToEnv "struct " n; DoChildren
      | Tstruct (n, None) -> 
          let n' = lookupEnv "struct " n in
          if n' != n then ChangeTo (Tstruct (n', None)) else ts
      | Tunion (n, Some _) -> addToEnv "union " n; DoChildren
      | Tunion (n, None) -> 
          let n' = lookupEnv "union " n in
          if n' != n then ChangeTo (Tunion (n', None)) else ts
      | Tenum (n, Some _) -> addToEnv "enum " n; DoChildren
      | Tenum (n, None) -> 
          let n' = lookupEnv "enum " n in
          if n' != n then ChangeTo (Tenum (n', None)) else ts
      | Tnamed n -> 
          let n' = lookupEnv "type " n in
          if n' != n then ChangeTo (Tnamed n') else ts

      | _ -> DoChildren
    with Not_found -> DoChildren
end
