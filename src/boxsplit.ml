open Cil
open Pretty
open Trace

module H = Hashtbl
module E = Errormsg

(* Go over the code and split fat temporary variables into several separate 
 * variables. The hope is that the compiler will have an easier time to do 
 * standard optimizations with the resulting scalars *)

(* Map a variable name to a list of component variables, along with the 
 * accessor field name *)
let newvars : (string, (fieldinfo * varinfo) list) H.t = H.create 13
let splitVar (func: fundec) 
             (v: varinfo) 
             (fstfld: fieldinfo) 
             (restflds: fieldinfo list) = 
  (* Update the type of the variable *)
  v.vtype <- fstfld.ftype;
  H.add newvars v.vname 
    ((List.map (fun f -> (f, 
                         makeTempVar func ~name:v.vname f.ftype)) restflds)
       (* It is important to put the first field last, so that it is updated 
        * last in an assignment *)
       @ [(fstfld, v)])

(* May raise Not_found if the variable does not have components *)
let getVarComponent (vname: string)
                    (fname: string) : varinfo = 
  match List.filter (fun (fn, compvar) -> fn.fname = fname) 
                    (H.find newvars vname) with
    [ (_, compvar) ] -> compvar
  | _ -> E.s (E.bug "Cannot find component %s of %s\n" fname vname)




(* A visitor that finds all locals that appear in a call or have their 
 * address taken *)

let dontSplit : (string, bool) H.t = H.create 111
class findVarsToSplitClass : cilVisitor = object (self) 
  inherit nopCilVisitor
        
        (* expressions, to see the address being taken *)
  method vexpr (e: exp) : exp visitAction =
    match e with 
      AddrOf (Var v, NoOffset) -> H.add dontSplit v.vname true; SkipChildren
    | _ -> DoChildren

          (* Sometimes the addrof is taken gratuituously 
  method vlval (lv: lval) : lval visitAction = 
    match lv with 
      (* Do just the offsets *)
      Mem (AddrOf (Var v, off1)), off2 -> 
        ignore (visitCilOffset (self :> cilVisitor) off1);
        ignore (visitCilOffset (self :> cilVisitor) off2);
        SkipChildren
    | _ -> DoChildren
        *)
          (* variables involved in call instructions *)
  method vinst (i: instr) : instr list visitAction = 
    match i with 
      Call (res, f, args, _) -> 
        (match res with 
          Some (Var v, NoOffset) -> H.add dontSplit v.vname true
        | _ -> ());
        List.iter (fun a -> 
          match a with
            Lval (Var v, NoOffset) -> H.add dontSplit v.vname true
          | _ -> ()) args;
        (* Now continue the visit *)
        DoChildren
    | _ -> DoChildren

          (* Variables used in return should not be split *)
  method vstmt (s: stmt) : stmt visitAction = 
    match s.skind with 
      Return (Some (Lval (Var v, NoOffset)), _) -> 
        H.add dontSplit v.vname true; DoChildren
    | Return (Some e, _) -> 
        DoChildren
    | _ -> DoChildren

end
let findVarsToSplit = new findVarsToSplitClass


class splitVarVisitorClass : cilVisitor = object (self)
  inherit nopCilVisitor

      (* Whenever we see a variable with a field access we try to replace it 
       * by its components *)
  method vlval ((b, off) : lval) : lval visitAction = 
    try
      match b, off with
        Var v, Field (fi, restoff) -> 
          ChangeTo (Var (getVarComponent v.vname fi.fname), restoff)
      | _ -> DoChildren
    with Not_found -> DoChildren
        
        (* Sometimes we pass the variable as a whole to a function or we 
         * assign it to something *)
  method vinst (i: instr) : instr list visitAction = 
    match i with
      (* Split into two instructions and then do children. Howver, v._p might 
       * appear in the lv and if we duplicate the instruction we might get 
       * bad results. To fix this problem we make sure that the update to _p 
       * happens last. Other fields of v should not appear in lv. *)
      Set ((Var v, NoOffset), Lval lv, l) when H.mem newvars v.vname -> 
        ChangeTo 
          (List.map 
             (fun (fld, newv) -> 
               let lv' = 
                 visitCilLval (self :> cilVisitor)
                   (addOffsetLval (Field(fld, NoOffset)) lv) in
               Set((Var newv, NoOffset), Lval lv', l))
             (H.find newvars v.vname))

      | Set (lv, Lval (Var v, NoOffset), l) when H.mem newvars v.vname ->
          ChangeTo 
            (List.map 
               (fun (fld, newv) -> 
                 let lv' = 
                   visitCilLval (self :> cilVisitor)
                     (addOffsetLval (Field(fld, NoOffset)) lv) in
                 Set(lv', Lval (Var newv, NoOffset), l))
               (H.find newvars v.vname))

      | _ -> DoChildren

end

let splitVarVisitor = new splitVarVisitorClass

let splitLocals (func: fundec) : unit = 
  (* Go over the locals and find the candidates *)
  ignore (visitCilBlock findVarsToSplit func.sbody);
  (* Now go over the locals and create the splits *)
  List.iter 
    (fun loc -> 
      if not (H.mem dontSplit loc.vname) then begin
        match unrollType loc.vtype with
          TComp (comp, _) when comp.cstruct -> begin 
            match comp.cfields with
              ({fname="_p"} as fstfld) :: restflds -> begin
                splitVar func loc fstfld restflds
              end
            | _ -> ()
          end
        | _ -> ()
      end)
    func.slocals;
  (* Now visit the body and change references to these variables *)
  visitCilFunction splitVarVisitor func;
  H.clear newvars;
  H.clear dontSplit
  

