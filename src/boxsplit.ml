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
  ignore (E.log "***Spliting variable %s\n" v.vname);
  (* Update the type of the variable *)
  v.vtype <- fstfld.ftype;
  H.add newvars v.vname 
    ((fstfld, v) ::
     List.map (fun f -> (f, 
                         makeTempVar func ~name:v.vname f.ftype)) restflds)

(* May raise Not_found if the variable does not have components *)
let getVarComponent (vname: string)
                    (fname: string) : varinfo = 
  match List.filter (fun (fn, compvar) -> fn.fname = fname) 
                    (H.find newvars vname) with
    [ (_, compvar) ] -> compvar
  | _ -> E.s (E.bug "Cannot find component %s of %s\n" fname vname)

let doprint = ref false
class splitVarVisitorClass = object (self)
  inherit nopCilVisitor

      (* Whenever we see a variable with a field access we try to replace it 
       * by its components *)
  method vlval ((b, off) : lval) : lval visitAction = 
    try
      match b, off with
        Var v, Field (fi, restoff) -> 
          if !doprint then 
            ignore (E.log "**vlval v=%s, fi=%s\n" v.vname fi.fname);
          ChangeTo (Var (getVarComponent v.vname fi.fname), restoff)
      | _ -> DoChildren
    with Not_found -> DoChildren
        
        (* Sometimes we pass the variable as a whole to a function or we 
         * assign it to something *)
  method vinst (i: instr) : instr list visitAction = 
    match i with
      (* Split into two instructions and then do children *)
      Set ((Var v, NoOffset), Lval lv, l) when H.mem newvars v.vname -> 
        ChangeTo 
          (List.map 
             (fun (fld, newv) -> 
               doprint := true;
               let lv' = 
                 visitCilLval (self :> cilVisitor)
                   (addOffsetLval (Field(fld, NoOffset)) lv) in
               doprint := false;
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
  ignore (E.log "***Splitting locals in %s\n" func.svar.vname);
  (* Go over the locals and find the candidates *)
  List.iter 
    (fun loc -> 
      if not loc.vaddrof then begin
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
  H.clear newvars
  

