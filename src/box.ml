open Cil

module H = Hashtbl

           (* After processing an expression, we create its type, a list of 
            * instructions that should be executed before this exp is used, 
            * a replacement exp and another exp denoting the base of a fat 
            * pointer. *)
type expRes = 
    typ * instr list * exp * exp

            (* Same for offsets *)
type offsetRes = 
    typ * instr list * offset * offset


          (* Check whether a type is represented by fat pointers *)
let hasBase = function
    TPtr _ -> true
  | TArray _ -> true
  | _ -> false

          (* A fat pointer type to be used for returning pointers *)
let fatPointerPtrField = 
  { struct_name = "__fatptr";
    field_name  = "ptr";
    ftype       = TPtr TVoid; }      
let fatPointerBaseField = 
  { struct_name = "__fatptr";
    field_name  = "base";
    ftype       = TPtr TVoid; }
let fatPointerType = 
  TStruct ("__fatptr", 
           [ fatPointerPtrField; fatPointerBaseField; ])

let zero = Const (Int (0, None))
                                        (* Apply a boxing transformation to 
                                         * an entire program *)
let boxProg (prog : program) = 
          (* Hash of bases for global variables *)
  let globBaseHash : (int, varinfo) H.t = H.new 17 in
  List.iter (fun vi ->
    if hasBase vi.vtype then 
      let bvi = newGlobalVariable prog (vi.vname ^ "__base") (TPtr TVoid) in
      H.add globBaseHash vi.vid bvi) prog.globalVars;

  (************ FUNCTIONS *******************)
  (* Apply the transformation to a function declaration *)
  let rec boxfunc (fdec : fun_decl) = 
    (* Hash of bases for local variables *)
    let localBaseHash : (int, varinfo) H.t = H.new 17 in 
    let addBaseVar vi = 
      if hasBase vi.vtype then 
        let bvi = newLocalVariable fdec (vi.vname ^ "__base") (TPtr TVoid) in
        H.add localBaseHash vi.vid bvi 
    in
    (* Create the bases for the local variables *)
    List.iter addBaseVar fdec.fformals;
    List.iter addBaseVar fdec.flocals;

    (* A way to get the base variable, as an lval *)
    let getBaseVar vi = 
      try 
        if vi.isglobal then 
          H.find globBaseHash vi.vid
        else
          H.find localBaseHash vi.vid
      with Not_found -> ignore (bug "I thought this has a companion")
    in

    (* A way to create temporary variables *)
    let newTempVar typ = 
      let vi = newTempVariable fdec typ in
      addBaseVar vi;
      vi
    in

    (************ EXPRESSIONS **************)
    let rec boxexp (e : exp) : expRes = 
      match e with 
        Const ((Int _ | Chr _), _) -> (TInt (IInt, []), [], e, zero)
      | Const (Real _, _) -> (TFloat (FDouble, []), [], e, zero)
      | Const (Str s, l) -> 
          (* Add a tag to the string *)
          let l = 1 + String.length s in 
          let wrds = (l + 3) lsr 2 in   (* Number of words *)
          let tagbytes = wrds lsr 2 in  (* Tag bytes *)
          let tag = String.create (tagbytes + 4 + l) in
          (* Zero out the tag *)
          String.fill tag 0 tagbytes (Char.chr 0);
          (* Write the length *)
          String.set tag (tagbytes + 0) (Char.chr ((wrds)       land 255));
          String.set tag (tagbytes + 1) (Char.chr ((wrds lsr  8) land 255));
          String.set tag (tagbytes + 2) (Char.chr ((wrds lsr  16) land 255));
          String.set tag (tagbytes + 3) (Char.chr ((wrds lsr  24) land 255));
          (* Copy the original string *)
          String.blit s 0 tag (tagbytes + 4) l;
          let t1 = newTempVar fatStringType in
          let t1ptr = Var(t1, Field(fatStringPtrField, NoOffset), l) in
          let t1base = Var(t1, Field(fatStringBaseField, NoOffset), l) in
          (TPtr (TInt(IChar,[]),[]), 
           [ Set (t1ptr, Const (Str tag, l), l) ;
             Set (t1ptr, 
                  AddrOf (Var(t1, Field(fatStringPtrField, 
                                        Index(Const(Int (tagbytes + 4), l),
                                              NoOffset)))), l) ;
             Set (t1base, LVar(t1ptr), l)],
           LVal(t1ptr), LVal(t1base))

      | LVal (Var(vi, off, l)) ->       (* Reading a variable *)
          let (rest, dooff, off', off'base) = boxoffset off vi.vtype in
          (rest, 
           dooff, 
           LVal(Var(vi, off', l)), 
           if hasBase rest then LVal(Var(vi, off'base, l)) else zero)
                                        (* Reading a memory address *)
      | LVal (Mem(addr, off, l)) -> 
          let (addrt, doaddr, addr', addr'base) = boxexp addr in
          let (rest, dooff, off', off'base) = boxoffset off addrt in
          let newlval = Mem(addr', off', l) in
          let newlvalbase, check = 
            if hasBase rest then
              LVal(Mem(addr', off'base, l)), "CHECK_SAFERDFAT"
            else
              zero, "CHECK_SAFERDLEAN"
          in
          (rest, 
           doaddr @ dooff @ [Call (None, Const (Str check, l),
                                   [ AddrOf(LVal(newlval)); 
                                     addr'base ])],
           LVal(newlval), newlvalbase)

      | UnOp (uop, e, restyp, l) -> 
          let (_, doe, e', _) = boxexp e in
          (* The result is never a pointer *)
          (restyp, doe, UnOp(uop, e', l), zero)

      | BinOp (bop, e1, e2, restyp, l) =
        let (et1, doe1, e1', e1'base) = boxexp e1 in
        let (et2, doe2, e2', e2'base) = boxexp e2 in
        (restyp, doe1 @ doe2, 
         BinOp(bop, e1', e2', restyp, l),
         if hasBase restyp then
           (* If the result is a pointer then it must be that either e1 or e2 
            * are pointers but not both ! *)
           if hasBase et1 then e1'base else e2'base
         else
           zero)
        end
          
      | CastE (newt, e) = 
        let (et, doe, e', e'base) = boxexp e in
        begin
          match et, newt with 
            (TInt _|TFloat _|TDouble _|TPtr _|TArray _), 
            (TInt _|TFloat _|TDouble _) -> 
              (newt, doe, CastE(newt, e'), zero)
          | TPtr (tp, _), TPtr (newtp, _) -> 
              (newt, doe, CastE(newt, e'), e'base)
          | (TInt _|TFloat _|TDouble _), TPtr (newt', _) -> 
              (newt, doe, CastE(newt, e'), zero)
          | _ -> E.i (E.unimp "caste")
        end

      | AddrOf (Var(vi,off,l), l') -> 
          let dataField = addressOf vi in
          let (rest, dooff, off', off'base) = boxoffset off vi.vtype in
          (TPtr (rest, []), dooff, 
           AddrOf (Var(vi, Field(dataField, off'), l)), 
           AddrOf (Var(vi, Field(dataField, NoOffset), l)))

      | AddrOf (Mem(e, off, l), l') -> 
          let (et, doe, e', e'base) = boxexp e in
          let (rest, dooff, off', off'base) = boxoffset off et in
          (TPtr (rest, []), doe @ dooff, 
           AddrOf (Mem(e', off', l), l'), 
           e'base)

    (************ LVALUE OFFSETS ***********)
    and boxoffset (off : offset) (basety : typ) : offsetRes = 
      match off with 
        NoOffset -> 
          if hasBase basety then
            let ptrField, baseField = getFatPointerType basety in
            (basety, [], 
             Field (ptrField, NoOffset), 
             Field (baseField, NoOffset))
          else
            (basety, [], NoOffset, NoOffset)
             
      | Index (e, resto) ->
          let rest =                    (* The result type *)
            match basety with
              TPtr t -> t
            | TArray (t, -) -> t
            | _ -> E.i (E.bug "Index")
          in
          let (_, doe, e', _) = boxexp e in
          let (rest, doresto, off', off'base) = boxoffset resto t in
          (rest, doe @ doresto, Index(e', off'), Index(e', off'base))

      | Field (fi, resto) ->
          let (rest, doresto, off', off'base) = boxoffset resto fi.ftype in
          (rest, doresto, Field(fi, off'), Field(fi, off'base))

          

    (************* STATEMENTS **************)
    and boxstmt (s : stmt) : stmt = 
      let makeStatement il = 
        Compound (List.map (fun i -> Instruction i) il)
      in
      match s with 
        Compound sl -> Compound (List.map boxstms sl)

      | (Label _ | Jump _ | Case _ | Default) -> s

      | While (e, s) -> 
          let (et, doe, e', e'base) = boxexp e in
          (* We allow casts from pointers to integers here *)
          makeStatement (doe @ [While (e', boxstmt s)])

      | IfThenElse (e, st, sf) -> 
          let (et, doe, e', e'base) = boxexp e in
          (* We allow casts from pointers to integers here *)
          makeStatement (doe @ [IfThenElse (e', boxstmt st, boxstmt sf)])

      | Return e -> 
          let (_, doe, e', e'base) = boxexp e in
          let doe' = 
            match fdec.stype with 
              TFun(tRes, _, _, _) when hasBase tRes -> 
                doe @ 
                (* check the safety of the access *)
                [Call (None, Const (Str "CHECK_SAFERET", l),
                       [ e'; e'base ], l)]
            | _ -> doe
          in
          makeStatement (doe' @ [Return e'])

      | Instruction (Set(Var(vi, off, l), e, l')) ->
          let (rest, dooff, off', off'base) = boxoffset off vi.vtype in
          let (_, doe, e', e'base) = boxexp (CastE (e, rest)) in
          makeStatement (dooff @ doe @ 
                         (Set(Var(vi, off', l), e', l') ::
                          (if hasBase rest then 
                            [Set(Var(vi,off'base,l), e'base, l')]
                          else
                            [])))

      | Instruction (Set(Mem(addr, off, l), e, l')) ->
          let (addrt, doaddr, addr', addr'base) = boxexp addr in
          let (rest, dooff, off', off'base) = boxoffset off addrt in
          let newlval = Mem(addr', off', l) in
          let setbase, check = 
            if hasBase rest then
              [Set(Mem(addr',off'base,l), e'base, l')], "CHECK_SAFEWRFAT"
            else
              [], "CHECK_SAFEWRLEAN"
          in
          makeStatement (doaddr @ dooff @ 
                         (Call (None, Const (Str check, l),
                                [ AddrOf(LVal(newlval)); 
                                  addr'base;
                                  e']) ::
                          Set(newlval, e', l') :: setbase))
          

      | Instruction (Call(vi, f, args, l)) ->
          let (ft, dof, f', _) = boxexp f in
          let (ftret, ftargs) =
            match ft with 
              TFun(fret, fargs, _, _) -> (fret, fargs) 
            | _ -> E.i (E.bug "call of a non-function") 
          in
          let rec doArgs restargs restargst = 
            match restargs, restargst with
              [], [] -> [], []
            | a :: resta, t :: restt -> 
                let (_, doa, a', a'base) = boxexp a in
                let (doresta, resta') = doArgs resta restt in
                (doa @ doresta, 
                 a' :: (if hasBase t then  a'base :: resta' else resta'))
            | _ -> E.i (E.bug "vararg")
          in
          let (doargs, args') = doArgs args ftargs in
          makeStatement 
            (dolv @ dof @ doargs @ [Call (vi, f', args', l)])


          
          
          
  









