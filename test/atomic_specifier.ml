open GoblintCil

let source = {|
_Atomic(int) scalar;
_Atomic(int *) pointer;
_Atomic(_Atomic(int) *) both;
_Atomic(int *) *nested;
_Atomic(int *) array[4];
_Atomic(void (*)(void)) callback;
_Atomic(int (*)[4]) array_pointer;
_Atomic(const int *) const_pointee;
const _Atomic(int *) const_pointer;
typedef int *ptr_t;
_Atomic(ptr_t) from_typedef;
typedef _Atomic(int *) atomic_ptr_t;
atomic_ptr_t via_typedef;
typedef struct Cell Cell;
struct Cell { int value; };
struct Stack { _Atomic(Cell *) top; };
void param(_Atomic(Cell *) *p);
_Atomic(Cell *) cast(void *p) { return (_Atomic(Cell *))p; }
|}

let variable file name =
  List.find_map (function
    | GVar (vi, _, _) | GVarDecl (vi, _) when vi.vname = name -> Some vi
    | GFun (fd, _) when fd.svar.vname = name -> Some fd.svar
    | _ -> None
  ) file.globals |> Option.get

let compound file name =
  List.find_map (function
    | GCompTag (ci, _) when ci.cname = name -> Some ci
    | _ -> None
  ) file.globals |> Option.get

let check_type name expected actual =
  if typeSig actual <> typeSig expected then
    failwith (name ^ ": incorrect atomic type")

let atomic = [Attr ("atomic", [])]
let const = [Attr ("const", []); Attr ("pconst", [])]

let check_file file =
  let pointer = TPtr (intType, atomic) in
  let cell_pointer = TPtr (TComp (compound file "Cell", []), atomic) in
  List.iter (fun (name, expected) ->
    check_type name expected (variable file name).vtype
  ) [
    "scalar", TInt (IInt, atomic);
    "pointer", pointer;
    "both", TPtr (TInt (IInt, atomic), atomic);
    "nested", TPtr (pointer, []);
    "array", TArray (pointer, Some (integer 4), []);
    "callback", TPtr (TFun (voidType, Some [], false, []), atomic);
    "array_pointer", TPtr (TArray (intType, Some (integer 4), []), atomic);
    "const_pointee", TPtr (TInt (IInt, const), atomic);
    "const_pointer", TPtr (intType, atomic @ const);
    "from_typedef", pointer;
    "via_typedef", pointer;
    "param", TFun (voidType, Some ["p", TPtr (cell_pointer, []), []], false, []);
    "cast", TFun (cell_pointer, Some ["p", voidPtrType, []], false, []);
  ];
  let top = List.find (fun fi -> fi.fname = "top") (compound file "Stack").cfields in
  check_type "top" cell_pointer top.ftype

let () =
  initCIL ();
  let filename = Filename.temp_file "cil-atomic-specifier" ".c" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) (fun () ->
    let write source =
      let channel = open_out filename in
      output_string channel source;
      close_out channel
    in
    write source;
    Whitetrack.enabled := true;
    let cabs, file = Frontc.parse_with_cabs filename () in
    check_file file;

    (* The CABS printer must retain the full type name inside _Atomic. *)
    let channel = open_out filename in
    Cprint.printFile channel cabs;
    Whitetrack.setOutput stdout;
    close_out channel;
    Whitetrack.enabled := false;
    check_file (Frontc.parse filename ());

    (* Visitors must reach both the specifiers and the abstract declarator. *)
    let visitor = object
      inherit Cabsvisit.nopCabsVisitor
      method! vtypespec = function
        | Cabs.Tint -> Cabsvisit.ChangeTo Cabs.Tlong
        | _ -> Cabsvisit.DoChildren
      method! vexpr = function
        | Cabs.CONSTANT (Cabs.CONST_INT "4") ->
            Cabsvisit.ChangeTo (Cabs.CONSTANT (Cabs.CONST_INT "5"))
        | _ -> Cabsvisit.DoChildren
    end in
    let visited = Cabs2cil.convFile (Cabsvisit.visitCabsFile visitor cabs) in
    check_type "visited pointer" (TPtr (longType, atomic))
      (variable visited "pointer").vtype;
    check_type "visited array pointer"
      (TPtr (TArray (longType, Some (integer 5), []), atomic))
      (variable visited "array_pointer").vtype;

    (* The type-name form may not contain an already qualified, atomic,
       array or function type, including when hidden behind a typedef. *)
    List.iter (fun source ->
      write source;
      Frontc.resetErrors ();
      let rejected =
        try
          ignore (Frontc.parse filename ());
          !Errormsg.hadErrors
        with Errormsg.Error -> true
      in
      if not rejected then failwith ("accepted invalid atomic type: " ^ source)
    ) [
      "_Atomic(int[4]) bad;";
      "typedef int array_t[4]; _Atomic(array_t) bad;";
      "typedef int func_t(void); _Atomic(func_t) bad;";
      "_Atomic(const int) bad;";
      "_Atomic(int * const) bad;";
      "typedef volatile int qual_t; _Atomic(qual_t) bad;";
      "_Atomic(_Atomic(int)) bad;";
      "typedef _Atomic(int *) atomic_t; _Atomic(atomic_t) bad;";
      "int _Atomic(int) bad;";
    ];
    Frontc.resetErrors ())
