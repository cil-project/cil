open GoblintCil

let source = {|
int *plain;
_Atomic int *pointee;
int * _Atomic pointer;
_Atomic int * _Atomic both;
int * _Atomic array[4];
void (* _Atomic callback)(void);
int * _Atomic *nested;
int * _Atomic volatile qualified;
|}

let () =
  initCIL ();
  let filename = Filename.temp_file "cil-atomic-qualifier" ".c" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) (fun () ->
    let channel = open_out filename in
    output_string channel source;
    close_out channel;
    let file = Frontc.parse filename () in
    let variables = List.filter_map (function
      | GVar (vi, _, _) | GVarDecl (vi, _) -> Some vi
      | _ -> None
    ) file.globals in
    let atomic = [Attr ("atomic", [])] in
    let atomic_int = TInt (IInt, atomic) in
    (* Check the qualifier's binding, not just whether parsing succeeds. *)
    List.iter (fun (name, expected) ->
      let vi = List.find (fun vi -> vi.vname = name) variables in
      assert (vi.vattr = []);
      if typeSig vi.vtype <> typeSig expected then
        failwith (name ^ ": incorrect atomic qualifier placement")
    ) [
      "plain", TPtr (intType, []);
      "pointee", TPtr (atomic_int, []);
      "pointer", TPtr (intType, atomic);
      "both", TPtr (atomic_int, atomic);
      "array", TArray (TPtr (intType, atomic), Some (integer 4), []);
      "callback", TPtr (TFun (voidType, Some [], false, []), atomic);
      "nested", TPtr (TPtr (intType, atomic), []);
      "qualified", TPtr (intType, [Attr ("atomic", []); Attr ("volatile", [])]);
    ])
