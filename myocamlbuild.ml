open Ocamlbuild_plugin ;;
open Command ;;
open Pathname ;;
open Outcome ;;


let find_modules builder mllib =
  let dirs = include_dirs_of (dirname mllib) in
  let modules = string_list_of_file mllib in
  let make_candidates m =
    List.map (expand_module dirs m) [["cmi"]; ["cmx"]; ["mli"; "inferred.mli"]] in
  let dependencies = List.flatten (List.map make_candidates modules) in
  let build_result = builder dependencies in
  let built_files = List.filter_opt
    (function Good file -> Some (!Options.build_dir/file) | Bad _ -> None) build_result in
  (* add a trailing space to ease concatenation of .libfiles *)
  (String.concat " " built_files) ^ " "
;;

let cil_version =
  try Printf.sprintf "(version %s)" (Sys.getenv "CIL_VERSION")
  with Not_found -> "" ;;

dispatch begin function
| After_rules ->
    (* the main CIL library *)
    ocaml_lib "src/cil";

    (* residual reliance on make to build some OCaml source files *)
    let make target =
      let basename = Pathname.basename target in
      rule ("make " ^ target)
      ~dep: "Makefile"
      ~prod: basename
      (fun _ _ -> Cmd (S
        [A (try Sys.getenv "MAKE" with Not_found -> "make");
         A "-C"; P ".."; P ("_build" / target)]))
      in
      make "machdep.ml";

    (* Build mllib for plugins by listing the content of their directory *)
    rule "plugin dir -> mllib"
    ~prod: "src/ext/%.mllib"
    (fun env builder ->
      let files = Array.to_list (Sys.readdir (".."/(env "src/ext/%"))) in
      let ml_files = List.filter
        (fun f -> let ext = get_extension f in ext = "ml" || ext = "mli") files in
      let modules = List.map module_name_of_pathname ml_files in
      let uniq_mods =
        List.fold_left (fun l m -> List.union l [m ^ "\n"]) [] modules in
      Echo (uniq_mods, (env "src/ext/%.mllib")));

    (* Build a list of files to install with ocamlfind *)
    rule "%.mllib -> %.libfiles"
    ~dep: "%.mllib"
    ~prod: "%.libfiles"
    (fun env builder -> Echo (
      [find_modules builder (env "%.mllib")],
      (env "%.libfiles")));

    (* Flags for ocamldoc *)
    flag ["ocaml";"doc"] (S [
      A "-stars";
      A "-hide"; A "Pervasives";
      A "-t" ; A (Printf.sprintf "CIL API Documentation %s" cil_version);
    ]);
| _ -> ()
end ;;
