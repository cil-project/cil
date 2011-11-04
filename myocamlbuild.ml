open Ocamlbuild_plugin
open Command


;;


dispatch begin
  function
    | After_rules ->
	(* the main CIL library *)
	ocaml_lib "src/cil";

	(* residual reliance on make to build some OCaml source files *)
	let make target =
	  let basename = Pathname.basename target in
	  rule ("make " ^ target)
	    ~dep: "Makefile"
	    ~prod: basename
	    begin
	      fun env _ ->
		Cmd (S [A "make";
			A "-s";
			A "-C"; P "..";
			A "MODULES=";
			A "OBJDIR=_build";
			P ("_build" / target)])
	    end
	in
	make "cilversion.ml";
	make "feature_config.ml";
	make "machdep.ml";
	make "lib/Cilly.pm";

    | _ ->
	()
end
