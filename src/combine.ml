(* combine.ml : special "main" procedure for Raymond's source-code combiner. *)

let combine (files : string list) = begin
    let list_of_parsed_files =
      List.map (fun file_name -> Frontc.parse_to_cabs file_name) files in
    (* your combining function goes somewhere around here *)
    let list_of_all_decls = List.flatten list_of_parsed_files in
    (* for now, let's just glom all of the declarations in one thing *)
    Cprint.print stdout list_of_all_decls 
end
    
