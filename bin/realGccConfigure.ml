module C = Configurator.V1

let bad_gcc_version_regexp = Str.regexp_case_fold "clang\\|apple\\|darwin"

let is_bad_gcc_version stdout =
  match Str.search_forward bad_gcc_version_regexp stdout 0 with
  | exception Not_found -> false
  | _ -> true

let is_real_gcc c gcc =
  let result: C.Process.result = C.Process.run c gcc ["--version"] in
  result.exit_code = 0 && not (is_bad_gcc_version result.stdout)

let gccs = [
  "gcc";
  "gcc-16";
  "gcc-15";
  "gcc-14";
  "gcc-13";
  "gcc-12";
  "gcc-11";
  "gcc-10";
  "gcc-9";
  "gcc-8";
  "gcc-7";
]

let () =
  C.main ~name:"real-gcc" (fun c ->
      match List.find_opt (is_real_gcc c) gccs with
      | Some gcc ->
        C.Flags.write_lines "real-gcc" [gcc]
      | None ->
        failwith "couldn't find real gcc"
    )
