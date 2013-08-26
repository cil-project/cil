(*
 * Copyright (c) 2013
 *  Gabriel Kerneis     <gabriel@kerneis.info>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open Cil

module E = Errormsg
module D = Dynlink
module F = Findlib

type t = Cil.featureDescr

(** List of registered features *)
let features = ref []
let register f = features := !features @ [f]
let list () = !features

(** Dynamic linking *)

let load s =
  try
    D.allow_unsafe_modules true;
    D.loadfile s
  with D.Error e -> E.s (E.error "%s" (D.error_message e))

(** Findlib magic *)

let initialized = ref false
let init () =
  if not !initialized then begin
    F.init ();
    initialized := true
  end

let findlib_lookup pkg =
  try
    let preds = [ if D.is_native then "native" else "byte"; "plugin" ] in
    let deps = F.package_deep_ancestors preds [pkg] in
    let find_modules pkg =
      let base = F.package_directory pkg in
      let archives = F.package_property preds pkg "archive" in
      let modules = Str.split (Str.regexp "[ ,]+") archives in
      List.map (fun m -> F.resolve_path ~base (D.adapt_filename m)) modules in
    let files = List.map find_modules deps in
    List.flatten files
  with
  | F.No_such_package (pkg, msg) ->
      E.s (E.error "findlib: no such package %s.\n%s" pkg msg)
  | F.Package_loop pkg ->
      E.s (E.error "findlib: package loop for %s." pkg)

let find_plugin s =
  if s = "" then E.s (E.error "missing module name") else
  if Sys.file_exists s then [s] else findlib_lookup s

(** List of loaded modules *)
let plugins = ref []

(** Add a single plugin, except if we have added it already *)
let add_plugin path =
  if not (List.mem path !plugins) then
  load path;
  plugins := path :: !plugins

(** Look for plugin and depencies and add them *)
let loadWithDeps s =
  let paths = find_plugin s in
  List.iter add_plugin paths

(** Parse only {switch} command-line option, ignoring every error raised by other, unparsed
 * options. Return the list of plugins to load. *)
let loadFromArgv switch =
  let spec = [
    switch, Arg.String loadWithDeps, "";
    (* ignore --help at this stage *)
    "--help", Arg.Unit ignore, ""; "-help", Arg.Unit ignore, "" ] in
  let idx = ref 0 in
  let rec aux () =
    try
      Arg.parse_argv ~current:idx Sys.argv spec ignore ""
    with Arg.Bad _ | Arg.Help _ -> incr idx; aux ()
  in init (); aux ()

let loadFromEnv name =
  try
    let plugins = Str.split (Str.regexp "[ ,]+") (Sys.getenv name) in
    List.iter loadWithDeps plugins
  with Not_found -> ()
