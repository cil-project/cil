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

(** Extending CIL with external features *)

(** Description of a CIL feature. *)
type t = Cil.featureDescr

(** Register a feature to be used by CIL. *)
val register : t -> unit

(**/**)

(** Get the list of registered features. *)
val list : unit -> t list

(** Initialize the module. This needs to be called before {!loadWithDeps} is
 * used. Called automatically by {!loadFromArgv}. *)
val init : unit -> unit

(** Find and dynamically links a module. The name should be either a path to a
 * cmo, cma or cmxs file, or the name of a findlib package. In the latter case,
 * package dependencies are loaded automatically. Each file is loaded at most
 * one.  The loaded module must call {!registerFeature} to make its features
 * available to CIL. *)
val loadWithDeps : string -> unit

(** {!loadFromArgv switch} searches {!Sys.argv} for the command-line option
 * {!switch}, and loads the modules passed as parameters. Ignores every other
 * {!Sys.argv} element. *)
val loadFromArgv : string -> unit

(** {!loadFromEnv name} loads coma-separated module names stored in the
 * environment variable {!name}. *)
val loadFromEnv : string -> unit
