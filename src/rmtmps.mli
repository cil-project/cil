(*
 *
 * Copyright (c) 2001-2002 by
 *  George C. Necula	necula@cs.berkeley.edu
 *  Scott McPeak        smcpeak@cs.berkeley.edu
 *  Wes Weimer          weimer@cs.berkeley.edu
 *  Ben Liblit          liblit@cs.berkeley.edu
 *   
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software for research purposes only is hereby granted, 
 * provided that the following conditions are met: 
 * 1. XSRedistributions of source code must retain the above copyright notice, 
 * this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 * this list of conditions and the following disclaimer in the documentation 
 * and/or other materials provided with the distribution. 
 * 3. The name of the authors may not be used to endorse or promote products 
 * derived from  this software without specific prior written permission. 
 *
 * DISCLAIMER:
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR 
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(* rmtmps.mli *)
(* remove unused things from cil files:               *)
(*   - local temporaries introduced but not used      *)
(*   - global declarations that are not used          *)
(*   - types that are not used                        *)


(* Some clients may wish to augment or replace the standard strategy
 * for marking reachable roots.  The optional "markRooks" argument to
 * Rmtmps.removeUnusedTemps grants this flexibility.  If given, it
 * should name a function which will set the appropriate referenced
 * bits on any global symbols which should be treated as retained
 * roots.
 * 
 * Function Rmtmps.defaultRootsMarker encapsulates the default root
 * marking logic, which consists of marking those global variables and
 * functions which are visible to the linker and runtime loader.  A
 * client's root marker can call this directly if the goal is to
 * augment rather than replace the standard logic.
 * 
 * Note that certain CIL- and CCured-specific pragmas induce
 * additional global roots.  This functionality is always present, and
 * is not subject to replacement by "markRoots".
 *)

type rootsMarker = Cil.file -> unit
val defaultRootsMarker : rootsMarker

(* process a complete Cil file *)
val removeUnusedTemps: ?markRoots:rootsMarker -> Cil.file -> unit


val keepUnused: bool ref (* Set this to true to turn off this module *)

(* AAARRGGGHHH!!!! *)
(*val hack_Cil_d_global: unit -> Cil.global -> Pretty.doc*)
