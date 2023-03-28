include Cil
(** @inline *)

(** {1 CIL modules} *)

module Cfg = Cfg
module Check = Check
module Cil = Cil
module Cilint = Cilint
module Cillower = Cillower
module Ciltools = Ciltools
module Cilutil = Cilutil
module Dataflow = Dataflow
module Dominators = Dominators
module Escape = Escape
module Expcompare = Expcompare
module Feature = Feature
module Formatcil = Formatcil
module Machdep = Machdep
module Machdepenv = Machdepenv
module Mergecil = Mergecil
module RmUnused = RmUnused

(** {1 FrontC modules} *)

module Cabs = Cabs
module Cabs2cil = Cabs2cil
module Cabshelper = Cabshelper
module Cabsvisit = Cabsvisit
module Cprint = Cprint
module Frontc = Frontc
module Patch = Patch
module Whitetrack = Whitetrack

(** {1 Utility modules} *)

module Alpha = Alpha
module Errormsg = Errormsg
module GrowArray = GrowArray (* needed for zrapp *)
module Inthash = Inthash (* needed for liveness, syntacticsearch *)
module Pretty = Pretty
module Stats = Stats
module Trace = Trace
module Util = Util
