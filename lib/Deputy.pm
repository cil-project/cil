#
#
# Copyright (c) 2001-2002, 
#  George C. Necula    <necula@cs.berkeley.edu>
#  Scott McPeak        <smcpeak@cs.berkeley.edu>
#  Wes Weimer          <weimer@cs.berkeley.edu>
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. The names of the contributors may not be used to endorse or promote
# products derived from this software without specific prior written
# permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

package Deputy;
use strict;

use CilConfig;
use Cilly;

$::default_is_merge = 0;

# NOTE: If perl chokes, complaining about 'our', or
# "Array found where operator expected", it's because
# you need perl version 5.6.0 or later.
our @ISA = qw(Cilly);

my $base = "$::cilhome/obj/$::archos/cilly";
# Select the most recent executable
my $mtime_asm = int((stat("$base.asm.exe"))[9]);
my $mtime_byte = int((stat("$base.byte.exe"))[9]);
my $use_debug = 
        grep(/--bytecode/, @ARGV) ||
        grep(/--ocamldebug/, @ARGV) ||
        ($mtime_asm < $mtime_byte);
if($use_debug) { 
    $ENV{"OCAMLRUNPARAM"} = "b" . $ENV{"OCAMLRUNPARAM"}; # Print back trace
} 

my $compiler = 
    $base . 
    ($use_debug ? ".byte.exe" : ".asm.exe");

sub collectOneArgument {
    my ($self, $arg, $pargs) = @_;
    my $res;
    if ($arg eq "--linux") {
        $self->{LINUX} = 1;
        $res = 1;
    } else {
        $res = $self->SUPER::collectOneArgument($arg, $pargs);
    }
    return $res;
}

sub preprocess_before_cil {
    my($self, $src, $dest, $ppargs) = @_;
    my @args = @{$ppargs};
    unshift @args,
    $self->forceIncludeArg("$::cilhome/include/deputy/annots.h");
    unshift @args, $self->{INCARG} . $::cilhome . "/include";
    return $self->SUPER::preprocess_before_cil($src, $dest, \@args);
}

sub preprocess_after_cil {
    my ($self, $src, $dest, $ppargs) = @_;
    my @args = @{$ppargs};
    push @args, "$self->{INCARG}$::cilhome/include";
    return $self->SUPER::preprocess_after_cil($src, $dest, \@args);
}

sub link_after_cil {
    my ($self, $psrcs, $dest, $ppargs, $ccargs, $ldargs) = @_;
    my @srcs = @{$psrcs};
    my $lib = $self->{LINUX} ? "linux" : "libc";
    push @srcs, "$::cilhome/obj/$::archos/deputy_$lib.$self->{OBJEXT}";
    return $self->SUPER::link_after_cil(\@srcs, $dest, $ppargs,
                                        $ccargs, $ldargs);
}

sub linktolib {
    my ($self, $psrcs, $dest, $ppargs, $ccargs, $ldargs) = @_;
    my @srcs = @{$psrcs};
    my $lib = $self->{LINUX} ? "linux" : "libc";
    push @srcs, "$::cilhome/obj/$::archos/deputy_$lib.$self->{OBJEXT}";
    return $self->SUPER::linktolib(\@srcs, $dest, $ppargs, $ccargs, $ldargs);
}

sub CillyCommand {
    my ($self, $ppsrc, $dest) = @_;

    my @cmd = ($compiler);
    my $aftercil = $self->cilOutputFile($dest, 'cil.c');
    return ($aftercil, @cmd, '--out', $aftercil);
}


sub helpMessage {
    my($self) = @_;
    # Print first the original
    $self->SUPER::helpMessage();
    print <<EOF;

  All other arguments starting with -- are passed to the Cilly process.

The following are the arguments of the Cilly process
EOF
   my @cmd = ($compiler, '-help');
   $self->runShell(@cmd); 
}

1;
