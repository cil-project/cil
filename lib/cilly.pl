#!/usr/bin/perl
# A simple use of the CompilerStub.
#
use strict;
use FindBin;
use Data::Dumper;

use lib "$FindBin::Bin"; # The libraries are in the same directory

use Merger;

my $stub = CilCompiler->new(@ARGV);

# print Dumper($stub);

$stub->doit();


# Define here your favorite compiler by overriding Merger methods
package CilCompiler;
use File::Basename;
use strict;
BEGIN {
    @CilCompiler::ISA = qw(Merger);
    $CilCompiler::base = $FindBin::Bin . "/../obj/cilly";

    $CilCompiler::mtime_asm = int((stat("$CilCompiler::base.asm.exe"))[9]);
    $CilCompiler::mtime_byte = int((stat("$CilCompiler::base.byte.exe"))[9]);
    $CilCompiler::compiler = 
        $CilCompiler::base . 
            ($CilCompiler::mtime_asm >= $CilCompiler::mtime_byte 
             ? ".asm.exe" : ".byte.exe");
    # For some strange reason on Windows the bytecode versions must be passed
    # as an argument to themselves
    if($^O eq 'MSWin32' && 
       ($CilCompiler::mtime_asm < $CilCompiler::mtime_byte)) {
        $CilCompiler::compiler .= " " . $CilCompiler::compiler;
    }
}



sub usage {
    print "cilly [options] [gcc_or_mscl arguments]\n";
}

sub helpMessage {
    my($self) = @_;
    # Print first the original
    $self->SUPER::helpMessage();
    print <<EOF;

  All other arguments starting with -- are passed to the Cilly process.

The following are the arguments of the Cilly process
EOF
   my $cmd = $CilCompiler::compiler . " -help";
   $self->runShell($cmd); 
}


# SRC is preprocessed already but may be already cilly-fied
sub applyCil {
    my ($self, $src, $ppargs) = @_;

    my ($base, $dir, $ext) = fileparse($src, "(\\.[^.]+)");
    my $dest = "$dir/$base" . "cil.c";
    if($self->{VERBOSE}) { print "Cilly compiling $src to $dest\n"; }
    
    my $cmd = $CilCompiler::compiler;
    
    if($self->{MODENAME} eq "MSVC") {
        $cmd .= " --MSVC ";
    }
    if($self->{VERBOSE}) {
        $cmd .= " --verbose ";
    }
    if(defined $self->{CILARGS}) {
        $cmd .= join(' ', @{$self->{CILARGS}});
    }
    # Make a name for the CIL file
    my $cilfile = "$dir$base" . "cil.c";
    $self->runShell("$cmd $src --out $cilfile");

    return $cilfile;
}

