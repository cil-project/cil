#!/usr/bin/perl
# A simple use of the CompilerStub.
#
use strict;
use FindBin;
use Data::Dumper;

use lib "$FindBin::Bin"; # The libraries are in the same dirctory

use CompilerStub;

my $stub = CilCompiler->new(@ARGV);

# print Dumper($stub);

$stub->doit();


# Define here your favorite compiler by overriding CompilerStub methods
package CilCompiler;
use File::Basename;
use strict;
BEGIN {
    @CilCompiler::ISA = qw(CompilerStub);
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


# Customize the preprocessing
# Add -DCIL
sub preprocess {
    my ($self, $src, $dest, $ppargs) = @_;
    my @newppargs = @{$ppargs};
    push @newppargs, " $self->{DEFARG}CIL ";
    return $self->SUPER::preprocess($src, $dest, \@newppargs);
}

sub usage {
    print "cilly [options] [gcc_or_mscl arguments]\n";
}

# Customize the compilation
# SRC is preprocessed already but may be already cilly-fied
sub compile {
    my ($self, $src, $dest, $ppargs, $ccargs) = @_;
    
    my ($base, $dir, $ext) = fileparse($src, "\\.[^.]+");
    if($base =~ m|cil$|) { # Already the output of CIL
        return $self->SUPER::compile($src, $dest, $ppargs, $ccargs);
    } else { # Must pas through CIL first
        if($self->{VERBOSE}) { print "CilCompiler compiling $src to $dest\n"; }

        my $cmd = $CilCompiler::compiler;
        
        if($self->{MODENAME} eq "MSVC") {
            $cmd .= " --MSVC ";
        }
        if($self->{VERBOSE}) {
            $cmd .= " --verbose ";
        }
        # Make a name for the CIL file
        my $cilfile = "$dir$base" . "cil.c";
        $self->runShell("$cmd $src -o $cilfile");

        # Now preprocess and compile again
        my $res = $self->SUPER::preprocess_compile($cilfile, 
                                                   $dest, $ppargs, $ccargs);
        return 0;
    }

}

# We need to customize the collection of arguments
sub collectOneArgument {
    my($self, $arg, $pargs) = @_;
    # See if the super class understands this
    if($self->SUPER::collectOneArgument($arg, $pargs)) { return 1; }
    if($arg =~ m|--nofail|)  {
        $self->{NOFAIL} = 1; return 1;
    }
    return 0;
}
