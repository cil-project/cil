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
use strict;
BEGIN {
    @CilCompiler::ISA = qw(CompilerStub);
    $CilCompiler::base = $FindBin::Bin . "/../obj/cilly";
}

# Customize the compilation
sub compile {
    my ($self, $src, $dest, $ppargs, $ccargs) = @_;
    if($self->{VERBOSE}) { print "CilCompiler compiling $src to $dest\n"; }

    # Select the compiler to use
    my $cmd = 
    if($self->{MODENAME} eq "MSVC") {
        $cmd .= " --MSVC ";
    }
    if($self->{VERBOSE}) {
        $cmd .= " --verbose ";
    }
    $cmd .= join(' ', @tocombine);
    $self->runShell($cmd);
    $self->SUPER::compile(@args);
}

