#!/usr/bin/perl
# A simple use of the CompilerStub.
#
use strict;
use FindBin;
use Data::Dumper;

use lib "$FindBin::Bin"; # The libraries are in the same dirctory

use CompilerStub;

my $stub = MyCompiler->new(@ARGV);

# print Dumper($stub);

$stub->doit();


# Define here your favorite compiler by overriding CompilerStub methods
package MyCompiler;
use strict;
BEGIN {
    @MyCompiler::ISA = qw(CompilerStub);
}

# Customize the compilation
sub compile {
    my($self, @args) = @_;
    print "MyCompiler is compiling\n";
    $self->SUPER::compile(@args);
}
