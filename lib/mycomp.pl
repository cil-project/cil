#!/usr/bin/perl
# A simple use of the CompilerStub.
#
use strict;
use FindBin;
use Data::Dumper;

use lib "$FindBin::Bin"; # The libraries are in the same dirctory

use CompilerStub;

my $stub = MyCompiler->new(@ARGV);

$stub->compile();

print Dumper($stub);

# Define here your favorite compiler by overriding CompilerStub methods
package MyCompiler;
use strict;
BEGIN {
    @MyCompiler::ISA = qw(CompilerStub);
}


sub compile {
    my($self, @args) = @_;
    print "MyCompiler is compiling\n";
    SUPER->compile($self, @args);
}
