#
# A regression tester for safec
#
require 5.000;

# Packages to import.
use Getopt::Long;           # Command-line option processing
use File::Basename;         # File name parsing
use Cwd;                    # Directory navigation
use strict;
use Data::Dumper;
use FindBin;
use lib "$FindBin::Bin";

use RegTest;

print "Test infrastructure for SafeC\n";

# Create out customized test harness
my $TEST = SafecRegTest->new();

# Now add tests
$TEST->newTest("hashtest-cil",
               Dir => "..",
               Args => ["make", "hashtest"],
               Group => ["cil"]);
$TEST->newTest("hashtest-box",
               Dir => "..",
               Args => ["make", "hashtest BOX=1"],
               Group -> ["box"]);

# print Dumper($TEST);

# Now invoke it
$TEST->doit();

exit(0);


### Specialize RegTest
package SafecRegTest;

use strict;
BEGIN {
    use RegTest;
    @SafecRegTest::ISA = qw(RegTest);        # Inherit from RegTest
}

# The constructor
sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = $class->SUPER::new(@_);

    return $self;
}

# Special command line options
sub extraOptions {
    my($self) = @_;
    my @supopt = $self->SUPER::extraOptions();
    return (
        @supopt,
        "--safec=s",
            );
}


sub extraHelpMessage {
    my($self) = @_;
    
    my ($scriptname, $extra) = $self->SUPER::extraHelpMessage();
    return ("testsafec",
            $extra . << "EOF");
  --safec

SafeC test harness
EOF
}

1;
