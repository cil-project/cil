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

my @runpattern = 
    ("^Run.+ ([.\\d]+)ms" => sub { $_[0]->{run} = $_[1]; });

# Now add tests
$TEST->addTest("hashtest", @runpattern);
$TEST->addTest("wes-hashtest", @runpattern);
$TEST->addTest("rbtest", @runpattern);
$TEST->addTest("wes-rbtest", @runpattern);
$TEST->addTest("btreetest", @runpattern);


# Now invoke it
$TEST->doit();

# print Dumper($TEST);

exit(0);


### Specialize RegTest
package SafecRegTest;

use strict;
use Data::Dumper;

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



sub addTest {
    my($self, $name, %patterns) = @_;
    
    my $theargs = " ";

    my %commonerrors = 
        ("^make: \\*\\*\\*" => sub { $_[0]->{ErrorCode} = 2; }
         );
    my $k;
    foreach $k (keys %commonerrors) {
        $patterns{$k} = $commonerrors{$k};
    }

    OneTest->new($self, $name . "-cil",
                 Dir => "..",
                 Cmd => "make " . $name . $theargs,
                 Group => ["cil"],
                 Patterns => \%patterns);

    OneTest->new($self, $name . "-box",
                 Dir => "..",
                 Cmd => "make " . $name . " BOX=1 " . $theargs,
                 Group => ["box"],
                 Patterns => \%patterns);

    OneTest->new($self, $name . "-inferbox",
                 Dir => "..",
                 Cmd => "make " . $name . " BOX=1 INFERBOX=1 " . $theargs,
                 Group => ["box", "infer"], 
                 Patterns => \%patterns);
}


1;
