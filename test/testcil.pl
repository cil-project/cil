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
    ("^Run.+ ([.\\d]+)ms" => sub { $_[1]->{run} = $_[2]; });

# Now add tests
$TEST->addTest("hashtest", @runpattern);
$TEST->addTest("wes-hashtest", @runpattern);
$TEST->addTest("rbtest", @runpattern);
$TEST->addTest("wes-rbtest", @runpattern);
$TEST->addTest("btreetest", @runpattern);
$TEST->addTest("apache/gzip");
# $TEST->addTest("test/t");

# print Dumper($TEST);

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

    if(! defined($self->{option}->{log})) {
        $self->{logFile} = "safec.log";
    }
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
  Default log file is safec.log

SafeC test harness
EOF
}

sub errorHeading {
    my($self, $err) = @_;
    return "Success" if $err == 0;
    return "Parse error" if $err == 1000;
    return "Cabs2cil error" if $err == 1001;
    return "Collecting constraints error" if $err == 1002;
    return "Constraint solving error" if $err == 1003;
    return "Boxing error" if $err == 1004;
    return "Error $err";
}

sub startParsingLog {
    my($self, $tst) = @_;
    $tst->{ErrorCode} = 0;
}


sub addTest {
    my($self, $name, %patterns) = @_;
    
    my $theargs = " ";

    my %commonerrors = 
        ("^make: \\*\\*\\*" => 
         sub { 
             if($_[1]->{ErrorCode} == 0) {
                 if($_[1]->{instage} == 0) {
                     $_[1]->{ErrorCode} = 2;
                 } else {
                     $_[1]->{ErrorCode} = $_[1]->{instage};
                 }
             }},

         "Syntax error" => sub { $_[0]->{ErrorCode} = 1000; },

         "^Error: Cabs2cil" => sub { $_[1]->{ErrorCode} = 1001; },

         "^FrontC finished conversion" => sub { $_[1]->{instage} = 1002; },

         "^Solving constraints" => sub { $_[1]->{instage} = 1003; },

         "^Error: Boxing" => sub { $_[1]->{ErrorCode} = 1004; },

         );
    my $k;
    foreach $k (keys %commonerrors) {
        $patterns{$k} = $commonerrors{$k};
    }

    $self->newTest(Name => $name . "-cil",
                   Dir => "..",
                   Cmd => "make " . $name . $theargs,
                   Group => ["cil"],
                   Patterns => \%patterns);

    $self->newTest(Name => $name . "-box",
                   Dir => "..",
                   Cmd => "make " . $name . " BOX=1 " . $theargs,
                   Group => ["box"],
                   Patterns => \%patterns);

    $self->newTest(Name => $name . "-inferbox",
                   Dir => "..",
                   Cmd => "make " . $name . " BOX=1 INFERBOX=1 " . $theargs,
                   Group => ["box", "infer"], 
                   Patterns => \%patterns);
}



1;
