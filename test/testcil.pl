#
# A regression tester for safec
#
require 5.000;

# Packages to import.
use Getopt::Long;           # Command-line option processing
use File::Basename;         # File name parsing
use Cwd;                    # Directory navigation
use strict;
# use Data::Dumper;
use FindBin;
use lib "$FindBin::Bin";

use RegTest;

print "Test infrastructure for SafeC\n";

# Create out customized test harness
my $TEST = SafecRegTest->new(AvailParams => {"run" => 1, 
                                             "parse" => 1, 
                                             "solve" => 1, 
                                             "print" => 1, 
                                             "box" => 1},
                             LogFile => "safec.log",
                             CommandName => "testsafec");

my @runpattern = 
    ("^Run.+ ([.\\d]+)ms" => sub { $_[1]->{"run"} = $_[2]; });

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
    
    "Syntax error" => sub { $_[1]->{ErrorCode} = 1000; },
    
    "^Error: Cabs2cil" => sub { $_[1]->{ErrorCode} = 1001; },
    
    "^FrontC finished conversion" => sub { $_[1]->{instage} = 1002; },
    
    "^Solving constraints" => sub { $_[1]->{instage} = 1003; },
    
    "^Error: Boxing" => sub { $_[1]->{ErrorCode} = 1004; },

         # Collect some more parameters
    "^parse\\s+([\\.\\d]+) s" => sub { $_[1]->{instage} = 1005; # If here we
                                                     # terminated successfully
                                     $_[1]->{parse} = $_[2];},
    "^print.+\\s+([\\.\\d]+) s" => sub { $_[1]->{print} += $_[2];},
    "^box\\s+([\\.\\d]+) s" => sub { $_[1]->{box} += $_[2];},
    "^\\s+simple solver\\s+([\\.\\d]+) s" => sub { $_[1]->{solve} += $_[2];},
         # Now error messages
    "^(Bug: .+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^(Unimplemented: .+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^(.+ : error .+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^(.+:\\d+: [^w].+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^(.+: fatal error.+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },

         );

my $inferbox = 4;

# Now add tests
$TEST->add3Tests("btreetest", "", @runpattern);
   $TEST->addComment("btreetest-inferbox", "bug in solver");
$TEST->add3Tests("hashtest", "", @runpattern);
$TEST->add3Tests("rbtest", "", @runpattern);
$TEST->add3Tests("hufftest", "", @runpattern);
   $TEST->addComment("hufftest-inferbox", "bug in solver");
#   $TEST->addComment("hufftest-box", "missing wrappers");
   $TEST->addComment("hufftest-cil", "bug running the test???");
$TEST->add3Tests("test/alloc");
$TEST->add3Tests("test/argcast", "", @runpattern);
$TEST->add3Tests("test/array1");
$TEST->add3Tests("test/array2");
$TEST->add3Tests("test/attr");
$TEST->add3Tests("test/bh1", "", @runpattern);
$TEST->add3Tests("test/bitfield");
$TEST->add3Tests("test/box1");
$TEST->add3Tests("test/cast1");
$TEST->add3Tests("test/cast2");
$TEST->add3Tests("test/constprop");
$TEST->add3Tests("test/enum");
$TEST->add3Tests("test/format1");
$TEST->add3Tests("test/func");
$TEST->add3Tests("test/globals");
$TEST->add3Tests("test/huff1");
#  $TEST->addComment("test/huff1-box", "pragma box misuse");
  $TEST->addComment("test/huff1-inferbox", "pragma box misuse");
$TEST->add3Tests("test/init");
$TEST->add3Tests("test/initial", "_GNUCC=1");
 $TEST->add3Comment("test/initial", "GCC/MSVC bug");
$TEST->add3Tests("test/jmp_buf");
$TEST->add3Tests("test/linux_atomic");
 $TEST->add3Comment("test/linux_atomic", "parsing error");
$TEST->add3Tests("test/li");
$TEST->add3Tests("test/li1", "_GNUCC=1");
$TEST->add3Tests("test/list");
$TEST->add3Tests("test/pointers");
$TEST->add3Tests("test/printf", "", @runpattern);
$TEST->add3Tests("test/retval");
$TEST->add3Tests("test/seq");
$TEST->add3Tests("test/sized");
$TEST->add3Tests("test/sizeof");
$TEST->add3Tests("test/smallstring");
$TEST->add3Tests("test/static", "", @runpattern);
$TEST->add3Tests("test/static1");
$TEST->add3Tests("test/strcpy");
$TEST->add3Tests("test/string");
$TEST->add3Tests("test/struct_init");
$TEST->add3Tests("test/structassign");
$TEST->add3Tests("test/tags");
$TEST->add3Tests("test/task");
    $TEST->add3Comment("test/task", "undefined structure");
$TEST->add3Tests("test/scope1");
$TEST->add3Tests("test/scope2");
$TEST->add3Tests("test/voidstar");
$TEST->add3Tests("wes-hashtest", "", @runpattern);
$TEST->add3Tests("wes-rbtest", "", @runpattern);
$TEST->add1Test("test/alloc-manualinferbox",
                "test/alloc INFERBOX=$inferbox MANUALBOX=1",
                %commonerrors);
$TEST->add3Tests("bh", "_GNUCC=1");
#   $TEST->addComment("bh-box", "missing wrappers");
$TEST->add3Tests("li", "_GNUCC=1");
#  $TEST->addComment("li-box", "bug in box.ml");
  $TEST->addComment("li-inferbox", "bug in box.ml");
$TEST->add3Tests("compress", "_GNUCC=1");
#   $TEST->addComment("compress-box", "missing wrappers");
$TEST->add3Tests("go", "_GNUCC=1");
$TEST->add3Tests("apache/gzip");
$TEST->add3Tests("apache/rewrite");
  $TEST->addComment("apache/rewrite-cil", "missing main");
$TEST->add3Tests("apache/urlcount");
$TEST->add3Tests("apache/layout");
$TEST->add3Tests("apache/random");

# $TEST->getTest("apache/gzip-inferbox")->{Enabled} = 0; # Due to a bug
# my $tst = $TEST->getTest("apache/gzip-inferbox");
# print Dumper($tst);


# print Dumper($TEST);

# Now invoke it
$TEST->doit();

# print Dumper($TEST);

exit(0);


###
###
###
### Specialize RegTest
###
package SafecRegTest;

use strict;
# use Data::Dumper;

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
        "--safecdebug!",
            );
}


sub extraHelpMessage {
    my($self) = @_;
    
    my ($scriptname, $extra) = $self->SUPER::extraHelpMessage();
    return ("testsafec",
            $extra . << "EOF");

Additional arguments for SafeC test harness
  --safecdebug         Use the debug versions of everything (default is false)

  Default log file is safec.log
EOF
}

sub errorHeading {
    my($self, $err) = @_;
    return "Not executed" if $err == -1;
    return "Success" if $err == 0;
    return "Parse error" if $err == 1000;
    return "Cabs2cil error" if $err == 1001;
    return "Collecting constraints error" if $err == 1002;
    return "Constraint solving error" if $err == 1003;
    return "Boxing error" if $err == 1004;
    return "Compilation error" if $err == 1005;
    return "Error $err";
}

sub startParsingLog {
    my($self, $tst) = @_;
    $tst->{ErrorCode} = 0;
}


sub availableParameters {
    my($self) = @_;
    return %::availpars;
}

sub add3Tests {
    my($self, $name, $extraargs, %patterns) = @_;
    
    my $theargs = defined($self->{option}->{safecdebug}) ? " " : " RELEASE=1 ";
    $theargs .= " $extraargs ";

    my $k;
    my %patterns = %commonerrors;

    $self->newTest(Name => $name . "-cil",
                   Dir => "..",
                   Cmd => "make " . $name . $theargs,
                   Group => ["cil"],
                   Patterns => \%patterns);

#    $self->newTest(Name => $name . "-box",
#                   Dir => "..",
#                   Cmd => "make " . $name . " INFERBOX=wild MANUALBOX= " . $theargs,
#                   Group => ["box"],
#                   Patterns => \%patterns);

    $self->newTest(Name => $name . "-inferbox",
                   Dir => "..",
                   Cmd => "make " . $name . " INFERBOX=$inferbox " . $theargs,
                   Group => ["box", "infer"], 
                   Patterns => \%patterns);
}


sub add1Test {
    my($self, $name, $args, %patterns) = @_;
    
    my $theargs = 
        defined($self->{option}->{safecdebug}) ? $args : " $args RELEASE=1 ";

    my $k;
    my %patterns = %commonerrors;

    $self->newTest(Name => $name,
                   Dir => "..",
                   Cmd => "make $theargs",
                   Group => ["cil"],
                   Patterns => \%patterns);
}


sub add3Comment {
    my ($self, $name, $comm) = @_;
    $self->addComment($name . "-cil", $comm);
#    $self->addComment($name . "-box", $comm);
    $self->addComment($name . "-inferbox", $comm);
}

sub prepend3Command {
    my ($self, $name, $comm) = @_;
    my $tst = $self->getTest($name . "-cil");
    $tst->{Cmd} = $comm . $tst->{Cmd};
    my $tst = $self->getTest($name . "-box");
    $tst->{Cmd} = $comm . $tst->{Cmd};
    my $tst = $self->getTest($name . "-inferbox");
    $tst->{Cmd} = $comm . $tst->{Cmd};
}

1;
