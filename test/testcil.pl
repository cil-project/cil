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

# Create our customized test harness
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
    "stackdump: Dumping stack trace" => sub { $_[1]->{ErrorCode} = 1006; },
    
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

# Start with a few tests that must be run first
$TEST->newTest(
    Name => "\@\@inittests0",
    Dir => "..",
    Cmd => "make defaulttarget RELEASE=");
if($TEST->{option}->{safecdebug}) {
    $TEST->newTest(
                   Name => "\@\@inittests2",
                   Dir => "..",
                   Cmd => "make defaulttarget _GNUCC=1");
} else {
    $TEST->newTest(
                   Name => "\@\@inittests1",
                   Dir => "..",
                   Cmd => "make defaulttarget RELEASE=1");
    $TEST->newTest(
                   Name => "\@\@inittests2",
                   Dir => "..",
                   Cmd => "make defaulttarget RELEASE=1 _GNUCC=1");
}
    
# Now add tests
$TEST->add3Tests("btreetest", "", @runpattern);
#   $TEST->addBadComment("btreetest-box", "crashes!!!");
$TEST->add3Tests("hashtest", "", @runpattern);
$TEST->add3Tests("rbtest", "", @runpattern);
$TEST->add3Tests("hufftest", "", @runpattern);
#   $TEST->addBadComment("hufftest-inferbox", "missing wrapper for memmove");
#   $TEST->addBadComment("hufftest-box", "missing wrappers");
$TEST->add3Tests("test/alloc");
$TEST->add3Tests("test/argcast", "", @runpattern);
$TEST->add3Tests("test/array1");
$TEST->add3Tests("test/array2");
$TEST->add3Tests("test/matrix");
$TEST->add3Tests("testrun/switch");
$TEST->add3Tests("testrun/caserange", "_GNUCC=1");
$TEST->add3Tests("test/attr");
$TEST->add3Tests("test/attr2", "_GNUCC=1");
$TEST->add3Tests("test/attr3", "_GNUCC=1");
#   $TEST->addBadComment("test/attr3-cil", "BUG");
#   $TEST->addBadComment("test/attr3-box", "error printing attributes");
#   $TEST->addBadComment("test/attr3-inferbox", "error printing attributes");
$TEST->add3Tests("testrun/attr4", "_GNUCC=1");
$TEST->add3Tests("test/bh1", "", @runpattern);
$TEST->add3Tests("test/bitfield");
$TEST->add3Tests("test/box1");
$TEST->add3Tests("test/cast1");
$TEST->add3Tests("test/cast2");
$TEST->add3Tests("test/constprop");
$TEST->add3Tests("test/enum");
$TEST->add3Tests("test/format1");
$TEST->add3Tests("test/func");
$TEST->add3Tests("testrun/func2");
$TEST->add3Tests("testrun/func3");
$TEST->add3Tests("test/globals");
$TEST->add3Tests("test/huff1");
  $TEST->addBadComment("test/huff1-box", "pragma box misuse");
  $TEST->addBadComment("test/huff1-inferbox", "pragma box misuse");
$TEST->add3Tests("testrun/init");
$TEST->add3Tests("testrun/init1");
$TEST->add1Tests("testrun/init2", "_GNUCC=1");
$TEST->add3Tests("test/initial", "_GNUCC=1");
$TEST->add3Tests("test/jmp_buf");
$TEST->add3Tests("test/linux_atomic", "_GNUCC=1");
  $TEST->addBadComment("test/linux_atomic-box", "strange C code");
$TEST->add3Tests("test/li");
$TEST->add3Tests("test/li1", "_GNUCC=1");
#  $TEST->addBadComment("test/li1-box", "BUG");
$TEST->add3Tests("test/list");
$TEST->add3Tests("test/pointers");
$TEST->add3Tests("test/printf", "", @runpattern);
$TEST->add3Tests("test/retval");
$TEST->add3Tests("test/seq");
$TEST->add3Tests("test/sized");
$TEST->add3Tests("test/sizeof");
$TEST->add3Tests("test/smallstring");
$TEST->add3Tests("testrun/static", "", @runpattern);
$TEST->add3Tests("test/static1");
$TEST->add3Tests("test/strcpy");
$TEST->add3Tests("test/string");
$TEST->add3Tests("test/struct_init");
$TEST->add3Tests("test/structassign");
$TEST->add3Tests("test/tags");
$TEST->add3Tests("test/task", "_GNUCC=1");
$TEST->add3Tests("test/power1");
$TEST->add3Tests("testrun/scope1");
$TEST->add3Tests("test/scope2");
$TEST->add3Tests("test/scope3");
$TEST->add3Tests("test/scope4");
$TEST->add3Tests("testrun/scope5", "_GNUCC=1");
$TEST->add3Tests("test/voidstar");
$TEST->add3Tests("testrun/memcpy1");
$TEST->add3Tests("testrun/label1");
$TEST->add3Tests("testrun/label2");
$TEST->add3Tests("testrun/label3");
$TEST->add3Tests("testrun/addrof", "MANUALBOX=1");
$TEST->add3Tests("testrun/addrof2", "MANUALBOX=1");
$TEST->add3Tests("testrun/lval1", "_GNUCC=1");
$TEST->add1Tests("testrun/decl1", "_GNUCC=1");
$TEST->add3Tests("wes-hashtest", "", @runpattern);
$TEST->add3Tests("wes-rbtest", "", @runpattern);
$TEST->add1Test("test/alloc-manualinferbox",
                "test/alloc INFERBOX=$inferbox MANUALBOX=1",
                %commonerrors);

#
# OLDEN benchmarks
#
$TEST->add3Tests("bh", "_GNUCC=1");
   $TEST->add3Group("bh", "slow", "olden");
#   $TEST->addBadComment("bh-box", "CRASHES");

$TEST->add3Tests("power", "_GNUCC=1");
   $TEST->add3Group("power", "olden");
#   $TEST->addBadComment("power-box", "Bug in BOX");

$TEST->add3Tests("health", "_GNUCC=1");
   $TEST->add3Group("health", "olden");
#   $TEST->addBadComment("health-inferbox", "Crashes");
#   $TEST->addBadComment("health-cil", "don't know how to run");
#   $TEST->addBadComment("health-box", "Bug in BOX");

$TEST->add3Tests("perimeter");
   $TEST->add3Group("perimeter", "olden");
$TEST->add3Tests("tsp");
   $TEST->add3Group("tsp", "olden");

#
# SPEC95
#
$TEST->add3Tests("li", "_GNUCC=1");
  $TEST->add3Group("li", "slow");
  $TEST->addBadComment("li-box", "bug in box.ml");
  $TEST->addBadComment("li-inferbox", "bug in box.ml");

$TEST->add3Tests("compress", "_GNUCC=1");
  $TEST->add3Group("compress", "slow");
#   $TEST->addBadComment("compress-box", "missing wrappers");

$TEST->add3Tests("go", "_GNUCC=1");
   $TEST->add3Group("go", "slow");
   $TEST->addComment("go-box", "CRASHES (only on Win)");
#   $TEST->addBadComment("go-inferbox", "CRASHES with LBound");

$TEST->add2Tests("ijpeg", "_GNUCC=1");
  $TEST->add2Group("ijpeg", "slow");
#  $TEST->addBadComment("ijpeg-cil", "EOF / read error?? (only on Win)");
  $TEST->addBadComment("ijpeg-inferbox", "missing wrappers");

$TEST->add2Tests("m88k", "_GNUCC=1");
  $TEST->add2Group("m88k", "slow");
  $TEST->addBadComment("m88k-cil", "Don't know how to run");
  $TEST->enable("m88k-inferbox", 0); # Infinite loop
  $TEST->addBadComment("m88k-inferbox", "missing wrappers");

$TEST->add2Tests("vortex", "_GNUCC=1");
  $TEST->add2Group("vortex", "slow");
  $TEST->addBadComment("vortex-cil", "wrong input data");
  $TEST->addBadComment("vortex-inferbox", "missing WRAPPERS");

$TEST->add3Tests("apache/gzip");
   $TEST->add3Group("apache/gzip", "apache", "slow");
#   $TEST->addBadComment("apache/gzip-inferbox", "BUG");
#   $TEST->addBadComment("apache/gzip-box", "BUG");
#$TEST->add3Tests("apache/rewrite");
#   $TEST->addBadComment("apache/rewrite-cil", "missing main");
#   $TEST->add3Group("apache/rewrite", "apache");
#   $TEST->addBadComment("apache/rewrite-inferbox", "BUG");
#   $TEST->addBadComment("apache/rewrite-box", "BUG");
#   $TEST->addBadComment("apache/rewrite-cil", "BUG");
#$TEST->add3Tests("apache/urlcount");
#   $TEST->add3Group("apache/urlcount", "apache");
#   $TEST->add3BadComment("apache/urlcount", "missing include file");
#$TEST->add3Tests("apache/layout");
#   $TEST->add3Group("apache/layout", "apache");
#   $TEST->addBadComment("apache/layout-box", "BUG");
#$TEST->add3Tests("apache/random");
#   $TEST->add3Group("apache/random", "apache");
#   $TEST->addBadComment("apache/random-box", "BUG");

# scott's tiny testcases
$TEST->add3Tests("scott/multiplestatics");
$TEST->add3Tests("scott/regbeforeassign");
$TEST->add3Tests("scott/partialbracket");
$TEST->add3Tests("scott/enuminit");
$TEST->add3Tests("scott/staticafternostorage");
$TEST->add3Tests("scott/voidfree");
$TEST->add3Tests("scott/recursetype");
#   $TEST->addBadComment("scott/recursetype-box", "BUG");
#   $TEST->addBadComment("scott/recursetype-inferbox", "BUG");
$TEST->add3Tests("scott/rmunused");
$TEST->add3Tests("scott/simplewild");
$TEST->add3Tests("scott/lexnum");

$TEST->add3Tests("scott/transpunion", "_GNUCC=1");
$TEST->add3Tests("scott/oldstyle");
$TEST->add3Tests("scott/typeof", "_GNUCC=1");
$TEST->add3Tests("scott/funcname", "_GNUCC=1");
$TEST->add3Tests("scott-nolink/asmfndecl", "_GNUCC=1");
$TEST->add3Tests("scott/litstruct", "_GNUCC=1");
$TEST->add3Tests("scott/xlsubr");
$TEST->add3Tests("scott/heapify");
$TEST->add3Tests("scott/argv");
$TEST->add1Test("scott/main", "_GNUCC=1");

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
        "--noremake!", 
            );
}


sub extraHelpMessage {
    my($self) = @_;
    
    my ($scriptname, $extra) = $self->SUPER::extraHelpMessage();
    return ("testsafec",
            $extra . << "EOF");

Additional arguments for SafeC test harness
  --safecdebug         Use the debug versions of everything (default is false)
  --noremake           Does not try to remake the executable before each test.
                       (so that you can modify the sources while the test 
                       is running)
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
    return "Execution error" if $err == 1006;
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
    
    if(defined $self->{option}->{noremake}) {
        $theargs .= " NOREMAKE=1";
    }
    my $k;
    my %patterns = %commonerrors;

    $self->newTest(Name => $name . "-cil",
                   Dir => "..",
                   Cmd => "make " . $name . $theargs,
                   Group => ["cil"],
                   Patterns => \%patterns);

    $self->newTest(Name => $name . "-box",
                   Dir => "..",
                   Cmd => "make " . $name . " INFERBOX=wild MANUALBOX= " . $theargs,
                   Group => ["box"],
                   Patterns => \%patterns);

    $self->newTest(Name => $name . "-inferbox",
                   Dir => "..",
                   Cmd => "make " . $name . " INFERBOX=$inferbox " . $theargs,
                   Group => ["infer"], 
                   Patterns => \%patterns);
}

sub add2Tests {
    my($self, $name, $extraargs, %patterns) = @_;
    
    my $theargs = defined($self->{option}->{safecdebug}) ? " " : " RELEASE=1 ";
    $theargs .= " $extraargs ";
    
    if(defined $self->{option}->{noremake}) {
        $theargs .= " NOREMAKE=1";
    }
    my $k;
    my %patterns = %commonerrors;

    $self->newTest(Name => $name . "-cil",
                   Dir => "..",
                   Cmd => "make " . $name . $theargs,
                   Group => ["cil"],
                   Patterns => \%patterns);


    $self->newTest(Name => $name . "-inferbox",
                   Dir => "..",
                   Cmd => "make " . $name . " INFERBOX=$inferbox " . $theargs,
                   Group => ["infer"], 
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

sub addBadComment {
    my($self, $name, $comm) = @_;
    $self->addComment($name, $comm);
    $self->addGroups($name, "bad");
}

sub add3Comment {
    my ($self, $name, $comm) = @_;
    $self->addComment($name . "-cil", $comm);
    $self->addComment($name . "-box", $comm);
    $self->addComment($name . "-inferbox", $comm);
}

sub add2Comment {
    my ($self, $name, $comm) = @_;
    $self->addComment($name . "-cil", $comm);
    $self->addComment($name . "-inferbox", $comm);
}


sub add3BadComment {
    my ($self, $name, $comm) = @_;
    $self->add3Comment($name, $comm);
    $self->add3Group($name, "bad");
}

sub add2BadComment {
    my ($self, $name, $comm) = @_;
    $self->add2Comment($name, $comm);
    $self->add2Group($name, "bad");
}

sub add3Group {
    my ($self, $name, @groups) = @_;
    $self->addGroups($name . "-cil", @groups);
    $self->addGroups($name . "-box", @groups);
    $self->addGroups($name . "-inferbox", @groups);
}

sub add2Group {
    my ($self, $name, @groups) = @_;
    $self->addGroups($name . "-cil", @groups);
    $self->addGroups($name . "-inferbox", @groups);
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
