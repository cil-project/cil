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
my $TEST = SafecRegTest->new(AvailParams => { 'SAFE' => 1,
                                              'WILD' => 1,
                                              'FSEQ' => 1,
                                              'RUN' => 1,
                                              'CURE' => 1,
                                              'SUCCESS' => 0},
                             LogFile => "safec.log",
                             CommandName => "testsafec");

# We watch the log and we remember in what stage we are (so that we can
# interpret the error 

# Stages:
#  1000 - Start (scripts, preprocessors, etc.)
#  1001 - Parsing
#  1002 - cabs2cil
#  1003 - collecting constraints
#  1004 - solving constraints
#  1005 - Boxing file
#  1006 - Optimization
#  1007 - Compilation
#  1008 - Running 

my @runpattern = 
    ("^Run.+ ([.\\d]+)ms" => sub { $_[1]->{"run"} = $_[2]; });

my %commonerrors = 
    ("^Parsing " => sub { $_[1]->{instage} = 1001; },

     "^Converting CABS" => sub { $_[1]->{instage} = 1002; },

     "^Collecting constraints" => sub { $_[1]->{instage} = 1003; },

     "^Solving constraints" => sub { $_[1]->{instage} = 1004; },

     "^Adding run-time checks" => sub { $_[1]->{instage} = 1005; },

     "^Optimizing checks" => sub { $_[1]->{instage} = 1006; },

     "^Cure complete" => sub { $_[1]->{instage} = 1007; },

     "^Linked the cured program" => sub { $_[1]->{instage} = 1008; },

# We are seeing an error from make. Try to classify it based on the stage
# in which we are
     "^make: \\*\\*\\*" => 
     sub { 
         if($_[1]->{ErrorCode} == 0) {
             $_[1]->{ErrorCode} = $_[1]->{instage};
         }},
    
    "Syntax error" => sub { $_[1]->{ErrorCode} = 1000; },
    
         # Collect some more parameters
         # Now error messages
    "^(Bug: .+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^(Error: .+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^(Unimplemented: .+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^(.+ : error .+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^(.+:\\d+: [^w].+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^(.+: fatal error.+)\$" => sub { $_[1]->{ErrorMsg} = $_[2]; },
    "^stackdump: Dumping stack trace" => sub { $_[1]->{ErrorMsg} = $_[2]; },

#
# Now collect some parameters
    "^ptrkinds:\\s+SAFE - \\d+ \\(\\s*(\\d+)\\%" 
              => sub { $_[1]->{SAFE} = $_[2]; },
    "^ptrkinds:\\s+WILD - \\d+ \\(\\s*(\\d+)\\%" 
              => sub { $_[1]->{WILD} = $_[2]; },
    "^ptrkinds:\\s+FSEQ - \\d+ \\(\\s*(\\d+)\\%" 
              => sub { $_[1]->{FSEQ} = $_[2]; },

    "^user\\s+(\\d+)m([\\d.]+)s"
              => sub { $_[1]->{RUN} = 60 * $_[2] + $_[3]; },

    "^TOTAL\\s+([\\d.]+) s" => sub { $_[1]->{CURE} = $_[2]; },
    );

                                         
my $inferbox = "infer";

# Start with a few tests that must be run first
$TEST->newTest(
    Name => "!inittests0",
    Dir => "..",
    Cmd => "make setup");
$TEST->newTest(
    Name => "!inittests2",
    Dir => "..",
    Cmd => "make setup _GNUCC=1");

$TEST->newTest(
    Name => "apache!1setup",
    Dir => "..",
    Groups => ["apache", "slow"],
    Cmd => "make apachesetup");
$TEST->newTest(
    Name => "apache!2setup",
    Dir => "..",
    Groups => ["apache", "slow"],
    Cmd => "make apachesetup _GNUCC=1");
    
# Now add tests
$TEST->add3Tests("btreetest");
$TEST->add3Tests("hashtest");
$TEST->add3Tests("rbtest");
$TEST->add3Tests("hufftest");
$TEST->add3Tests("test/alloc");
$TEST->add3Tests("test/apachebits");
$TEST->add3Tests("testrun/apachebuf");
$TEST->add3Tests("testrun/apachefptr");
$TEST->add2Tests("testrun/asm1", "_GNUCC=1");
    $TEST->addBadComment("testrun/asm1-inferbox", 
                         "Unimplemented inline assmebly");
$TEST->addTests("test/asm2", "_GNUCC=1", ['cil']);
$TEST->addTests("test/asm3", "_GNUCC=1", ['cil']);

$TEST->add3Tests("testrun/offsetof");
$TEST->addTests("testrun/question", "", ['cil']);
$TEST->add3Tests("test/argcast");
$TEST->add3Tests("test/array1");
$TEST->add3Tests("test/array2");
$TEST->add3Tests("test/matrix");
$TEST->add3Tests("testrun/switch");
$TEST->add3Tests("testrun/caserange", "_GNUCC=1");
$TEST->add3Tests("test/attr");
$TEST->add3Tests("test/attr2", "_GNUCC=1");
    $TEST->addBadComment("test/attr2-box", "Format is a fat pointer to string");
$TEST->add3Tests("test/attr3", "_GNUCC=1");
$TEST->add3Tests("testrun/attr4", "_GNUCC=1");
$TEST->addTests("testrun/attr5", "_GNUCC=1", ['cil']);
$TEST->add3Tests("test/bh1");
$TEST->add3Tests("test/bitfield");
$TEST->add3Tests("testrun/bitfield3");
     
$TEST->add3Tests("testrun/bitfield2");
$TEST->add3Tests("test/box1");
$TEST->add3Tests("test/cast1");
$TEST->add3Tests("test/cast2");
$TEST->add3Tests("test/constprop");
$TEST->addTests("testrun/const1", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/const2", "", ['cil']);
$TEST->addTests("testrun/const3", "", ['cil']);
$TEST->addTests("testrun/const4", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/const5", "_GNUCC=1", ['cil']);
$TEST->add3Tests("test/enum");
$TEST->add3Tests("test/format1");
$TEST->add3Tests("test/func");
$TEST->add3Tests("testrun/func2");
$TEST->add3Tests("testrun/func3");
$TEST->add3Tests("testrun/func4");
$TEST->add3Tests("test/globals");
$TEST->add3Tests("test/huff1");
  $TEST->addBadComment("test/huff1-box", "pragma box misuse");
  $TEST->addBadComment("test/huff1-inferbox", "pragma box misuse");
$TEST->add3Tests("testrun/init");
$TEST->add3Tests("testrun/init1");
$TEST->addTests("testrun/init2", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/init3", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/init4", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/init5", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/init6", "", ['cil']);
$TEST->addTests("test/init8", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/init9", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/init9", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/init10", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/initial", "_GNUCC=1", ['cil']);
$TEST->add3Tests("test/jmp_buf");
$TEST->add3Tests("test/linux_atomic", "_GNUCC=1");
$TEST->add3Tests("test/li");
$TEST->add3Tests("test/li1", "_GNUCC=1");
$TEST->add3Tests("test/list");
$TEST->add3Tests("test/pointers");
$TEST->add3Tests("test/printf", "", @runpattern);
$TEST->add3Tests("test/printf_const", "", @runpattern);
$TEST->add3Tests("testrun/printf2");
$TEST->add2Tests("testrun/vararg1");
$TEST->add2Tests("testrun/vararg2");
$TEST->add2Tests("testrun/vararg3");
$TEST->add2Tests("testrun/vararg4");
$TEST->add2Tests("testrun/vararg5", "_GNUCC=1");
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
$TEST->addTests("testrun/align1", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/align2", "_GNUCC=1 EXTRAARGS=-O2", ['cil']);
$TEST->add3Tests("test/tags");
$TEST->add3Tests("test/task", "_GNUCC=1");
$TEST->add3Tests("test/power1");
$TEST->add3Tests("testrun/scope1");
$TEST->add3Tests("test/scope2");
$TEST->add3Tests("test/scope3");
$TEST->add3Tests("test/scope4");
$TEST->add3Tests("testrun/scope5", "_GNUCC=1");
$TEST->add3Tests("testrun/scope6");
$TEST->add3Tests("test/voidstar");
$TEST->add3Tests("testrun/memcpy1");
$TEST->add3Tests("testrun/memset1");
$TEST->add3Tests("testrun/poly1");
$TEST->add3Tests("testrun/poly2");
$TEST->add3Tests("testrun/poly3");
$TEST->add3Tests("testrun/polyrec");
$TEST->add3Tests("testrun/label1");
$TEST->add3Tests("testrun/label2");
$TEST->add3Tests("testrun/label3");
$TEST->add3Tests("testrun/wchar1");
$TEST->add3Tests("testrun/wchar2");
$TEST->add3Tests("testrun/wchar3");
$TEST->add3Tests("testrun/tablebug", "TABLE=A");
$TEST->add3Tests("testrun/addrof", "MANUALBOX=1");
$TEST->add3Tests("testrun/addrof2", "MANUALBOX=1");
$TEST->addTests("testrun/addrof3", "_GNUCC=1", ['cil']);
$TEST->add3Tests("testrun/lval1", "_GNUCC=1");
$TEST->add3Tests("testmodel/model1");
$TEST->add3Tests("testmodel/modelpoly");
$TEST->addTests("testrun/decl1", "_GNUCC=1", ['cil']);
$TEST->add3Tests("wes-hashtest", "");
$TEST->add3Tests("wes-rbtest", "");
$TEST->addTests("test/alloc", "MANUALBOX=1", ['inferbox']);
$TEST->add3Tests("testrun/addr-array");
$TEST->addTests("combine1", "", ['cil']);
$TEST->addTests("combine2", "", ['cil']);
$TEST->addTests("combine3", "", ['cil']);
$TEST->addTests("combine5", "", ['cil']);
$TEST->addTests("combine6", "", ['cil']);
$TEST->add2Tests("testrun/funptr1");
$TEST->addTests("testrun/typespec1", "_GNUCC=1", ['cil']);
   $TEST->addBadComment("testrun/typespec1-cil", 
                        "Must emulate bug in GCC?");
$TEST->add2Tests("testrun/wild2", "_GNUCC=1");
$TEST->addTests("testrun/returnvoid", "", ['cil']);
$TEST->addTests("testrun/returnvoid1", "", ['cil']);
$TEST->addTests("testrun/void", "_GNUCC=1", ['cil']);
$TEST->addTests("test/restrict", "EXTRAARGS=-std=c9x _GNUCC=1", ['cil']);
$TEST->addTests("test/restrict1", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/rmtmps1", "", ['cil']);
$TEST->addTests("test/proto1", "", ['cil']);
$TEST->addTests("testrun/struct1", "", ['cil']);
$TEST->addTests("testrun/voidarg", "", ['cil']);
$TEST->addTests("testrun/union2", "", ['cil']);
$TEST->addTests("testrun/union3", "", ['cil']);
   
# Tests that are expected to fail
$TEST->add2TestsFail("testrun/failubound1", "", "Failure .+: Ubound");
$TEST->add2TestsFail("testrun/failnull1", "", "Failure .+: Non-pointer");
$TEST->add2TestsFail("testrun/failprintf1", "", "Failure .+: Non-pointer");
$TEST->add2TestsFail("testrun/failprintf2", "", "Failure .+: Non-pointer");
$TEST->add2TestsFail("testrun/failprintf3", "", "Failure .+: type mismatch");
$TEST->add2TestsFail("testrun/failprintf4", "", "Failure .+: type mismatch");
$TEST->add2TestsFail("testrun/failprintf5", "", 
                     "Failure .+: Non-terminated string");
$TEST->add2TestsFail("testrun/failprintf6", "", "Failure .+: type mismatch");

$TEST->add2TestsFail("testrun/failsprintf1", "", "Failure .+: Ubound");
$TEST->add2TestsFail("testrun/failsprintf2", "", "Failure .+: ");
$TEST->add2TestsFail("testrun/failsprintf3", "", "Failure .+: Non-pointer");

$TEST->add2TestsFail("testrun/failsscanf1", "", "Failure .+: Ubound");
    $TEST->addBadComment("testrun/failsscanf1-box", "Missing wrappers");
$TEST->add2TestsFail("testrun/simon6", "", "Failure .+: Non-pointer");
    
$TEST->add2TestsFail("testrun/infer1", "", "Failure .+: ");
$TEST->addTestsFail("testrun/fseq1", "", "Failure .+: Decrement FSEQ", 
                    ['inferbox']);
$TEST->addTestsFail("testrun/fseq1", "", "Failure .+: Lbound", 
                    ['box']);
$TEST->addTestsFail("testrun/string1", "", "Failure .+: ", ['inferbox']);
$TEST->addTestsFail("testrun/fseq3", "", "Failure .+: ", ['inferbox']);

#
# OLDEN benchmarks
#
$TEST->add3Tests("bh", "_GNUCC=1");
   $TEST->add3Group("bh", "slow", "olden");

$TEST->add3Tests("power", "_GNUCC=1");
   $TEST->add3Group("power", "olden");

$TEST->add3Tests("health", "_GNUCC=1");
   $TEST->add3Group("health", "olden");

$TEST->add3Tests("perimeter");
   $TEST->add3Group("perimeter", "olden");
$TEST->add3Tests("tsp");
   $TEST->add3Group("tsp", "olden");
$TEST->add3Tests("bisort", "_GNUCC=1");
   $TEST->add3Group("bisort", "olden");
$TEST->add2Tests("mst");
   $TEST->add2Group("mst", "olden");
$TEST->add3Tests("em3d", "_GNUCC=1");
   $TEST->add3Group("em3d", "olden");
$TEST->add3Tests("treeadd", "_GNUCC=1");
   $TEST->add3Group("treeadd", "olden");

# PTR INTENSIVE BENCHMARKS 
$TEST->add2Tests("anagram", "_GNUCC=1");
   $TEST->add2Group("anagram", "ptrdist", "slow");
$TEST->add2Tests("bc", "_GNUCC=1");
   $TEST->add2Group("bc", "ptrdist", "slow");
$TEST->add2Tests("ft", "_GNUCC=1");
   $TEST->add2Group("ft", "ptrdist", "slow");
$TEST->add2Tests("ks", "_GNUCC=1");
   $TEST->add2Group("ks", "ptrdist", "slow");
$TEST->add2Tests("yacr", "_GNUCC=1");
   $TEST->add2Group("yacr", "ptrdist", "slow");

#
# SPEC95
#
$TEST->add2Tests("li", "_GNUCC=1");
  $TEST->add2Group("li", "slow", "spec");

$TEST->add3Tests("compress", "_GNUCC=1");
  $TEST->add3Group("compress", "slow", "spec");

$TEST->add3Tests("go", "_GNUCC=1");
   $TEST->add3Group("go", "slow", "spec", "vslow");

$TEST->add2Tests("ijpeg", "_GNUCC=1");
  $TEST->add2Group("ijpeg", "slow", "spec");
#  $TEST->addBadComment("ijpeg-cil", "EOF / read error?? (only on Win)");
#  $TEST->addBadComment("ijpeg-cil", "strange failure on MANJU.");

$TEST->add2Tests("m88k", "_GNUCC=1");
  $TEST->add2Group("m88k", "slow", "spec");
  $TEST->addBadComment("m88k-cil", "Don't know how to build");
  $TEST->enable("m88k-inferbox", 0); # Infinite loop
  $TEST->addBadComment("m88k-inferbox", "missing wrappers");

$TEST->add2Tests("vortex", "_GNUCC=1 OPTIM= ");
  $TEST->add2Group("vortex", "vslow", "spec");
  $TEST->addBadComment("vortex-inferbox", "bug in resetSScanf");


$TEST->add2Tests("apache/gzip");
   $TEST->add2Group("apache/gzip", "apache", "slow");
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
$TEST->addTests("scott/main", "_GNUCC=1", ['cil']);
$TEST->addTests("scott/globalprob", "_GNUCC=1", ['cil']);
$TEST->addTests("scott/bisonerror", "_GNUCC=1", ['cil']);
$TEST->addTests("scott/cmpzero", "", ['cil']);
$TEST->addTests("scott/mknod", "_GNUCC=1", ['cil']);
    $TEST->addBadComment("scott/mknod-cil", "Strange C code");
$TEST->addTests("scott/kernel1", "_GNUCC=1", ['cil']);
$TEST->addTests("scott/kernel2", "_GNUCC=1", ['cil']);
$TEST->addTests("scott/xcheckers", "_GNUCC=1", ['cil']);
$TEST->addTests("scott/memberofptr", "_GNUCC=1", ['cil']);
$TEST->addTests("scott/invalredef", "_GNUCC=1", ['cil']);
$TEST->addTests("scott/invalredef2", "_GNUCC=1", ['cil']);
$TEST->add3Tests("scott/stralloc", "_GNUCC=1", ['cil']);
$TEST->addTests("scott/errorinfn", "", ['cil']);
$TEST->addTests("scott/unionassign", "", ['inferbox', 'box']);
# $TEST->getTest("apache/gzip-inferbox")->{Enabled} = 0; # Due to a bug
# my $tst = $TEST->getTest("apache/gzip-inferbox");
# print Dumper($tst);


# print Dumper($TEST);

# Disable most tests
#foreach my $tst (keys %{$TEST->{tests}}) {
#    if($tst ne "testrun/failnull1-inferbox") {
#        print "Disabling $tst\n";
#        $TEST->{tests}->{$tst}->{Enabled} = 0;
#    } else {
#        print "Enabling $tst\n";
#        $TEST->{tests}->{$tst}->{Enabled} = 1;
#    }
#}

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
    return "Preprocessor error" if $err == 1000;
    return "Parse error" if $err == 1001;
    return "Cabs2cil error" if $err == 1002;
    return "Collecting constraints error" if $err == 1003;
    return "Constraint solving error" if $err == 1004;
    return "Boxing error" if $err == 1005;
    return "Optimization error" if $err == 1006;
    return "Compilation error" if $err == 1007;
    return "Execution error" if $err == 1008;
    return $self->SUPER::errorHeading($err);
}

sub startParsingLog {
    my($self, $tst) = @_;
    $tst->{instage} = 1000;
    $tst->{ErrorCode} = 0;
}


sub availableParameters {
    my($self) = @_;
    return %::availpars;
}


# Add a number of tests. 
# name is the base name of the tests
# extrargs are passed on the command line for each test
# kinds must be a list containint: cil, inferbox, box
# fields must be fields to be added to the newly created tests
sub addTests {
    my($self, $name, $extraargs, $pkinds, %extrafields) = @_;

    my $theargs = defined($self->{option}->{safecdebug}) 
        ? " " : " OPTIM=1 RELEASE=1 RELEASELIB=1 ";
    $theargs .= " $extraargs ";
    if(defined $self->{option}->{noremake}) {
        $theargs .= " NOREMAKE=1";
    }
    # Turn on the verbose flag
    $theargs .= " STATS=1 PRINTSTAGES=1 ";

    my %patterns = %commonerrors;
    my $kind;
    my @tests = ();
    foreach $kind (@{$pkinds}) {
        my $thisargs = $theargs;
        if($kind eq 'inferbox') {
            $thisargs .= "  INFERBOX=$inferbox ";
        }
        if($kind eq 'box') {
            $thisargs .= "  INFERBOX=wild ";
        }
        my $tst = 
            $self->newTest(Name => $name . "-" . $kind,
                           Dir => "..",
                           Cmd => "make " . $name . $thisargs,
                           Group => [$kind],
                           Patterns => \%patterns);
        # Add the extra fields
        my $key;
        foreach $key (keys %extrafields) {
            $tst->{$key} = $extrafields{$key};
        }
    }
    return @tests;
}


sub add3Tests {
    my($self, $name, $extraargs) = @_;
    return $self->addTests($name, $extraargs, ['cil', 'inferbox', 'box']);
}

sub add2Tests {
    my($self, $name, $extraargs) = @_;
    return $self->addTests($name, $extraargs, ['cil', 'inferbox']);
}

sub addTestsFail {
    my($self, $name, $extraargs, $failpattern, $pkinds) = @_;
    my @tests = $self->addTests($name, $extraargs, $pkinds, 
                                MustFail => $failpattern);
    return @tests;
}
sub add2TestsFail {
    my($self, $name, $extraargs, $failpattern) = @_;
    return $self->addTestsFail($name, $extraargs, $failpattern, 
                               ['box', 'inferbox']);
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


1;
