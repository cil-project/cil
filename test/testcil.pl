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

print "Test infrastructure for CCured and CIL\n";

# Create our customized test harness
my $TEST = SafecRegTest->new(AvailParams => { 'SAFE' => 1,
                                              'WILD' => 1,
                                              'FSEQ' => 1,
                                              'RUN' => 1,
                                              'CURE' => 1,
                                              'NODES' => 1,
                                              'SUCCESS' => 0},
                             LogFile => "safec.log",
                             CommandName => "testsafec");

# sm: I want a global name for this $TEST thing, since I find it is merely
# excess verbiage when adding tests..
$main::globalTEST = $TEST;

# am I on win32?
my $win32 = ($^O eq 'MSWin32');
my $unix = !$win32;

# am I using egcs?
my $egcs = $unix && system("gcc -v 2>&1 | grep egcs >/dev/null")==0;

# am I on manju?
my $manju = $unix && system("hostname | grep manju >/dev/null")==0;


# We watch the log and we remember in what stage we are (so that we can
# interpret the error)

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
    
    "[sS]yntax error" => sub { $_[1]->{ErrorCode} = 1000; },
    
         # Collect some more parameters
         # Now error messages
    "^((Error|Bug|Unimplemented): .+)\$" 
                      => sub { if(! defined $_[1]->{ErrorMsg}) {
                                 $_[1]->{ErrorMsg} = $_[2];} },
    "^(.+ : error .+)\$" => sub { if(! defined $_[1]->{ErrorMsg}) {
                                     $_[1]->{ErrorMsg} = $_[2];} },
    "^(.+:\\d+: (Error|Unimplemented|Bug):.+)\$" 
                     => sub { if(! defined $_[1]->{ErrorMsg}) {
                                       $_[1]->{ErrorMsg} = $_[2];} },
    "^(.+: fatal error.+)\$" => sub { if(! defined $_[1]->{ErrorMsg}) {
                                         $_[1]->{ErrorMsg} = $_[2];} },
    "^stackdump: Dumping stack trace" => 
                   sub { if(! defined $_[1]->{ErrorMsg}) {
                         $_[1]->{ErrorMsg} = $_[2];} },

#
# Now collect some parameters
    "^ptrkinds:\\s+SAFE - \\d+ \\(\\s*(\\d+)\\%" 
              => sub { $_[1]->{SAFE} = $_[2]; },
    "^ptrkinds:\\s+WILD - \\d+ \\(\\s*(\\d+)\\%" 
              => sub { $_[1]->{WILD} = $_[2]; },
    "^ptrkinds:\\s+FSEQ - \\d+ \\(\\s*(\\d+)\\%" 
              => sub { $_[1]->{FSEQ} = $_[2]; },

    "contains (\\d+) nodes" => sub { $_[1]->{NODES} = $_[2]; },

    "^user\\s+(\\d+)m([\\d.]+)s"
              => sub { $_[1]->{RUN} = 60 * $_[2] + $_[3]; },

    "^TOTAL\\s+([\\d.]+) s" => sub { $_[1]->{CURE} = $_[2]; },
    );

                                         
my $inferbox = "infer";	# weimer: "paper"

# Start with a few tests that must be run first
$TEST->newTest(
    Name => "!inittests0",
    Dir => "..",
    Cmd => "make setup",
    Group => ['ALWAYS']);
$TEST->newTest(
    Name => "!inittests2",
    Dir => "..",
    Cmd => "make setup _GNUCC=1",
    Group => ['ALWAYS']);

$TEST->newTest(
    Name => "apache!1setup",
    Dir => ".",
    Group => ["apache", "slow"],
    Cmd => "make apachesetup");
$TEST->newTest(
    Name => "apache!2setup",
    Dir => ".",
    Group => ["apache", "slow"],
    Cmd => "make apachesetup _GNUCC=1");
    
# Now add tests
$TEST->addTests("testrun/const-array-init", "WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->add3Tests("btreetest");
$TEST->add3Tests("hashtest");
$TEST->add3Tests("rbtest");
$TEST->add3Tests("hufftest");
$TEST->add3Tests("test/alloc");
$TEST->add3Tests("test/apachebits");
$TEST->add3Tests("testrun/apachebuf");
$TEST->add3Tests("testrun/alloc2");
$TEST->add3Tests("testrun/alloc3");
$TEST->add3Tests("testrun/apachefptr");
$TEST->add2Tests("testrun/asm1", "_GNUCC=1");
    # sm: this one works for me
    #$TEST->addBadComment("testrun/asm1-inferbox",
    #                     "Unimplemented inline assmebly");
$TEST->addTests("test/asm2", "_GNUCC=1", ['cil']);
$TEST->addTests("test/asm3", "_GNUCC=1", ['cil']);
$TEST->addTests("test/asm4", "_GNUCC=1", ['cil']);

$TEST->add3Tests("testrun/offsetof");
$TEST->addTests("testrun/question", "", ['cil']);
$TEST->add3Tests("test/argcast");
$TEST->add3Tests("test/array1");
$TEST->add3Tests("test/array2");
$TEST->add3Tests("test/matrix");
$TEST->add3Tests("testrun/switch");
$TEST->add3Tests("testrun/strloop");
$TEST->add3Tests("testrun/caserange", "_GNUCC=1");
if (!$egcs) {
  $TEST->add3Tests("test/attr");
  $TEST->add3Tests("test/attr2", "_GNUCC=1");
      $TEST->addBadComment("test/attr2-box", 
                           "Format is a fat pointer to string");
  $TEST->add3Tests("test/attr3", "_GNUCC=1");
  $TEST->add3Tests("testrun/attr4", "_GNUCC=1");
  $TEST->addTests("testrun/attr5", "_GNUCC=1", ['cil']);
}
$TEST->add3Tests("test/bh1");
$TEST->add3Tests("test/bitfield");
$TEST->add3Tests("testrun/bitfield3");
     
$TEST->add3Tests("testrun/bitfield2");
$TEST->add3Tests("test/box1");
$TEST->add3Tests("test/cast1");
$TEST->add3Tests("test/cast2");
$TEST->add2Tests("test/cast4", "_GNUCC=1");
$TEST->add3Tests("test/constprop");
$TEST->addTests("testrun/const1", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/const2", "", ['cil']);
$TEST->addTests("testrun/const3", "", ['cil']);
$TEST->addTests("testrun/const4", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/const5", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/const6", "", ['cil']);
$TEST->add2Tests("test/deref", "_GNUCC=1");
$TEST->add3Tests("test/enum");
$TEST->add3Tests("testrun/enum2");
$TEST->add3Tests("test/format1");
$TEST->add3Tests("test/func");
$TEST->add3Tests("testrun/func2");
$TEST->add3Tests("testrun/func3");
$TEST->add3Tests("testrun/func4");
$TEST->add3Tests("testrun/func5");
$TEST->add3Tests("test/globals");
$TEST->add3Tests("testrun/float");
$TEST->add3Tests("testrun/ptr1");
$TEST->add3Tests("test/huff1");
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
$TEST->addTests("testrun/init11", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/init12", "_GNUCC=1", ['cil']);
$TEST->add2Tests("testrun/init13", "_GNUCC=1");
$TEST->add2Tests("testrun/init14", "_GNUCC=1");
$TEST->add2Tests("testrun/init15", "_GNUCC=1");
$TEST->addTests("testrun/cond1", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/cond2", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/initial", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/inline1", "_GNUCC=1", ['cil']);
$TEST->addTests("test/decl2", "_GNUCC=1", ['cil']);
$TEST->add3Tests("test/jmp_buf");
$TEST->add3Tests("test/linux_atomic", "_GNUCC=1");
$TEST->add3Tests("test/li");
$TEST->add3Tests("test/li1", "_GNUCC=1");
$TEST->add3Tests("test/list");
$TEST->add2Tests("testrun/perror");
$TEST->add2Tests("testrun/perror1");
$TEST->add2Tests("test/pure");
$TEST->add3Tests("test/pointers");
$TEST->add3Tests("test/printf", "", @runpattern);
$TEST->add3Tests("test/printf_const", "", @runpattern);
$TEST->add3Tests("testrun/printf2");
$TEST->addTests("testrun/safeunion", "", ['inferbox']);
$TEST->add3Tests("testrun/solver1");
    $TEST->addBadComment("testrun/solver1-inferbox", 
                         "Code uses __builtin_next_arg directly");
    $TEST->addBadComment("testrun/solver1-box", 
                         "Code uses __builtin_next_arg directly");
$TEST->add2Tests("test/unimplemented");
$TEST->add2Tests("testrun/vararg1");
$TEST->add2Tests("testrun/vararg2");
$TEST->add2Tests("testrun/vararg3");
$TEST->add2Tests("testrun/vararg4");
$TEST->add2Tests("testrun/vararg5", "_GNUCC=1");
if (!$egcs) {
  $TEST->add2Tests("testrun/vararg6");
}
$TEST->add2Tests("testrun/va-arg-1", "_GNUCC=1");
$TEST->add2Tests("testrun/va-arg-2", "_GNUCC=1");
if (!$egcs) {
  $TEST->add2Tests("testrun/va-arg-7", "_GNUCC=1");
}
$TEST->addTests("testrun/comma1", "_GNUCC=1", ['cil']);
$TEST->add3Tests("test/retval");
$TEST->add3Tests("test/seq");
$TEST->addTestsFail("test/seq2", "Lbound", ['inferbox']);
$TEST->add3Tests("testrun/sized");
$TEST->addTestsFail("testrun/sized2", [], "Initializing SIZED open array", ['inferbox']);
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
$TEST->add3Tests("testrun/scope8");
$TEST->addTests("testrun/scope9", "", ['cil']);
$TEST->add3Tests("test/voidstar");
$TEST->add3Tests("testrun/memcpy1");
$TEST->add3Tests("testrun/memset1");
$TEST->add3Tests("testrun/poly1");
$TEST->add3Tests("testrun/poly2");
$TEST->add3Tests("testrun/poly3");
$TEST->add3Tests("testrun/polypeek1");
$TEST->add3Tests("testrun/polypeek2");
$TEST->add3Tests("testrun/polypeek3");
$TEST->add3Tests("testrun/polyapply1");
$TEST->add3Tests("testrun/polyapply2");
$TEST->add3Tests("testrun/polyapply3");
$TEST->add3Tests("testrun/polyrec");
$TEST->addTests("testrun/polylist", "", ['inferbox']);
$TEST->addTests("test-bad1/polystruct", "", ['inferbox']);
$TEST->addTests("test-bad1/fseqfail", "", ['inferbox']);
$TEST->addTests("test-bad/globinit", "", ['inferbox']);
$TEST->add3Tests("testrun/label1");
$TEST->add3Tests("testrun/label2");
$TEST->add3Tests("testrun/label3");
$TEST->add2Tests("testrun/label4", "_GNUCC=1");
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
$TEST->add2Tests("testwrapper/wrapper1");
$TEST->add2Tests("testwrapper/wrapperpoly");
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
$TEST->addTests("combine8", "", ['cil']);
$TEST->addTestsFail("combine9", "", "Incompatible declaration for g", ['cil']);
$TEST->addTests("combine10", "", ['cil']);
$TEST->addTests("combine11", "", ['cil']);
$TEST->addTests("combine12", "", ['cil']);
$TEST->addTests("combine13", "", ['cil']);
$TEST->addTests("combine14", "", ['cil']);
$TEST->addTests("combine15", "", ['cil']);
$TEST->addTests("combine16", "", ['cil']);
$TEST->addTests("combine17", "", ['cil']);
$TEST->addTests("combine18", "", ['cil']);
$TEST->addTests("combineenum1", "", ['cil']);
$TEST->addTests("combineenum2", "", ['cil']);
$TEST->addTests("combineenum3", "", ['cil']);
$TEST->addTests("combineinline1", "", ['cil']);
$TEST->addTests("combineinline2", "", ['cil']);
$TEST->addTests("combineinline3", "", ['cil']);
$TEST->addTests("combineinline4", "", ['cil']);
$TEST->addTests("combineinline6", "", ['cil']);
$TEST->addTests("combinestruct1", "", ['cil']);
$TEST->addTests("test/linuxcombine1_1", "", ['cil']);

$TEST->addTests("arcombine", "_GNUCC=1", ['cil']);
$TEST->add2Tests("testrun/funptr1");
$TEST->addTests("testrun/typespec1", "_GNUCC=1", ['cil']);
   $TEST->addBadComment("testrun/typespec1-cil", 
                        "Must emulate bug in GCC?");
$TEST->add2Tests("testrun/wild2", "_GNUCC=1");
$TEST->addTests("testrun/wild3", "_GNUCC=1", ['inferbox']);
$TEST->addTests("testrun/returnvoid", "", ['cil']);
$TEST->addTests("testrun/returnvoid1", "", ['cil']);
$TEST->addTests("testrun/return1", "", ['cil']);
$TEST->addTests("testrun/for1", "", ['cil']);
$TEST->addTests("testrun/void", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/wrongnumargs", "", ['cil']);
if (!$egcs) {
  $TEST->addTests("test/restrict", "EXTRAARGS=-std=c9x _GNUCC=1", ['cil']);
  $TEST->addTests("test/restrict1", "_GNUCC=1", ['cil']);
}
$TEST->addTests("testrun/rmtmps1", "", ['cil']);
$TEST->addTests("testrun/rmtmps2", "_GNUCC=1", ['cil']);
$TEST->addTests("test/proto1", "", ['cil']);
$TEST->addTests("test/proto2", "", ['cil']);
   $TEST->addBadComment("test/proto2-cil", 
                        "Bug in parser (precedences)");
$TEST->addTests("testrun/struct1", "", ['cil']);
$TEST->addTests("testrun/voidarg", "", ['cil']);
$TEST->addTests("testrun/union2", "", ['cil']);
$TEST->addTests("testrun/union3", "", ['cil']);
$TEST->addTests("testrun/inline1", "", ['cil']);
$TEST->addTests("testrun/tcast2", "", ['inferbox']);
$TEST->addTests("testrun/pointerdiff", "", ['cil', 'inferbox', 'box']);
   $TEST->addBadComment("testrun/pointerdiff-inferbox", 
                        "A pointer that is not read should not be bound-checked");
$TEST->addTests("test/cpp-2", "", ['cil']);
   $TEST->addBadComment("test/cpp-2-cil", 
                        "Bug in parser (empty pragmas)");

$TEST->addTestsFail("testrun/struct3", "", "Non-pointer", ['inferbox']);

if($^O eq 'MSWin32') {
    $TEST->addTests("testrun/extern_init", "_MSVC=1", ['cil']);   
}

# Tests that are expected to fail
$TEST->add2TestsFail("testrun/failubound1", "", "Failure: Ubound");
$TEST->add2TestsFail("testrun/failnull1", "", "Failure:");
$TEST->add2TestsFail("testrun/failprintf1", "", "Failure: Non-pointer");
$TEST->add2TestsFail("testrun/failprintf2", "", "Failure: Non-pointer");
$TEST->add2TestsFail("testrun/failprintf3", "", "Failure: type mismatch");
$TEST->add2TestsFail("testrun/failprintf4", "", "Failure: type mismatch");
$TEST->add2TestsFail("testrun/failprintf5", "", 
                     "Failure: Non-terminated string");
$TEST->add2TestsFail("testrun/failprintf6", "", "Failure: type mismatch");

$TEST->add2TestsFail("testmodel/noproto1", "", "Function pointer");
$TEST->add2TestsFail("testmodel/noproto2", "", "Failure: Non-pointer");
# $TEST->add2Tests("testmodel/noproto");

# sm: worksforme: $TEST->add2TestsFail("testrun/failsprintf1", "", "Failure .+: Ubound");
$TEST->add2Tests("testrun/failsprintf1", "");
$TEST->add2TestsFail("testrun/failsprintf2", "", "Failure: ");
$TEST->add2TestsFail("testrun/failsprintf3", "", "Failure: Non-pointer");

$TEST->add2TestsFail("testrun/failsscanf1", "", "Failure: Ubound");
    $TEST->addBadComment("testrun/failsscanf1-box", "Missing wrappers");
$TEST->add2TestsFail("testrun/simon6", "", "Failure:");
    
$TEST->add2TestsFail("testrun/infer1", "", "Failure: ");
    $TEST->addBadComment("testrun/infer1-inferbox", "Unsound solver casting of SEQ pointers");
$TEST->addTestsFail("testrun/fseq1", "", "Failure: Decrement FSEQ", 
                    ['inferbox']);
$TEST->addTestsFail("testrun/fseq1", "", "Failure: Lbound", 
                    ['box']);
$TEST->addTestsFail("testrun/string1", "", "Failure: Ubound", ['inferbox']);
$TEST->addTestsFail("testrun/fseq3", "", "Failure: Integer arithmetic overflow", ['inferbox']);
$TEST->addTests("test-bad/badpoly", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/polyfunptr", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/polylist", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/poly2", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/castnoedge", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/checkret", "_GNUCC=1 RELEASE=", [ 'inferbox' ]);
$TEST->addTests("test-bad/checkstore", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/checkinit", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/union2", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/override", "", [ 'inferbox' ]);
$TEST->addTests("test-bad/wild1", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("scott/union5", "", ['inferbox']);
$TEST->addTests("scott/funptr1", "", ['inferbox']);
$TEST->addTests("testrun/unrolltype", "", ['inferbox']);
$TEST->addTests("testrun/wrapper2", "", ['cil', 'inferbox', 'box']);
$TEST->addTests("testrun/fseqn1", "", ['inferbox']);
$TEST->addTests("testrun/seqn1", "", ['inferbox']);

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
  $TEST->add2Group("vortex", "vslow", "spec", "slow");
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

# -------------- scott's testcases --------------
# sm: trying to make a regrtest-like interface
# 'args' should include things like "INFERBOX=infer" to specify operating mode
sub smAddTest {
  if (scalar(@_) != 1) {
    print STDERR "wrong number of args to smAddTest: @_\n";
    exit 2;
  }

  my $self = $main::globalTEST;
  my ($command) = @_;
  my ($name, $args) = ($command =~ /^(\S+) ?(.*)$/);     # name is first word
  my $tname = $self->uniqueName($name);

  my %patterns = %commonerrors;
  my $tst = $self->newTest(Name => $tname,
                           Dir => ".",
                           Cmd => "make $command" . $self->testCommandExtras(""),
                           Group => ['quick'],
                           Patterns => \%patterns);

  return $tname;
}

sub addCXXTest {
    my $tname = &smAddTest(@_);
    my $self = $main::globalTEST;
    $self->addGroups($tname, 'cxx');
}

# here 'why' is a human-readable explanation for why the test fails,
# rather than a regexp to match the error message because:
#   - I don't care if the error message changes while it's a failing test
#   - sometimes the message is machine- or compiler-dependent
#   - I want a human-readable explanation
sub smFailTest {
  my ($why, $command) = @_;

  if (!$command) {
      # presumably 'why' is actually the intended command
      print STDERR ("You forgot to give a reason for $why\n");
      exit 2;
  }

  my $tname = smAddTest($command);
  $main::globalTEST->addBadComment($tname, $why);

  return $tname;
}

sub failCXXTest {
    my $tname = &smFailTest(@_);
    my $self = $main::globalTEST;
    $self->addGroups($tname, 'cxx');
}


# operating modes
my $box =       "INFERBOX=infer";
my $manualbox = "INFERBOX=infer";
my $wildbox =   "INFERBOX=wild";
my $table =     "INFERBOX=wild TABLE=A";
my $gcc =       "_GNUCC=1";     # sm: not sure where/why this is needed

# self-contained tests of specific things which had problems before
smAddTest("scott/multiplestatics");
smAddTest("scott/regbeforeassign $box");
smAddTest("scott/partialbracket");
smAddTest("scott/enuminit");
smAddTest("scott/staticafternostorage $box");
smAddTest("scott/voidfree $box");
smAddTest("scott/recursetype $box");
smAddTest("scott/rmunused $box $gcc");
smAddTest("scott/simplewild $box");
smAddTest("scott/ptrtolocal $wildbox");
smAddTest("scott/tprintf $box");
smAddTest("scott/rmunused2 $box");

smAddTest("scott/gimpdouble");
smAddTest("scott/struct_cs");

$TEST->setField(smAddTest("scott/tprintf $wildbox"), "FailDiagnosis", <<'EOF');

  Failure of this test case usually indicates the stdin/out/err
  section of the patch file (lib/ccured_???.patch) does not exactly
  match your /usr/include/stdio.h.

EOF

smAddTest("scott/ptrmanip $wildbox");
smAddTest("scott/bogus_redef");
smAddTest("scott/s59");
smAddTest("scott/putc $gcc");
smAddTest("scott/putc $wildbox $gcc");
smAddTest("scott/lexnum");
smAddTest("scott/ctype");
smAddTest("scott/ctype $box");
smAddTest("test-bad/wildfun $box");

# verify results of inference
$TEST->setField(smAddTest("scott/ptrkinds $box"), "AfterSuccessScript", <<'EOF');

  grepBoth() {
    grep -w $2 $1 | grep -w $3
  }

  # looks like we no longer print __SAFE with safe pointers...
  #grepBoth $src safeptr __SAFE && \

  src=small2/ptrkindscured.c
  if grepBoth $src fseqptr __FSEQ && \
     grepBoth $src seqptr __SEQ && \
     grepBoth $src wildptr __WILD; then
    echo "inference seems to work"
  else
    echo ""
    echo "Something is wrong with the inference algorithm."
    exit 2
  fi

EOF

# similar to ptrkinds failure
smAddTest("scott/argv $box");

# function pointers don't work with inferred wildness
smAddTest("scott/funcptr");
smAddTest("scott/funcptr $wildbox");
smAddTest("scott/funcptr $box");

# transparent unions are a problem for network apps
smAddTest("scott/transpunion $gcc");
smAddTest("scott/sockaddr $gcc");

# test of recent __HEAPIFY annotation
smAddTest("scott/heapify $box");
smAddTest("scott/heapify $wildbox");

# misc...
smAddTest("scott/constdecl");
smAddTest("scott/oldstyle");
smAddTest("scott/typeof $gcc");
smAddTest("scott/asmfndecl $gcc");
smAddTest("scott/xlsubr $box");
smAddTest("scott/open $gcc");
smAddTest("scott/ioctl $box $gcc");
smAddTest("scott/stralloc $box $gcc");
smAddTest("scott/mknod $box $gcc");
smAddTest("badd/nullfield $manualbox");
smAddTest("scott/constfold");
smAddTest("scott/mode_sizes $gcc");       # mode(__QI__) stuff
smAddTest("scott-nolink/brlock $gcc");
smAddTest("scott/qsort_wild $box");
smAddTest("scott/regparm0 $gcc");         # this works, unfortunately..
smAddTest("scott/unscomp");               # kernel/fs/buffer.c
smAddTest("scott/suppress_optim $box");
smAddTest("scott/suppress_optim $wildbox");
smFailTest("makes too many things tagged; this changed when matth modified "
           . "the way main() gets its arguments wrapped, and we haven't "
           . "cared enough about TAGALLFNS=1 to fix it",
           "scott/suppress_optim $wildbox TAGALLFNS=1");
smAddTest("testrun/bug1 $box");
smAddTest("scott/structs_edg_stl_ccuredlib_test $box");
smAddTest("misc-tests");
smAddTest("scott/chararr1 $box");
smAddTest("scott/chararr2 $box");
smAddTest("scott/thing");
smAddTest("scott/strerror1 $box");

# current problematic test cases
smAddTest("scott/complex_float $box");
smAddTest("mergeinline");
smAddTest("test/addrofparam $box");
smAddTest("scott-nolink/name-capture-bitand $box");
smAddTest("scott-nolink/wildfun2 $box");
smAddTest("scott/dblarg.int $box");       # this yields a warning that might be a problem
smAddTest("scott/decl_inl $box");         # produces a gcc warning I'd like to silence
smFailTest("infers a safe ptr argument to __throw_setup",
           "doThrowFv $wildbox UNTAGGEDFNS=1");
smAddTest("scott/uninit_tmp");
smAddTest("test-tagfile $wildbox TAGFILE=tagfile.txt");
smAddTest("test-tagfile $wildbox TAGFILE=tagfile.txt EXTRAARGS=-DSTATIC_FUNC");
smAddTest("scott/monthname $box");
smFailTest("problem with gcc coercions",
           "scott/floatarg INFERBOX=wild TAGALLFNS=1");
smFailTest("problem with over-aggressive pointer checks?",
           "scott/ptrarith INFERBOX=infer");
smAddTest("combine_samefn");
smAddTest("combine_node_alloc");
smAddTest("combine_sbump");
smAddTest("combine_sbumpB");
smAddTest("combine_sbumpB MERGEINLINES=1");
smAddTest("combine_allocate");
smAddTest("combine_allocate MERGEINLINES=1");
smAddTest("combine_theFunc");
smAddTest("combine_theFunc MERGEINLINES=1");
smAddTest("combine_syserr");
smAddTest("combine_syserr MERGEINLINES=1");
smAddTest("combine_copyptrs WARNINGS_ARE_ERRORS=1");
smAddTest("combine_copyptrs WARNINGS_ARE_ERRORS=1 MERGEINLINES=1");
smAddTest("merge-twice");
smAddTest("scott/arrayexpand INFERBOX=infer");
smAddTest("scott/byteprintf INFERBOX=infer");
smFailTest("inference bug with function pointers",
           "scott/bufferlinegetter INFERBOX=infer");

# tests of things implemented for EDG compatibility
smAddTest("mergestruct");
smAddTest("test-bad/globstackptr $box");
smAddTest("test-bad/ehstack $box");
smAddTest("test-bad/setjmp $box");
smAddTest("combinetaggedfn $wildbox SEPARATE=1 UNTAGGEDFNS=1");

# test of strings (need more!)
smFailTest("unsound user annotation RWSTRING", "badd/ovwrnull $box");
smAddTest("test-bad/strloop2 $box");

# tests of function models
smAddTest("scott/memcpy $box");
smAddTest("scott/realloc $box");
smAddTest("scott/strchr $box");
smAddTest("scott/models $box");

# tests of things in safec.c
smAddTest("scott/qsort $box");
smAddTest("scott/strpbrk $box");
smAddTest("scott/fgets $box");
smAddTest("test-bad/sockets $box $gcc");

# more stuff, mostly from ftpd
if ($TEST->{option}->{safecdebug}) {
  smAddTest("scott/reply $box");
}
else {
  smFailTest("problem with __extinline and varargs", "scott/reply $box");
}

# works on my machine; works on manju now too apparently
smAddTest("scott/getpwnam $box $gcc");

smAddTest("test-bad/execv $box $gcc");
$TEST->setField(smAddTest("scott/popen $box $gcc"),
                "FailDiagnosis", 
                "inferred glob_t probably has unanticipated type");
smAddTest("scott/memset_int $box");
smAddTest("scott/printfllong $box $gcc");
smAddTest("test-bad/replydirname $box");
smAddTest("test-bad/boundaries $box");
smAddTest("scott/stat $box");
smAddTest("scott/scanf $box");

# simple self-contained thing
smAddTest("hola");
smAddTest("hola $box");

# a few things that should fail
smAddTest("badd/lbound $box $gcc");
smAddTest("badd/ubound $box $gcc");
smAddTest("badd/fseq $box $gcc");
smAddTest("badd/calloc $box $gcc");
smAddTest("badd/stackaddr $box $gcc");
smAddTest("test-bad/trivial-tb");
smAddTest("test-bad/retptr $box RELEASE=");

# verify we have tags and that they work
# smAddTest("badd/nonptr $box $gcc");          # very simple test. Covered by wild1
smAddTest("scott/arraytags $box $gcc");     # this one is pretty hairy

# simple test of combiner
smAddTest("comb $gcc");
smAddTest("comb $box $gcc");

# test combiner's ability to detect inconsistency
smAddTest("baddef");

# test for lean fats ("table")
smAddTest("scott/ptrtolocal $table");
smAddTest("scott/ptrinint $table");
smAddTest("scott/simplewild $table");
smAddTest("scott/ptrmanip $table");
smAddTest("scott/regthenprintf $table");
smAddTest("scott/twoprintfs $table");
smAddTest("scott/nested $table");

# hashtest and rbtest with TABLE
my $iters = "EXTRAARGS=-DITERS=100";
$TEST->setField(smAddTest("rbtest $table $iters"),
                "FailDiagnosis", "Maybe ARCHOS isn't set right?");
smAddTest("hashtest $table $iters");

# red-black tree
smAddTest("rbtest $iters");
smAddTest("rbtest $box $iters");

smAddTest("wes-rbtest $iters");
smAddTest("wes-rbtest $box $iters");

# hashtable
$iters = "EXTRAARGS=-DITERS=10000";
smAddTest("hashtest $iters");
smAddTest("hashtest $box $iters");
smAddTest("hashtest $wildbox $iters");

smAddTest("wes-hashtest $iters");
smAddTest("wes-hashtest $box $iters");

# some piece of PCC
smAddTest("testpcc/parseobject EXTRAARGS=--no-idashi");

# apache modules; set is needed for next one
$TEST->setField(smAddTest("apache!1setup"), 'Cmd', "make apachesetup");
$TEST->setField(smAddTest("apache!2setup"), 'Cmd', "make apachesetup");
smAddTest("apache/gzip");

# does not work: complains of many incompatible type redefinitions
#runTest $make apache/rewrite

# The following block of tests was transferred from testsafec.pl to
# the regrtest script, and now has been transferred back.
# I keep it here because for each test I just run one mode
# (whichever mode has shown itself to be possibly an issue in
# the past, or CIL-only when I have no data), so it's faster
# than the add3Tests above.
smAddTest("test/list");
smAddTest("test/alloc");
smAddTest("test/array1");

if ($egcs) {
  # these fail because I'm using egcs instead of gcc 2.95
  smFailTest("parse error on cil output", "test/attr");
  smFailTest("cast specifies function type", "test/attr $wildbox");
  smFailTest("parse error on cil output", "test/attr3");
  smFailTest("cast specifies function type", "test/attr3 $wildbox");
}
else {
  smAddTest("test/attr");
  smAddTest("test/attr $wildbox");
  smAddTest("test/attr3");
  smAddTest("test/attr3 $wildbox");
}

smAddTest("test/attr4 $box");
smAddTest("test/bitfield");
smAddTest("test/box1");
smAddTest("test/cast1");
smAddTest("test/constprop");
smAddTest("test/enum");
smAddTest("test/format1");
smAddTest("test/func");
smAddTest("test/globals");
smAddTest("test/init");
smAddTest("test/init $box");
smAddTest("test/initial");
smAddTest("test/jmp_buf");
smAddTest("test/linux_atomic");
smAddTest("test/list");
smAddTest("test/pointers");
smAddTest("test/printf");
smAddTest("test/retval");
smAddTest("test/seq");
smAddTest("test/sized");
smAddTest("test/sizeof");
smAddTest("test/smallstring");
smAddTest("test/static");
smAddTest("test/strcpy");
smAddTest("test/string");
smAddTest("test/struct_init");
smAddTest("test/structassign");
smAddTest("test/tags");
smAddTest("test/task");
smAddTest("test/voidstar");
# end copied block of tests

# more random stuff
smAddTest("scott/funcname $gcc");
smAddTest("scott/litstruct $gcc");
smAddTest("scott/main $gcc");
smAddTest("scott/globalprob $gcc");
smAddTest("scott/bisonerror $gcc");
smAddTest("scott/cmpzero");
smAddTest("scott/kernel1 $gcc");
smAddTest("scott/kernel2 $gcc");
smAddTest("scott/xcheckers $gcc");
smAddTest("scott/memberofptr $gcc");
smAddTest("scott/invalredef $gcc");
smAddTest("scott/invalredef2 $gcc");
smAddTest("scott/errorinfn");
smAddTest("scott/unionassign $box");
smAddTest("scott/unionassign $wildbox");

# C++ tests
# sm: most of these fail for me, with parse errors..
failCXXTest("parse error", "cxx/hello");
failCXXTest("parse error", "cxx/exc1");
failCXXTest("parse error", "cxx/exspec1");
failCXXTest("parse error", "cxx/structname");
failCXXTest("parse error", "cxx/class1");

# $TEST->getTest("apache/gzip-inferbox")->{Enabled} = 0; # Due to a bug
# my $tst = $TEST->getTest("apache/gzip-inferbox");
# print Dumper($tst);

# ---------------- c-torture -------------
## if we have the c-torture tests add them
## But only if the ctorture group was specfied
my $ctorture = '/usr/local/src/gcc/gcc/testsuite/gcc.c-torture';
if(-d $ctorture && 
   defined $TEST->{option}->{group} &&
    grep { $_ eq 'ctorture'} @{$TEST->{option}->{group}}) {
    
    # Omit some tests because they use __complex__
    my @omit = ('compile/20000804-1', 'compile/20001222-1', 'compile/941019-1',
                'compile/981223-1', 'compile/991213-1', 'compile/20010605-2',
                'compile/960512-1', 'compile/complex-1', 
                'compile/complex-2', 'compile/complex-4', 
                'compile/complex-5', 'execute/complex-2', 'execute/complex-5',
                'execute/960512-1', 'execute/complex-4', 
                'execute/complex-1', 'execute/20010605-2');

    # Also omit those with inner functions
    push @omit, 
    ('compile/951116-1', 'compile/920415-1',
     'execute/920415-1', 'compile/20010605-1', 
     'execute/20010605-1', 'compile/20011023-1',
     'compile/20010903-2', 'execute/comp-goto-2', 'execute/nestfunc-2',
     'execute/921215-1', 'execute/920428-2', 'execute/921017-1',
     'execute/nest-stdar-1', 'execute/nestfunc-3', 'execute/920501-7', 
     'execute/920721-4', 'execute/920612-2', 'execute/20010209', 
     'execute/931002-1', 'execute/nestfunc-1', 'execute/20000822-1',
     'compile/930506-2', 'execute/20010209-1');

    # Read the compile tests 
   my @tortures;
   foreach my $tortdir ('compile', 'execute', 'compat') { 
       @tortures = 
           map { $_ =~ m|$ctorture/$tortdir/(.+)\.c|; $1 } 
                 (glob "$ctorture/$tortdir/*.c");
       # Remove those that were produced in previous runs
       @tortures = grep { $_ !~ m|cil$| } @tortures;
       # Remove those that we know should fail
       @tortures = grep { my $t = "$tortdir/$_"; 
                          ! grep { $_ =~ m|$t|} @omit } @tortures;
       foreach my $tst (@tortures) {
           $TEST->addTests("tort/$tortdir/$tst", "_GNUCC=1", ['cil']); 
           $TEST->addGroups("tort/$tortdir/$tst-cil", 'ctorture');
       }
   }
}

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


# given the current options configuration, return a string of
# additional 'make' arguments to append to test commands
sub testCommandExtras {
    my ($self, $extraargs) = @_;

    # (sm: pulled this out of addTests so I could write my own addTests)
    my $theargs = defined($self->{option}->{safecdebug})
        ? " " : " OPTIM=1 RELEASE=1 RELEASELIB=1 ";
    $theargs .= " $extraargs ";
    if(defined $self->{option}->{noremake}) {
        $theargs .= " NOREMAKE=1";
    }
    # Turn on the verbose flag
    $theargs .= " STATS=1 PRINTSTAGES=1 ";

    return $theargs;
}


# Add a number of tests.
# name is the base name of the tests
# extrargs are passed on the command line for each test
# kinds must be a list containing: cil, inferbox, box
# fields must be fields to be added to the newly created tests
sub addTests {
    my($self, $name, $extraargs, $pkinds, %extrafields) = @_;

    my $theargs = $self->testCommandExtras($extraargs);

    my %patterns = %commonerrors;
    my $kind;
    my @tests = ();
    foreach $kind (@{$pkinds}) {
        my $thisargs = $theargs;
        if($kind eq 'inferbox') {
            $thisargs .= "  INFERBOX=$inferbox"; #ww: "EXTRAARGS=--typecheck";
        }
        if($kind eq 'box') {
            $thisargs .= "  INFERBOX=wild ";
        }
        my $tst =
            $self->newTest(Name => $name . "-" . $kind,
                           Dir => ".",
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


# ensure uniqueness of names (I don't like using these names to
# name tests.. regrtest used numbers.. oh well)
sub uniqueName {
  my ($self, $name) = @_;

  if (!$self->testExists($name)) {
    return $name;   # already unique
  }
  else {
    my $ct = 2;
    while ($self->testExists($name . $ct)) {
      $ct++;
    }
    return $name . $ct;
  }
}

1;


