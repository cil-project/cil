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
use lib "$FindBin::Bin/../ocamlutil";

use RegTest;

$ENV{LANG} = 'C';

print "Test infrastructure for CCured and CIL\n";

# Create our customized test harness
my $TEST = SafecRegTest->new(AvailParams => { 'SAFE' => 1,
                                              'WILD' => 1,
                                              'FSEQ' => 1,
                                              'CHK_RET' => 1,
                                              'CHK_STR' => 1,
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
my $win32 = ($^O eq 'MSWin32' || $^O eq 'cygwin');
my $unix = !$win32;
my $solaris = $^O eq 'solaris';

# am I using egcs?
my $egcs = $unix && system("gcc -v 2>&1 | grep egcs >/dev/null")==0;

# am I on manju?
my $manju = $unix && system("hostname | grep manju >/dev/null")==0;

my $make;
if ($solaris) {
    $make = "gmake";
} else {
    $make = "make";
}

# We watch the log and we remember in what stage we are (so that we can
# interpret the error)

# Stages:
#  1000 - Start (scripts, preprocessors, etc.)
#  1001 - Parsing
#  1002 - cabs2cil
#  1003 - collecting constraints
#  1004 - solving constraints
#  1005 - Curing file
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

#     "^\\s*CHECK_NULL\\s+(\\d+)" => sub { $_[1]->{CHK_NULL} = $_[2]; },

    "^\\s*CHECK_RETURNPTR\\s+(\\d+)" => sub { $_[1]->{CHK_RET} = $_[2]; },

    "^\\s*CHECK_STOREPTR\\s+(\\d+)" => sub { $_[1]->{CHK_STR} = $_[2]; },

    "^user\\s+(\\d+)m([\\d.]+)s"
              => sub { $_[1]->{RUN} = 60 * $_[2] + $_[3]; },

    "^TOTAL\\s+([\\d.]+) s" => sub { $_[1]->{CURE} = $_[2]; },
    );

                                         
my $inferbox = "infer";	# weimer: "paper"

# Start with a few tests that must be run first
$TEST->newTest(
    Name => "!inittests0",
    Dir => "..",
    Cmd => "$make setup",
    Group => ['ALWAYS']);
$TEST->newTest(
    Name => "!inittests2",
    Dir => "..",
    Cmd => "$make setup _GNUCC=1",
    Group => ['ALWAYS']);

$TEST->newTest(
    Name => "apache!1setup",
    Dir => ".",
    Group => ["apache", "slow"],
    Cmd => "$make apachesetup");
$TEST->newTest(
    Name => "apache!2setup",
    Dir => ".",
    Group => ["apache", "slow"],
    Cmd => "$make apachesetup _GNUCC=1");

# build the documentation
$TEST->newTest(
    Name => "doc",
    Dir => "..",
    Cmd => "$make doc",
    Group => ["doc"]);
    
# Now add tests
$TEST->addTests("testrun/const-array-init", "WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->addTests("testrun/const-struct-init", "WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->addTests("test/const-struct-init", "WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->addTests("testrun/warnings-noreturn", "WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->addTests("testrun/warnings-unused-label", "WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->addTests("test/warnings-cast", "WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->addTests("test_withtrusted/deepcopy1", "", ['inferbox']);
$TEST->addTests("test_withtrusted/deepcopy3", "", ['inferbox']);
$TEST->addTests("test/compat1", "", ['inferbox']);
$TEST->addTests("testrun/compat2", "", ['inferbox']);
$TEST->addTests("testrun/pointsto", "", ['inferbox']);
$TEST->addTests("testrun/trusted1", "", ['inferbox']);
$TEST->addTests("testrun/hostent", "", ['inferbox'],
                Group => [ 'slow' ]);
$TEST->addTests("testrun/hostent2", "", ['inferbox'],
                Group => [ 'slow' ] );
$TEST->add3Tests("btreetest");
   $TEST->add3Group("btreetest", "slow");
$TEST->add3Tests("hashtest");
   $TEST->add3Group("hashtest", "slow");
$TEST->add3Tests("rbtest");
   $TEST->add3Group("rbtest", "slow");
$TEST->add2Tests("hufftest");
   $TEST->add2Group("hufftest", "slow");
$TEST->add3Tests("test/alloc");
$TEST->add3Tests("test/apachebits");
$TEST->add3Tests("testrun/apachebuf");
$TEST->add3Tests("testrun/alloc2");
$TEST->addTests("testrun/alloc3", "", ['inferbox']);
$TEST->add3Tests("testrun/apachefptr");
$TEST->add2Tests("testrun/asm1", "EXTRAARGS=--allowInlineAssembly _GNUCC=1");
    # sm: this one works for me
    #$TEST->addBadComment("testrun/asm1-inferbox",
    #                     "Unimplemented inline assmebly");
$TEST->addTests("test/asm2", "_GNUCC=1", ['cil']);
$TEST->addTests("test/asm3", "_GNUCC=1", ['cil']);
$TEST->addTests("test/asm4", "_GNUCC=1", ['cil']);
$TEST->addTests("testobj/asm5", "_GNUCC=1", ['cil']);

$TEST->add3Tests("testrun/offsetof");
$TEST->add3Tests("testrun/offsetof1");
$TEST->add3Tests("testrun/offsetof2");
$TEST->addTests("testrun/question", "", ['cil']);
$TEST->add3Tests("test/argcast");
$TEST->add3Tests("test/array1");
$TEST->add3Tests("test/array2");
$TEST->addTests("testrun/array3", "", ['inferbox']);
$TEST->add3Tests("test/matrix");
$TEST->add3Tests("testrun/switch");
$TEST->add3Tests("testrun/strloop");
$TEST->add2Tests("testrun/strloop3");
$TEST->add2Tests("testrun/percentm");
$TEST->add2Tests("testrun/percent400");
$TEST->add2Tests("testrun/functattr");
$TEST->add3Tests("testrun/caserange", "_GNUCC=1");
if (!$egcs) {
  $TEST->add3Tests("test/attr");
  $TEST->add3Tests("test/attr2", "_GNUCC=1");
  $TEST->add3Tests("test/attr3", "_GNUCC=1");
  $TEST->add3Tests("testrun/attr4", "_GNUCC=1");
  $TEST->addTests("testrun/attr5", "_GNUCC=1", ['cil']);
}
$TEST->addTests("test/attr6", "_GNUCC=1", ['cil']);
$TEST->addTests("test/attr7", "_GNUCC=1", ['cil']);
$TEST->addTests("test/attr8", "_GNUCC=1", ['cil']);
$TEST->add3Tests("test/bh1");
$TEST->add3Tests("test/bitfield");
$TEST->add3Tests("testrun/bitfield3");
     
$TEST->add3Tests("testrun/bitfield2");
$TEST->add3Tests("test/box1");
$TEST->addTests("testrun/call1", "", ['inferbox']);
$TEST->addTests("testrun/call2", "", ['cil']);
$TEST->add3Tests("test/cast1");
$TEST->add3Tests("test/cast2");
$TEST->add2Tests("test/cast4", "_GNUCC=1");
$TEST->addTestsFail("test/cast5", "Failure UBOUND", ['inferbox', 'box']);
$TEST->addTestsFail("test/cast6", "Failure UBOUND", ['inferbox', 'box']);
$TEST->addTests("testrun/cast7", "", ['inferbox']);
$TEST->addTests("testrun/cast8", "", ['cil']);
$TEST->add3Tests("test/constprop");
$TEST->addTests("testrun/const1", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/const2", "", ['cil']);
$TEST->addTests("testrun/const3", "", ['cil']);
$TEST->addTests("testrun/const4", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/const5", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/const6", "", ['cil']);
$TEST->addTests("test/const7", "", ['cil']);
$TEST->addTests("testrun/const8", "", ['cil']);
$TEST->addTests("test/const9", "", ['cil']);
$TEST->addTests("testrun/const10", "", ['cil']);
$TEST->addTests("testrun/const11", "", ['cil']);
$TEST->add2Tests("test/deref", "_GNUCC=1");
$TEST->add3Tests("test/enum");
$TEST->add3Tests("testrun/enum2");
$TEST->add3Tests("test/format1");
$TEST->add3Tests("test/func");
$TEST->addTests("test/splitargs","WARNINGS_ARE_ERRORS=1", ['inferbox']);
$TEST->addTests("test/funcarg", "", ['cil']);
   $TEST->addBadComment("test/funcarg-cil", "Bug in parser (argument of function type)");

$TEST->add3Tests("testrun/func2");
$TEST->add3Tests("testrun/func3");
$TEST->add3Tests("testrun/func4");
$TEST->add3Tests("testrun/func5");
$TEST->add3Tests("testrun/func6");
$TEST->addTests("testrun/func7", "", ['inferbox']);
$TEST->addTests("testrun/func8", "", ['inferbox']);
$TEST->add2Tests("testrun/func9");
#fixed:   $TEST->addBadComment("testrun/func9-inferbox", "When we take the address of a polymorphic function, the address gets \"dummyNode\" as its node, which causes us to not add some edges.");
$TEST->add3Tests("test/globals");
$TEST->add3Tests("testrun/float");
$TEST->addTests("testrun/float2", "", ['cil']);
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
$TEST->add2Tests("testrun/init16", "");
$TEST->addTests("testrun/init17", "", ['cil']);
$TEST->addTests("test/array-size-trick", "", ['cil']);
$TEST->add2Tests("testrun/logical", "");
$TEST->addTests("testrun/cond1", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/cond2", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/initial", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/inline1", "_GNUCC=1", ['cil']);
$TEST->addTests("test/decl2", "_GNUCC=1", ['cil']);
$TEST->add3Tests("test/jmp_buf");
$TEST->add3Tests("test/linux_atomic", "_GNUCC=1");
$TEST->add3Tests("testrun/linux_signal", "_GNUCC=1");
$TEST->add3Tests("test/li");
$TEST->add3Tests("test/li1", "_GNUCC=1");
$TEST->add3Tests("test/list");
$TEST->addTests("testrun/localinit", "", ['cil']);

$TEST->addTests('testrun/longBlock', '', ['cil']);
$TEST->add2Tests("testrun/perror");
$TEST->add2Tests("testrun/perror1");
$TEST->add2Tests("test/pure");
$TEST->add3Tests("test/pointers");
$TEST->addTests("testrun/post-assign", "", ['cil']);
   $TEST->addBadComment("testrun/post-assign-cil", 
                        "CIL does not have the same evaluation order for ++ as gcc");
$TEST->add3Tests("test/printf", "", @runpattern);
$TEST->add3Tests("test/printf_const", "", @runpattern);
$TEST->add3Tests("testrun/printf2");
$TEST->addTests("testrun/safeunion", "", ['inferbox']);
$TEST->add3Tests("testrun/solver1");
$TEST->add2Tests("test/unimplemented");
$TEST->add2Tests("testrun/vararg1");
$TEST->add2Tests("testrun/vararg2");
$TEST->add2Tests("testrun/vararg3");
$TEST->add2Tests("testrun/vararg4");
if($win32) {
  $TEST->add2Tests("testrun/vararg11", "_MSVC=1");
}
$TEST->add2Tests("testrun/varargauto1");
$TEST->add2Tests("testrun/vararg5", "_GNUCC=1");
$TEST->add2Tests("testrun/vararg-tagged1");
if (!$egcs) {
  $TEST->add2Tests("testrun/vararg6");
}
$TEST->add2Tests("test/vararg7", "_GNUCC=1");
$TEST->addTests("testrun/vararg8", "", ['inferbox']);
$TEST->addTests("testrun/vararg9", "", ['cil', 'inferbox']);
$TEST->add2Tests("testrun/va-arg-1", "_GNUCC=1");
$TEST->add2Tests("testrun/va-arg-2", "_GNUCC=1");
if (!$egcs) {
  $TEST->add2Tests("testrun/va-arg-7", "_GNUCC=1");
}
$TEST->addTests("test-bad/vararg", "", ['inferbox']);
$TEST->addTests("test-bad/arrsize", "", ['cil']);
$TEST->addTests("testrun/comma1", "_GNUCC=1", ['cil']);
$TEST->add3Tests("test/retval");
$TEST->add3Tests("test/seq");
$TEST->addTestsFail("test/seq2", "Failure LBOUND", ['inferbox']);
$TEST->addTestsFail("test/fseq4fail", "Failure DECFSEQ", ['inferbox']);
$TEST->add3Tests("testrun/sized");
$TEST->addTestsFail("testrun/sized2", "", 
    "Initializing SIZED open array", ['inferbox']);
$TEST->add3Tests("test/sizeof");
$TEST->add3Tests("test/smallstring");
$TEST->add3Tests("testrun/static", "", @runpattern);
$TEST->add3Tests("test/static1");
$TEST->addTests("testrun/static2", "", ['cil']);
$TEST->add3Tests("test/strcpy");
$TEST->addTests("test-bad/strcpy1", "", ['inferbox']);
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
$TEST->addTests("testrun/scope10", "", ['cil']);
$TEST->addTests("testrun/scope11", "", ['cil']);
$TEST->add3Tests("test/voidstar");
$TEST->add3Tests("testrun/memcpy1");
$TEST->add3Tests("testrun/memset1");
$TEST->addTests("testrun/memcpy2", "", ['inferbox'],
                Group => [ 'slow' ]);
$TEST->add3Tests("testrun/poly1");
$TEST->add3Tests("testrun/poly2");
$TEST->add3Tests("testrun/poly3");
$TEST->add3Tests("testrun/polypeek1");
$TEST->add3Tests("testrun/polypeek2", "RELEASE=");
$TEST->add3Tests("testrun/polypeek3");
$TEST->add3Tests("testrun/polyapply1");
$TEST->add3Tests("testrun/polyapply2");
$TEST->add3Tests("testrun/polyapply3");
$TEST->add3Tests("testrun/polyrec");
$TEST->addTests("testrun/polylist", "", ['inferbox']);
$TEST->addTests("test-bad1/polystruct", "", ['inferbox']);
  $TEST->addBadComment("test-bad1/polystruct-inferbox", 
                       "Polymorphic structures seem to be broken");
$TEST->addTests("test-bad1/helpers", "", ['inferbox']);
$TEST->addTests("test-bad1/helpers2", "", ['inferbox']);
$TEST->addTests("testrun/polystruct2", "", ['inferbox']);
$TEST->addTests("test-bad/fseq1fail", "", ['inferbox']);
$TEST->addTests("test-bad/seqalign", "", ['inferbox']);
$TEST->addTests("test-bad/globinit", "", ['inferbox']);
$TEST->addTests("test-bad/index1", "", ['inferbox']);
$TEST->addTests("test-bad/stackptr", "", ['inferbox']);
$TEST->addTests("test-bad1/escape", "", ['inferbox']);
$TEST->addTests("test-bad1/alias", "", ['inferbox']);
$TEST->addTests("test-bad1/overflow", "", ['inferbox']);

$TEST->addTests("test-bad/nonptr1", 
                "EXTRAARGS=--logNonPointers", ['inferbox']);
$TEST->addTests("test-bad/asm1", 
                "EXTRAARGS=--allowInlineAssembly", ['inferbox']);

$TEST->addTests("test-bad/size1", "", ['inferbox']);
$TEST->addTests("runall/size2", "", ['inferbox']);
$TEST->addTests("runall/size3", 
                "EXTRAARGS=--noUnrefPointerChecks", ['inferbox']);
$TEST->addTests("runone/size4", "", ['inferbox']);
$TEST->addTests("runall/size5", 
                "EXTRAARGS=--noUnrefPointerChecks", ['inferbox']);
$TEST->addTests("runall/endannot", 
                "EXTRAARGS=--noUnrefPointerChecks", ['inferbox']);
$TEST->addTests("test/noreturn", "", ['cil']);
                
$TEST->addTests("test-bad-ln/handler1",
    "CCURED_ERROR_HANDLERS=handler1.handlers FAILISVERBOSE=1", ['inferbox']);

$TEST->addTests("runall/cilreturn", "", ['inferbox']);
$TEST->addTests("runall/strings", "", ['inferbox']);
$TEST->addTests("runall/strings-bill", "", ['inferbox']);
$TEST->addTests("runall/strings-jeremy", "", ['inferbox']);
$TEST->addTests("runall/strings-jeremy2", "", ['inferbox']);
$TEST->addTests("runall/strings-zach", "", ['inferbox']);
  $TEST->addBadComment("runall/strings-jeremy-inferbox", 
                       "Fails on Manju because Manju doesn't have strlcat");
$TEST->addTests("runall/nullterm", "", ['inferbox']);
$TEST->addTests("scott/nullterm2", "", ['inferbox']);
$TEST->addTests("scott/nullterm3", "", ['inferbox']);
$TEST->addTests("runall/sentinel", "", ['inferbox']);


$TEST->addTests("test-bad/fieldaddr", "", ['inferbox']);
$TEST->add3Tests("testrun/label1");
$TEST->add3Tests("testrun/label2");
$TEST->add3Tests("testrun/label3");
$TEST->add2Tests("testrun/label4", "_GNUCC=1");
$TEST->add3Tests("testrun/wchar1");
$TEST->add3Tests("testrun/wchar2");
$TEST->add3Tests("testrun/wchar3");
$TEST->add3Tests("testrun/wchar4");
$TEST->addTests("testrun/wchar5", "", ['cil']);
$TEST->add2Tests("testrun/wchar6"); 
$TEST->add3Tests("testrun/wchar7"); 
$TEST->add2Tests("testrun/escapes");
$TEST->addTests("test-bad1/wchar-bad", "", ['cil']);
$TEST->add3Tests("testrun/addrof", "MANUALBOX=1");
$TEST->add3Tests("testrun/addrof2", "MANUALBOX=1");
$TEST->addTests("testrun/addrof3", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/addrof4", "", ['inferbox']);
$TEST->add3Tests("testrun/lval1", "_GNUCC=1");
$TEST->add3Tests("testrun/bind1", "EXTRAARGS=--assumePrintf");
$TEST->add3Tests("test/bind2", "EXTRAARGS=--allowInlineAssembly");
   $TEST->add3Group("test/bind2", "slow");
# $TEST->add3Tests("testmodel/model1");
# $TEST->add3Tests("testmodel/modelpoly");
$TEST->add2Tests("testwrapper/wrapper1");
$TEST->add2Tests("testwrapper/wrapperpoly");
$TEST->addTests("test-bad-wrapper/wrapper1", "", ['inferbox']);
$TEST->addTests("test-bad-wrapper/wrapper2", "", ['inferbox']);
$TEST->addTests("test-bad-crypt/crypt", "", ['inferbox']);
$TEST->addTests("testrun/decl1", "_GNUCC=1", ['cil']);
$TEST->addTests("wes-hashtest", "", ['cil', 'inferbox'], 
                Group => [ 'slow' ]);
$TEST->add3Tests("wes-rbtest", "");
  $TEST->add3Group("wes-rbtest", "slow");
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
$TEST->addTests("combine20", "", ['cil']);
$TEST->addTests("combine21", "", ['cil']);
$TEST->addTests("combine22", "", ['cil']);
$TEST->addTests("combinelibrik", "", ['cil']);
$TEST->addTests("combineenum1", "", ['cil']);
$TEST->addTests("combineenum2", "", ['cil']);
$TEST->addTests("combineenum3", "", ['cil']);
$TEST->addTests("combineinline1", "", ['cil']);
$TEST->addTests("combineinline2", "", ['cil']);
$TEST->addTests("combineinline3", "", ['cil']);
$TEST->addTests("combineinline4", "", ['cil']);
$TEST->addTests("combineinline6", "", ['cil']);
$TEST->addTests("combinestruct1", "", ['cil']);
$TEST->addTests("testrun/math1", "", ['cil']);
$TEST->addTests("test/linuxcombine1_1", "", ['cil']);

$TEST->addTests("arcombine", "_GNUCC=1", ['cil']);
$TEST->add2Tests("testrun/funptr1");
$TEST->addTests("testrun/funptr2", "", ['inferbox']);
$TEST->addTests("testrun/funptralloc1", "", ['inferbox']);
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
   $TEST->addBadComment("testrun/wrongnumargs-cil", 
                        "Should fail since we don't pad argument lists");
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
$TEST->addTests("testrun/union4", "", ['inferbox']);
$TEST->addTests("test/union5", "", ['cil']);
$TEST->addTests("testrun/inline1", "", ['cil']);
$TEST->addTests("testrun/tcast2", "", ['inferbox']);
$TEST->addTests("testrun/rtti1", "", ['inferbox']);
$TEST->addTests("testrun/rtti2", "", ['inferbox']);
$TEST->addTests("testrun/rtti3", "", ['inferbox']);
$TEST->addTests("testrun/rtti4", "", ['inferbox']);
$TEST->addTests("testrun/rtti5", "", ['inferbox']);
$TEST->addTests("testrun/rtti6", "", ['inferbox']);
$TEST->addTests("testrun/rtti7", "", ['inferbox']);
$TEST->addTests("testrun/rtti8", "", ['inferbox']);
$TEST->addTests("test-bad1/rtti9", "", ['inferbox']);
$TEST->addTests("test/rtti10", "", ['inferbox']);
$TEST->addTests("testrun/rttioo", "", ['inferbox']);
$TEST->addTests("testrun/rttioo2", "", ['inferbox']);
$TEST->addTests("test/bind-cannot-convert", "", ['inferbox']);
$TEST->addTests("test/bind-used-not-defined", "", ['inferbox']); # Superseeded
                                                                 # by oneret
$TEST->addTests("testrun/oneret", "", ['inferbox']);
$TEST->addTests("test/bind-too-many", "", ['inferbox']);
$TEST->addTests("testrun/split1", "", ['inferbox']);
$TEST->addTests("testrun/rmtmps-attr", "", ['cil']);
   $TEST->addBadComment("testrun/rmtmps-attr-cil", 
                        "A limitation of our support for attributes");
 
$TEST->add3Tests("testrun/vsp");
$TEST->addTests("testrun/vsp1", "", ['inferbox']);
$TEST->addTests("testrun/strtoul", "", ['inferbox'], Group => [ 'slow']);

$TEST->addTests("test/bind-formatstring", "EXTRAARGS=--assumePrintf",
                ['inferbox'],
                Group => [ 'slow' ]);
$TEST->addTests("test/bind-empty-chain", "", ['inferbox']);
$TEST->addTests("test/bind-safe-wild", "EXTRAARGS=--assumePrintf", ['inferbox']);
$TEST->addTests("test/bind-zero", "EXTRAARGS=--assumePrintf", ['inferbox']);
$TEST->addTests("testrun/pointerdiff", "", ['cil', 'inferbox', 'box']);
$TEST->addTests("test/cpp-2", "", ['cil']);
   $TEST->addBadComment("test/cpp-2-cil", 
                        "Bug in parser (empty pragmas)");
$TEST->addTests("test/cpp-3", "_GNUCC=1", ['cil']);

$TEST->addTests("testrun/field1", "", ['inferbox']);
   $TEST->addBadComment("testrun/field1-inferbox", 
                        "Bug in handling of unsafe unions?");

$TEST->addTests("testrun/openssl-bounds", "", ['inferbox']);
   $TEST->addBadComment("testrun/openssl-bounds-inferbox", 
                        "FSEQ2SAFE prevents code that should be legal.");


$TEST->addTestsFail("testrun/struct3", "", "Failure NULL", ['inferbox']);

if($win32) {
    $TEST->addTests("testrun/extern_init", "_MSVC=1", ['cil']);   
    $TEST->addTests("testrun/msvc2", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc3", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc4", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc6", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc7", "_MSVC=1", ["cil", "inferbox"]);
    $TEST->addTests("testrun/msvc8", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc9", "_MSVC=1", ["cil"]);

    $TEST->addTests("test-bad/try1", "_MSVC=1", ["cil", "inferbox"]);
}
$TEST->addTests("testrun/msvc1", "", ["cil"]);
$TEST->addTests("testrun/msvc5", "", ["cil"]);

$TEST->addTests("testrun/extern1", "", ['cil']);

$TEST->addTests("test/duplicate", "", ['cil']);

# Tests that are expected to fail
$TEST->add2TestsFail("testrun/failubound1", "", "Failure UBOUND");
$TEST->add2TestsFail("testrun/failnull1", "", "Failure");
$TEST->add2TestsFail("testrun/failprintf1", "", "Failure NONPTR");
$TEST->add2TestsFail("testrun/failprintf2", "", "Failure NONPTR");
$TEST->add2TestsFail("testrun/failprintf3", "", "Failure VARARGBAD");
$TEST->add2TestsFail("testrun/failprintf4", "", "Failure VARARGBAD");
$TEST->add2TestsFail("testrun/failprintf5", "", 
                     "Failure UBOUND");
$TEST->add2TestsFail("testrun/failprintf6", "", "Failure VARARGBAD");
$TEST->add2TestsFail("testrun/demo1", "", "Failure UBOUND");
$TEST->add2TestsFail("testrun/demo2", "", "Failure UBOUND");
$TEST->add2TestsFail("testrun/demo3", "", "Failure UBOUND");
$TEST->add2TestsFail("testrun/demo4", "", "Failure LBOUND");

# $TEST->add2TestsFail("testmodel/noproto1", "", "Function pointer");
# $TEST->add2TestsFail("testmodel/noproto2", "", "Failure: Non-pointer");
# $TEST->add2Tests("testmodel/noproto");

$TEST->add2TestsFail("testrun/failsprintf1", "", "Failure UBOUND");
$TEST->add2TestsFail("testrun/failsprintf2", "", "Failure");
$TEST->add2TestsFail("testrun/failsprintf3", "", "Failure LBOUND");

$TEST->add2TestsFail("testrun/failsscanf1", "", "Failure UBOUND");
    $TEST->addBadComment("testrun/failsscanf1-box", "Missing wrappers");
$TEST->add2Tests("testrun/simon6");
    
$TEST->add2TestsFail("testrun/infer1", "", "Failure ");
$TEST->addTestsFail("testrun/fseq1", "", "Failure DECFSEQ", 
                    ['inferbox']);
$TEST->addTestsFail("testrun/fseq1", "", "Failure LBOUND", 
                    ['box']);
$TEST->addTestsFail("testrun/string1", "", "Failure UBOUND", ['inferbox']);
$TEST->addTestsFail("testrun/string2", "", "Failure UBOUND", ['inferbox']);
$TEST->addTestsFail("testrun/string3", "", "Failure UBOUND", ['inferbox']);
$TEST->addTestsFail("testrun/fseq3", "", "Failure NONPTR", ['inferbox']);
$TEST->addTests("test-bad/badpoly", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/polyfunptr", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/polylist", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/poly2", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/castnoedge", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("runall/checkret", "_GNUCC=1 RELEASE=", [ 'inferbox' ]);
$TEST->addTests("test-bad/checkstore", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/checkstore3", "RELEASE=", [ 'inferbox' ]);
$TEST->addTests("test-bad/checkinit", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/union2", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/metabug3", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/voidstarint", "", ['inferbox']);
$TEST->addTests("test-bad/override", "", [ 'inferbox' ]);
$TEST->addTests("test-bad/wild1", "_GNUCC=1", [ 'inferbox' ]);
$TEST->addTests("test-bad/union4", "", [ 'inferbox' ]);
$TEST->addTests("test-bad/union6", "", [ 'inferbox' ]);
$TEST->addTests("test-bad/union7", "", [ 'inferbox' ]);
  $TEST->addBadComment("test-bad/union7-inferbox", 
                       "Discriminated unions not yet implemented");
$TEST->addTests("runall/union8", "", [ 'inferbox' ]);
$TEST->addTests("test-bad/malloc1", "", [ 'inferbox' ]);
$TEST->addTests("scott/union5", "", ['inferbox']);
$TEST->addTests("scott/funptr1", "", ['inferbox']);
$TEST->addTests("testrun/unrolltype", "", ['inferbox']);
$TEST->addTests("testrun/wrapper2", "", ['cil', 'inferbox']);
$TEST->addTests("testrun/fseqn1", "", ['inferbox']);
$TEST->addTests("testrun/ubound1", "", ['inferbox']);
$TEST->addTests("test/longunion", "", ['inferbox']);
$TEST->addTests("testrun/fseq5", "", ['inferbox']);
$TEST->addTests("testrun/recur1", "", ['inferbox']);
$TEST->addTests("testrun/recur2", "", ['inferbox']);
# $TEST->addTests("testrun/seqn1", "", ['inferbox']);
$TEST->addTests("test_heapify", "", ['cil']);
$TEST->addTests("testrun/scanf2", "", ['inferbox']);
$TEST->addTests("testrun/scanf3", "", ['inferbox']);
    $TEST->addBadComment("testrun/scanf3-inferbox", "ccured_fscanf_string is too consrevative");
$TEST->addTests("testrun/scanf4", "", ['inferbox']);
$TEST->add2Tests("testrun/stringsize");
$TEST->addTests("testrun/argv2", "", ['inferbox']);


$TEST->addTests("test/simplify_structs1", 
                "USECILLY=1 EXTRAARGS=--dosimplify",  ['cil']);
$TEST->addTests("testrun/simplify_structs2", 
                "USECILLY=1 EXTRAARGS=--dosimplify",  ['cil']);

$TEST->addTests("testrun/typeof1", "", ['cil']);
$TEST->addTests("testrun/semicolon", "_GNUCC=1", ['cil']);
$TEST->addTests("testrun/oom", "", ['inferbox']);

$TEST->add2Tests("merge-ar", "");
#
# OLDEN benchmarks
#
$TEST->add2Tests("bh", "EXTRAARGS=--allowInlineAssembly _GNUCC=1");
   $TEST->add2Group("bh", "slow", "olden");

$TEST->add2Tests("power", "EXTRAARGS=--allowInlineAssembly _GNUCC=1");
   $TEST->add2Group("power", "olden", "slow");

$TEST->add2Tests("health", "_GNUCC=1");
   $TEST->add2Group("health", "olden", "slow");

$TEST->add3Tests("perimeter");
   $TEST->add3Group("perimeter", "olden", "slow");
$TEST->add3Tests("tsp");
   $TEST->add3Group("tsp", "olden", "slow");
$TEST->add2Tests("bisort", "_GNUCC=1");
   $TEST->add2Group("bisort", "olden", "slow");
$TEST->add2Tests("mst");
   $TEST->add2Group("mst", "olden", "slow");
$TEST->add2Tests("em3d", "_GNUCC=1");
   $TEST->add2Group("em3d", "olden", "slow");
$TEST->add2Tests("treeadd", "_GNUCC=1");
   $TEST->add2Group("treeadd", "olden", "slow");

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
$TEST->add2Tests("li", "EXTRAARGS=--allowInlineAssembly _GNUCC=1");
  $TEST->add2Group("li", "slow", "spec");

$TEST->add2Tests("compress", "_GNUCC=1");
  $TEST->add2Group("compress", "slow", "spec");

$TEST->add2Tests("go", "_GNUCC=1");
   $TEST->add2Group("go", "slow", "spec");

$TEST->add2Tests("ijpeg", "_GNUCC=1");
  $TEST->add2Group("ijpeg", "slow", "spec");

#$TEST->add2Tests("vortex", "_GNUCC=1 OPTIM= ");
#  $TEST->add2Group("vortex", "vslow", "spec", "slow");
#  $TEST->addBadComment("vortex-inferbox", "bug in resetSScanf");


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

$TEST->add2Tests("testrun/sizeof1");
$TEST->add2Tests("testrun/sizeof2");
$TEST->addTests("test/outofmem", "", ['cil']);
$TEST->addTests("testrun/seq1", "", ['inferbox']);
$TEST->addTests("testrun/seq2", "", ['inferbox']);
$TEST->addTests("testrun/builtin", "", ['cil']);
$TEST->addTests("test/builtin2", "", ['cil']);
$TEST->addTests("testrun/builtin3", "", ['cil']);
$TEST->addTests("blockattr", "", ['cil']);
$TEST->add2Tests("testrun/comparisons");
    

$TEST->add2Tests("cfrac");
   $TEST->add2Group("cfrac", "slow");
$TEST->add2Tests("matxmult");

#---------------- some performance tests 
$TEST->add2Tests("perf/perfglobarray");
$TEST->add2Tests("perf/perffseq");
$TEST->add2Tests("perf/perffseq1");
$TEST->add2Tests("perf/perfindex");
$TEST->add2Tests("perf/perfseq");
$TEST->add2Tests("perf/perfwild");
$TEST->add2Tests("perf/perfrtti");


# VSLOW tests
#$TEST->addTests("linux", "", ['cil']);
#  $TEST->addGroups("linux-cil", 'vslow');
#$TEST->addTests("linux-merge3", "", ['cil']);
#  $TEST->addGroups("linux-merge3-cil", 'vslow');
#$TEST->newTest(
#    Name => "emacs",
#    Dir => ".",
#    Cmd => "$make emacs",
#    Group => ['vslow'],
#    Patterns => []);
#$TEST->addTests("perl", "", ['cil']);
#  $TEST->addGroups("perl-cil", 'vslow');
#$TEST->addTests("bind", "", ['cil']);
#  $TEST->addGroups("bind-cil", 'vslow');
#$TEST->addTests("wuftpd", "", ['cil']);
#  $TEST->addGroups("wuftpd-cil", 'vslow');
#$TEST->addTests("openssh", "", ['cil']);
#  $TEST->addGroups("openssh-cil", 'vslow');
#$TEST->addTests("apache", "", ['cil']);
#  $TEST->addGroups("apache-cil", 'vslow');


#
# GIMP and friends
#
$TEST->newTest(
    Name => "gimpall-world", # A CIL only test
    Dir => ".",
    Enabled => 1,
    Cmd => "$make gimpall-world LD_LIBRARY_PATH=$FindBin::Bin/../gimp/lib",
    Group => ['vslow'],
    Patterns => \%commonerrors);

#
# Apache CIL-ified
#
$TEST->newTest(
    Name => "apache-cil", # A CIL only test
    Dir => ".",
    Enabled => 1,
    Cmd => "$make apache-cil",
    Group => ['vslow'],
    Patterns => \%commonerrors);
# Apache-CIL,  Modules-Cured
$TEST->newTest(
    Name => "apache-modules-cured", 
    Dir => ".",
    Enabled => 0,
    Cmd => "$make apache-modules-ccured",
    Group => ['vslow'],
    Patterns => \%commonerrors);

#
# PING
#
$TEST->newTest(
    Name => "ping-cil",
    Dir => ".",
    Cmd => "$make ping " . $TEST->testCommandExtras(""),
    Enabled => 1,
    Group => ['vslow'],
    Patterns => \%commonerrors);
$TEST->newTest(
    Name => "ping-inferbox",
    Dir => ".",
    Cmd => "$make ping " . $TEST->testCommandExtras("INFERBOX=infer"),
    Enabled => 1,
    Group => ['vslow'],
    Patterns => \%commonerrors);

#
# FTPD
#
$TEST->newTest(
    Name => "ftpd-cil",
    Dir => ".",
    Cmd => "$make ftpd " . $TEST->testCommandExtras(""),
    Enabled => 1,
    Group => ['vslow'],
    Patterns => \%commonerrors);
$TEST->newTest(
    Name => "ftpd-inferbox",
    Dir => ".",
    Cmd => "$make ftpd " . $TEST->testCommandExtras("INFERBOX=infer"),
    Enabled => 1,      # sm: 9/09/02 05:02: it works!
    Group => ['vslow'],
    Patterns => \%commonerrors);


#
# ACE
#
$TEST->newTest(
    Name => "ace",
    Dir => "/home/necula/ex/ace.edg",
    Cmd => "$make regtest-clean regtest",
    Enabled => 1,
    Group => ['vslow'],
    Patterns => \%commonerrors);

#
# Sendmail
#
$TEST->newTest(
    Name => "sendmail-cil",
    Dir => ".",
    Cmd => "$make sendmail " . $TEST->testCommandExtras(""),
    Enabled => 1,
    Group => ['vslow'],
    Patterns => \%commonerrors);
$TEST->newTest(
    Name => "sendmail-inferbox",
    Dir => ".",
    Cmd => "$make sendmail " . $TEST->testCommandExtras("INFERBOX=infer"),
    Enabled => 1,
    Group => ['vslow'],
    Patterns => \%commonerrors);


#
# openssl
#
$TEST->newTest(
    Name => "openssl-cil",
    Dir => ".",
    Cmd => "$make openssl-test-withclean " . $TEST->testCommandExtras(""),
    Enabled => 1,
    Group => ['vslow'],
    Patterns => \%commonerrors);
$TEST->newTest(
    Name => "openssl-inferbox",
    Dir => ".",
    Cmd => "$make openssl-test-withclean " . $TEST->testCommandExtras("INFERBOX=infer"),
    Enabled => 1,
    Group => ['vslow'],
    Patterns => \%commonerrors);

#
# SciMark2 benchmark from NIST, used in a paper submitted to Usenix that
# compared itself to CCured. 
#
$TEST->add2Tests("testrunlm/scimark2", "-lm");
   $TEST->add2Group("testrunlm/scimark2", "slow");

# -------------- alternate testcase interface ---------------
# sm: trying to make a regrtest-like interface
# 'args' should include things like "INFERBOX=infer" to specify operating mode
sub altAddTest {
  my ($command, @groups) = @_;

  my $self = $main::globalTEST;
  my ($name, $args) = ($command =~ /^(\S+) ?(.*)$/);     # name is first word
  my $tname = $self->uniqueName($name);

  if(scalar(@groups) == 0) { 
      @groups = ( 'alt' );
  } 
  my %patterns = %commonerrors;
  my $tst = $self->newTest(Name => $tname,
                           Dir => ".",
                           Cmd => "$make $command" . $self->testCommandExtras(""),
                           Group => [ @groups ],
                           Patterns => \%patterns);

  return $tname;
}


# here 'why' is a human-readable explanation for why the test fails,
# rather than a regexp to match the error message because:
#   - I don't care if the error message changes while it's a failing test
#   - sometimes the message is machine- or compiler-dependent
#   - I want a human-readable explanation
sub altFailTest {
  my ($why, $command) = @_;

  if (!$command) {
      # presumably 'why' is actually the intended command
      print STDERR ("You forgot to give a reason for $why\n");
      exit 2;
  }

  my $tname = altAddTest($command);
  $main::globalTEST->addBadComment($tname, $why);

  return $tname;
}


# operating modes
my $box =       "INFERBOX=infer";
my $manualbox = "INFERBOX=infer";
my $wildbox =   "INFERBOX=wild";
my $gcc =       "_GNUCC=1";     # sm: not sure where/why this is needed

# self-contained tests of specific things which had problems before
altAddTest("scott/multiplestatics");
altAddTest("scott/regbeforeassign $box");
altAddTest("scott/partialbracket");
altAddTest("scott/enuminit");
altAddTest("scott-nogcc/staticafternostorage $box");
altAddTest("scott/voidfree $box");
altAddTest("scott/recursetype $box");
altAddTest("scott/rmunused $box $gcc");
altAddTest("scott/simplewild $box");
altAddTest("scott/ptrtolocal $wildbox");
altAddTest("scott/tprintf $box");
altAddTest("scott/rmunused2 $box");

altAddTest("scott/gimpdouble");
altAddTest("scott/struct_cs");

$TEST->setField(altAddTest("scott/tprintf $wildbox"), "FailDiagnosis", <<'EOF');

  Failure of this test case usually indicates the stdin/out/err
  section of the patch file (lib/ccured_???.patch) does not exactly
  match your /usr/include/stdio.h.

EOF

altAddTest("scott/ptrmanip $wildbox");
altAddTest("scott-nogcc/bogus_redef");
altAddTest("scott/s59");
altAddTest("scott/putc $gcc");
altAddTest("scott/putc $wildbox $gcc");
altAddTest("scott/lexnum");
altAddTest("scott/ctype");
altAddTest("scott/ctype $box");
altAddTest("test-bad/wildfun $box");

# verify results of inference
$TEST->setField(altAddTest("scott/ptrkinds $box"), "AfterSuccessScript", <<'EOF');

  grepBoth() {
    grep -w $2 $1 | grep -w $3
  }

  # looks like we no longer print __SAFE with safe pointers...
  #grepBoth $src safeptr __SAFE && \

  src=small2/ptrkinds.cured.c
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
altAddTest("scott/argv $box");

# function pointers don't work with inferred wildness
altAddTest("scott/funcptr");
altAddTest("scott/funcptr $wildbox", "slow");
altAddTest("scott/funcptr $box", "slow");

# transparent unions are a problem for network apps
altAddTest("scott/transpunion $gcc");
altAddTest("scott/sockaddr $gcc");

# test of recent __HEAPIFY annotation
altAddTest("scott/heapify $box");
altAddTest("scott/heapify $wildbox");

# misc...
altAddTest("scott/constdecl");
altAddTest("scott/oldstyle");
altAddTest("scott/typeof $gcc");
altAddTest("scott/asmfndecl $gcc");
altAddTest("scott/xlsubr $box");
altAddTest("scott/open $gcc");
altAddTest("scott/ioctl $box $gcc");
altAddTest("scott/stralloc $box $gcc");
altAddTest("scott/mknod $box $gcc");
altAddTest("badd/nullfield $manualbox");
altAddTest("scott/constfold");
altAddTest("scott/mode_sizes $gcc");       # mode(__QI__) stuff
altAddTest("scott-nolink/brlock $gcc");
altAddTest("scott/qsort_wild $box");
altAddTest("scott/regparm0 $gcc");         # this works, unfortunately..
altAddTest("scott/unscomp");               # kernel/fs/buffer.c
altAddTest("scott/suppress_optim $box");
altFailTest("missing __mkptr_string_ww",
           "scott/suppress_optim $wildbox");
altFailTest("makes too many things tagged; this changed when matth modified "
           . "the way main() gets its arguments wrapped, and we haven't "
           . "cared enough about TAGALLFNS=1 to fix it",
           "scott/suppress_optim $wildbox TAGALLFNS=1");
altAddTest("testrun/bug1 $box");
altAddTest("scott/structs_edg_stl_ccuredlib_test $box", "slow");
altAddTest("misc-tests");
altAddTest("scott/chararr1 $box", "slow");
altAddTest("scott/chararr2 $box", "slow");
altAddTest("scott/thing");
altAddTest("scott/strerror1 $box");
altAddTest("scott/bsearch $box", "slow");
altAddTest("scott/signal $box");
altAddTest("scott/getaddrinfo $box", "slow");
altAddTest("test-bad/sin_zero $box");
altAddTest("scott/getopt $box", "slow");
altAddTest("scott/glob $box", "slow");

# current problematic test cases
altAddTest("scott/complex_float $box");
altAddTest("mergeinline");
altAddTest("test/addrofparam $box");
altAddTest("scott-nolink/name-capture-bitand $box");
altAddTest("scott-nolink/wildfun2 $box");
altAddTest("scott/dblarg.int $box");       # this yields a warning that might be a problem
altAddTest("scott/decl_inl $box");         # produces a gcc warning I'd like to silence
altAddTest("doThrowFv $wildbox UNTAGGEDFNS=1");
altAddTest("scott/uninit_tmp");
altAddTest("test-tagfile $wildbox TAGFILE=tagfile.txt");
altAddTest("test-tagfile $wildbox TAGFILE=tagfile.txt EXTRAARGS=-DSTATIC_FUNC");
altAddTest("scott/monthname $box");
altFailTest("problem with gcc coercions",
           "scott/floatarg INFERBOX=wild TAGALLFNS=1");
altFailTest("problem with over-aggressive pointer checks?",
           "scott/ptrarith INFERBOX=infer");
altAddTest("combine_samefn");
altAddTest("combine_node_alloc");
altAddTest("combine_sbump");
altAddTest("combine_sbumpB");
altAddTest("combine_sbumpB MERGEINLINES=1");
altAddTest("combine_allocate");
altAddTest("combine_allocate MERGEINLINES=1");
altAddTest("combine_theFunc");
altAddTest("combine_theFunc MERGEINLINES=1");
altAddTest("combine_syserr");
altAddTest("combine_syserr MERGEINLINES=1");
altAddTest("combine_copyptrs WARNINGS_ARE_ERRORS=1");
altAddTest("combine_copyptrs WARNINGS_ARE_ERRORS=1 MERGEINLINES=1");
altAddTest("merge-twice");
altAddTest("scott/arrayexpand INFERBOX=infer");
altAddTest("scott/byteprintf INFERBOX=infer");
altAddTest("scott/bufferlinegetter INFERBOX=infer");
altAddTest("scott/null_pointer_field INFERBOX=infer");
altAddTest("scott/closefunc INFERBOX=infer");
altAddTest("scott/sockunion INFERBOX=infer", "slow");

# tests of things implemented for EDG compatibility
altAddTest("mergestruct");
altAddTest("test-bad/globstackptr $box");
altAddTest("test-bad/ehstack $box");
altAddTest("test-bad/setjmp $box", "slow");
altAddTest("combinetaggedfn $wildbox SEPARATE=1 UNTAGGEDFNS=1");

# test of strings (need more!)
altAddTest("badd/ovwrnull $box");
altAddTest("test-bad/strloop2 $box");

# tests of function models
altAddTest("scott/memcpy $box");
altAddTest("scott/realloc $box");
altAddTest("scott/strchr $box");
altAddTest("scott/models $box", "slow");

# tests of things in safec.c
altAddTest("scott/qsort $box");
altAddTest("scott/strpbrk $box");
altFailTest("needs a deep-mangled wrapper?", "scott/fgets $box");
altAddTest("runall/sockets $box $gcc");

# more stuff, mostly from ftpd
altAddTest("scott/reply $box");

# works on my machine; works on manju now too apparently
altAddTest("scott/getpwnam $box $gcc", "slow");

altAddTest("test-bad/execv $box $gcc");
altAddTest("scott/popen $box $gcc", "slow");
altAddTest("scott/memset_int $box");
altAddTest("scott/printfllong $box $gcc");
altAddTest("test-bad/replydirname $box");
altAddTest("test-bad/boundaries $box", "slow");
altAddTest("scott/stat $box");
altAddTest("scott/scanf $box");

# simple self-contained thing
altAddTest("hola");
altAddTest("hola $box");

# a few things that should fail
altAddTest("badd/lbound $box $gcc");
altAddTest("badd/ubound $box $gcc");
altAddTest("badd/fseq $box $gcc");
altAddTest("badd/calloc $box $gcc");
altAddTest("badd/stackaddr $box $gcc");
altAddTest("test-bad/trivial-tb");
altAddTest("test-bad/retptr $box RELEASE=1");
$TEST->addBadComment("test-bad/retptr", "Fails in RELEASE mode because the ok() function is inlined, making it look like we're returning a local.");

# verify we have tags and that they work
# altAddTest("badd/nonptr $box $gcc");          # very simple test. Covered by wild1
altAddTest("scott/arraytags $box $gcc");     # this one is pretty hairy

# simple test of combiner
altAddTest("comb $gcc");
altAddTest("comb $box $gcc");

# test combiner's ability to detect inconsistency
altAddTest("baddef");


# These take too long if we turn off strings
## hashtest and rbtest with TABLE
my $iters = "EXTRAARGS=-DITERS=100";
#$TEST->setField(altAddTest("rbtest $table $iters"),
#                "FailDiagnosis", "Maybe ARCHOS isn't set right?");
#altAddTest("hashtest $table $iters");

# red-black tree
altAddTest("rbtest $iters", "slow");
altAddTest("rbtest $box $iters", "slow");

altAddTest("wes-rbtest $iters");
altAddTest("wes-rbtest $box $iters", "slow");

# hashtable
$iters = "EXTRAARGS=-DITERS=10000";
altAddTest("hashtest $iters", "slow");
altAddTest("hashtest $box $iters", "slow");
altAddTest("hashtest $wildbox $iters", "slow");

altAddTest("wes-hashtest $iters");
altAddTest("wes-hashtest $box $iters");

altAddTest("hashtest $box $iters MAXSPLIT=1", "slow");

# some piece of PCC
altAddTest("testpcc/parseobject EXTRAARGS=--no-idashi");

# apache modules; set is needed for next one
$TEST->setField(altAddTest("apache!1setup"), 'Cmd', "$make apachesetup");
$TEST->setField(altAddTest("apache!2setup"), 'Cmd', "$make apachesetup");
altAddTest("apache/gzip");

# does not work: complains of many incompatible type redefinitions
#runTest $make apache/rewrite

# The following block of tests was transferred from testsafec.pl to
# the regrtest script, and now has been transferred back.
# I keep it here because for each test I just run one mode
# (whichever mode has shown itself to be possibly an issue in
# the past, or CIL-only when I have no data), so it's faster
# than the add3Tests above.
altAddTest("test/list");
altAddTest("test/alloc");
altAddTest("test/array1");

if ($egcs) {
  # these fail because I'm using egcs instead of gcc 2.95
  altFailTest("parse error on cil output", "test/attr");
  altFailTest("cast specifies function type", "test/attr $wildbox");
  altFailTest("parse error on cil output", "test/attr3");
  altFailTest("cast specifies function type", "test/attr3 $wildbox");
}
else {
  altAddTest("test/attr");
  altAddTest("test/attr $wildbox");
  altAddTest("test/attr3");
  altAddTest("test/attr3 $wildbox");
}

altAddTest("test/attr4 $box");
altAddTest("test/bitfield");
altAddTest("test/box1");
altAddTest("test/cast1");
altAddTest("test/constprop");
altAddTest("test/enum");
altAddTest("test/format1");
altAddTest("test/func");
altAddTest("test/globals");
altAddTest("test/init");
altAddTest("test/init $box");
altAddTest("test/initial");
altAddTest("test/jmp_buf");
altAddTest("test/linux_atomic");
altAddTest("test/list");
altAddTest("test/pointers");
altAddTest("test/printf");
altAddTest("test/retval");
altAddTest("test/seq");
altAddTest("test/sized");
altAddTest("test/sizeof");
altAddTest("test/smallstring");
altAddTest("test/static");
altAddTest("test/strcpy");
altAddTest("test/string");
altAddTest("test/struct_init");
altAddTest("test/structassign");
altAddTest("test/tags");
altAddTest("test/task");
altAddTest("test/voidstar");
# end copied block of tests

# more random stuff
altAddTest("scott-nogcc/funcname $gcc");
altAddTest("scott/litstruct $gcc");
altAddTest("scott/main $gcc");
altAddTest("scott/globalprob $gcc");
altAddTest("scott/bisonerror $gcc");
altAddTest("scott/cmpzero");
altAddTest("scott/kernel1 $gcc");
altAddTest("scott/kernel2 $gcc");
altAddTest("scott/xcheckers $gcc");
altAddTest("scott/memberofptr $gcc");
altAddTest("scott/invalredef $gcc");
altAddTest("scott/invalredef2 $gcc");
altAddTest("scott/errorinfn");
altAddTest("scott/unionassign $box");
altAddTest("scott/unionassign $wildbox");
altAddTest("scott/readv $box", "slow");
altFailTest("FSEQ incompleteness", "scott/bloop $box");
altAddTest("scott/funcptr3 $box");
altAddTest("scott/structattr");
altAddTest("scott/neg64");
altAddTest("testc/arrayinitsize");
altAddTest("test-bad/enuminit2");
altAddTest("scott/volatilestruct");
altAddTest("scott/sizeofchar");
altAddTest("scott/initedextern");
altAddTest("scott/arrayinit");
altAddTest("scott/structattr2");
altAddTest("scott/structattr3");
altAddTest("scott/enumerator_sizeof");
altAddTest("testrun/decl_mix_stmt");
altFailTest("output makes GCC unhappy", "scott/enumattr");
altAddTest("scott/alignprob $box");
altAddTest("scott/doublefree $box RELEASE=1");
altAddTest("scott/alignok $box");
altAddTest("scott/subtypebug1 $box");
altAddTest("scott/subtypebug2 $box");


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
        ? " " : " RELEASE=1 ";
    $theargs .= " $extraargs ";
    if(defined $self->{option}->{noremake}) {
        $theargs .= " NOREMAKE=1";
    }
    # Turn on the verbose flag
    $theargs .= " STATS=1 PRINTSTAGES=1 ";
    # Turn on the strings
    # $theargs .= " EXTRAARGS=--useStrings ";

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
        $thisargs .= ' NOEMITBROWSER=1 ';  
        my $tst =
            $self->newTest(Name => $name . "-" . $kind,
                           Dir => ".",
                           Cmd => "$make " . $name . $thisargs,
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
                               [ 'box', 
                                'inferbox']);
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


# sm: tweak to test cvs commit
# another one
