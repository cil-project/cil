# A regression tester for CIL
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

print "Test infrastructure for CIL\n";

# Create our customized test harness
my $TEST = SafecRegTest->new(AvailParams => { 'RUN' => 1,
                                              'SUCCESS' => 0},
                             LogFile => "cil.log",
                             CommandName => "testcil");

# sm: I want a global name for this $TEST thing, since I find it is merely
# excess verbiage when adding tests..
$main::globalTEST = $TEST;

my $inferbox="none";

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
#  1003 - Compilation
#  1004 - Running 

my @runpattern = 
    ("^Run.+ ([.\\d]+)ms" => sub { $_[1]->{"run"} = $_[2]; });

my %commonerrors = 
    ("^Parsing " => sub { $_[1]->{instage} = 1001; },

     "^Converting CABS" => sub { $_[1]->{instage} = 1002; },

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


    "^user\\s+(\\d+)m([\\d.]+)s"
              => sub { $_[1]->{RUN} = 60 * $_[2] + $_[3]; },

    "^TOTAL\\s+([\\d.]+) s" => sub { $_[1]->{CURE} = $_[2]; },
    );

                                         

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


# build the documentation, to make sure that it still builds
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


$TEST->add3Tests("test/apachebits");
$TEST->add3Tests("testrun/apachebuf");

$TEST->add3Tests("testrun/apachefptr");
$TEST->add2Tests("testrun/asm1", "_GNUCC=1");
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
$TEST->addTests("testrun/array4", "", ['cil']);
$TEST->add3Tests("test/array2");
$TEST->add2Tests("testrun/array_varsize");
$TEST->addTests("testrun/formalscope", "", ['cil']);
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
$TEST->addTests("test/attr9", "_GNUCC=1 WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->addTests("testrun/packed", "_GNUCC=1 WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->add3Tests("test/bitfield");
$TEST->add3Tests("testrun/bitfield3");
     
$TEST->add3Tests("testrun/bitfield2");
$TEST->addTests("testrun/call2", "", ['cil']);
$TEST->add3Tests("test/cast1");
$TEST->add3Tests("test/cast2");
$TEST->add2Tests("test/cast4", "_GNUCC=1");
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
$TEST->add3Tests("test/func");
$TEST->addTests("test/funcarg", "", ['cil']);
   $TEST->addBadComment("test/funcarg-cil", "Bug in parser (argument of function type)");

$TEST->add3Tests("testrun/func2");
$TEST->add3Tests("testrun/func3");
$TEST->add3Tests("testrun/func4");
$TEST->addTests("test/func10", "", ['cil']);
$TEST->addBadComment("test/func10-cil", 
                     "Cil bug: Cannot parse some strange K&R function definition");
$TEST->add3Tests("test/globals");
$TEST->addTests("test/globals2", "", ['cil']);
$TEST->addBadComment("test/globals2-cil", "CIL bug: we print array size expressions that refer to variables that haven't been defined yet.");
$TEST->add3Tests("testrun/float");
$TEST->addTests("testrun/float2", "", ['cil']);
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
$TEST->addTests("testrun/init18", "", ['cil']);
$TEST->addTests("testrun/init19", "WARNINGS_ARE_ERRORS=1", ['cil']);
$TEST->addTests("testrun/init20", "_GNUCC=1", ['cil']);
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
$TEST->add3Tests("test/list");
$TEST->addTests("testrun/localinit", "", ['cil']);

$TEST->addTests('testrun/longBlock', '', ['cil']);
$TEST->add2Tests("testrun/perror");
$TEST->add2Tests("testrun/perror1");
$TEST->add2Tests("test/pure");
$TEST->addTests("testrun/post-assign", "", ['cil']);
   $TEST->addBadComment("testrun/post-assign-cil", 
                        "CIL does not have the same evaluation order for ++ as gcc");
$TEST->add3Tests("test/printf", "", @runpattern);
$TEST->add3Tests("test/printf_const", "", @runpattern);
$TEST->add3Tests("testrun/printf2");
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
if (!$egcs) {
  $TEST->add2Tests("testrun/vararg6");
}
$TEST->add2Tests("test/vararg7", "_GNUCC=1");
$TEST->add2Tests("testrun/va-arg-1", "_GNUCC=1");
$TEST->add2Tests("testrun/va-arg-2", "_GNUCC=1");
if (!$egcs) {
  $TEST->add2Tests("testrun/va-arg-7", "_GNUCC=1");
}
$TEST->addTests("test-bad/arrsize", "", ['cil']);
$TEST->addTests("testrun/comma1", "_GNUCC=1", ['cil']);
$TEST->add3Tests("test/retval");
$TEST->add3Tests("testrun/static", "", @runpattern);
$TEST->add3Tests("test/static1");
$TEST->addTests("testrun/static2", "", ['cil']);
$TEST->add3Tests("test/strcpy");
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

$TEST->addTests("test/noreturn", "", ['cil']);
                

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
$TEST->addTests("testrun/addrof3", "_GNUCC=1", ['cil']);
$TEST->add3Tests("testrun/lval1", "_GNUCC=1");
$TEST->add3Tests("testrun/bind1", "EXTRAARGS=--assumePrintf");
$TEST->add3Tests("test/bind2", "EXTRAARGS=--allowInlineAssembly");
   $TEST->add3Group("test/bind2", "slow");
$TEST->addTests("testrun/decl1", "_GNUCC=1", ['cil']);
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
$TEST->addTests("combinealias", "", ['cil']);
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
$TEST->addTests("mixedcomb", "", ['cil']);
$TEST->addTests("testrun/math1", "", ['cil']);
$TEST->addTests("test/linuxcombine1_1", "", ['cil']);

$TEST->addTests("arcombine", "_GNUCC=1", ['cil']);
$TEST->add2Tests("testrun/funptr1");
$TEST->addTests("testrun/typespec1", "_GNUCC=1", ['cil']);
   $TEST->addBadComment("testrun/typespec1-cil", 
                        "Must emulate bug in GCC?");
$TEST->addTests("testrun/returnvoid", "", ['cil']);
$TEST->addTests("testrun/returnvoid1", "", ['cil']);
$TEST->addTests("testrun/return1", "", ['cil']);
$TEST->addTests("testrun/for1", "", ['cil']);
$TEST->addTests("testrun/void", "_GNUCC=1", ['cil']);
$TEST->addTests("test/voidtypedef", "", ['cil']);
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
$TEST->addTests("test/union5", "", ['cil']);
$TEST->addTests("testrun/inline1", "", ['cil']);
$TEST->addTests("runall/extinline", "", ['cil']);

$TEST->addTests("testrun/rmtmps-attr", "", ['cil']);
   $TEST->addBadComment("testrun/rmtmps-attr-cil", 
                        "A limitation of our support for attributes");
 
$TEST->add3Tests("testrun/vsp");

$TEST->addTests("test/cpp-2", "", ['cil']);
   $TEST->addBadComment("test/cpp-2-cil", 
                        "Bug in parser (empty pragmas)");
$TEST->addTests("test/cpp-3", "_GNUCC=1", ['cil']);



if($win32) {
    $TEST->addTests("testrun/extern_init", "_MSVC=1", ['cil']);   
    $TEST->addTests("testrun/msvc2", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc3", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc4", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc6", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc7", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc8", "_MSVC=1", ["cil"]);
    $TEST->addTests("testrun/msvc9", "_MSVC=1", ["cil"]);

    $TEST->addTests("test-bad/try1", "_MSVC=1", ["cil"]);
}
$TEST->addTests("testrun/msvc1", "", ["cil"]);
$TEST->addTests("testrun/msvc5", "", ["cil"]);

$TEST->addTests("testrun/extern1", "", ['cil']);

$TEST->addTests("test/duplicate", "", ['cil']);

$TEST->add2Tests("testrun/simon6");
    
$TEST->add2Tests("testrun/stringsize");
$TEST->addTests("testrun/min", "", ['cil']);



$TEST->addTests("test/simplify_structs1", 
                "USECILLY=1 EXTRAARGS=--dosimplify",  ['cil']);
$TEST->addTests("testrun/simplify_structs2", 
                "USECILLY=1 EXTRAARGS=--dosimplify",  ['cil']);

$TEST->addTests("testrun/typeof1", "", ['cil']);
$TEST->addTests("testrun/semicolon", "_GNUCC=1", ['cil']);

$TEST->add2Tests("merge-ar", "");



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
altAddTest("scott/bufferlinegetter INFERBOX=infer");
altAddTest("scott/null_pointer_field INFERBOX=infer");

# tests of things implemented for EDG compatibility
altAddTest("mergestruct");
altAddTest("test-bad/globstackptr $box");
altAddTest("test-bad/ehstack $box");
altAddTest("test-bad/setjmp $box", "slow");
altAddTest("combinetaggedfn $wildbox SEPARATE=1 UNTAGGEDFNS=1");

# test of strings (need more!)
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
altAddTest("test-bad/trivial-tb");
altAddTest("test-bad/retptr $box RELEASE=1");
$TEST->addBadComment("test-bad/retptr", "Fails in RELEASE mode because the ok() function is inlined, making it look like we're returning a local.");

altAddTest("scott/arraytags $box $gcc");     # this one is pretty hairy

# simple test of combiner
altAddTest("comb $gcc");
altAddTest("comb $box $gcc");

# test combiner's ability to detect inconsistency
altAddTest("baddef");


# apache modules; set is needed for next one
$TEST->setField(altAddTest("apache!1setup"), 'Cmd', "$make apachesetup");
$TEST->setField(altAddTest("apache!2setup"), 'Cmd', "$make apachesetup");

# does not work: complains of many incompatible type redefinitions
#runTest $make apache/rewrite

altAddTest("test/attr4 $box");
altAddTest("test/init");
altAddTest("test/init $box");
altAddTest("test/initial");
altAddTest("test/jmp_buf");
altAddTest("test/static");


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
altAddTest("scott/funcptr3 $box");
altAddTest("scott/structattr");
altAddTest("scott/neg64");
altAddTest("testrun/arrayinitsize");
altAddTest("test-bad/enuminit2");
altAddTest("scott/volatilestruct");
altAddTest("scott/sizeofchar");
altAddTest("scott/initedextern");
altAddTest("scott/arrayinit");
altAddTest("scott/structattr2");
altAddTest("scott/structattr3");
altAddTest("scott/enumerator_sizeof");
altAddTest("testrun/decl_mix_stmt");
altAddTest("scott/enumattr");
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
            next;
        }
        if($kind eq 'box') {
            next;
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
    my @tests = $self->addTests($name, $extraargs, ['cil']);
    # Run the CCured test as a quick test, and the others as slow.
    # The CCured test should catch any CIL errors, anyways.
    $self->addGroups($name . "-cil", "slow");
    return @tests;
}

sub add2Tests {
    my($self, $name, $extraargs) = @_;
    my @tests = $self->addTests($name, $extraargs, ['cil']);
    # Run the CCured test as a quick test, and the CIL as slow.
    # The CCured test should catch any CIL errors, anyways.
    $self->addGroups($name . "-cil", "slow");
    return @tests;
}

sub addTestsFail {
    my($self, $name, $extraargs, $failpattern, $pkinds) = @_;
    my @tests = $self->addTests($name, $extraargs, $pkinds, 
                                MustFail => $failpattern);
    return @tests;
}

sub addBadComment {
    my($self, $name, $comm) = @_;
    $self->addComment($name, $comm);
    $self->addGroups($name, "bad");
}

sub add3Comment {
    my ($self, $name, $comm) = @_;
    $self->addComment($name . "-cil", $comm);
}

sub add2Comment {
    my ($self, $name, $comm) = @_;
    $self->addComment($name . "-cil", $comm);
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
}

sub add2Group {
    my ($self, $name, @groups) = @_;
    $self->addGroups($name . "-cil", @groups);
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
