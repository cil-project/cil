#!/usr/bin/perl
# A Perl script that substitutes either the GCC or the MSVC compiler. Invokes
# the preprocessor, then another preprocessor and then the real compiler. 
#
use strict;
use File::Basename;
use File::Copy;
use FindBin;

sub printHelp {
    print <<EOL;
Wrapper for GCC and MSVC. 
Usage: safecc [args] [cargs]

args: 
  --help       Prints this help message
  --cil        Uses the CIL version of the file (default)
  --box        Uses the BOX version of the file
  --boxonly    Like BOx but does not compile
  --inferbox   Infers the pointer types
  --optim      Removes redundant checks
  --cabs       Creates a CABS version of the file
  --release    Runs with the native code version
  --keep=dir   Keep intermediate files in dir
  --safec=xxx  Passes xxx to the safec executable. Multiple occurences of 
               --safec should be used to add more arguments.
  --tr=flag    Passes "-tr flag" (tracing flag) to the safec executable.
  --patch=xxx  Patches before the preprocessor with the given specification
  --combine    Combine all the sources into one file
 It detects from the form of the command line arguments whether GCC or MSVC
 should be used.
EOL
}

my $cmd;

# Go through the arguments and separate the source names from the other
# arguments. Among the arguments separate those that can be passed to the
# preprocessor (@ppargs) and those that should be passed only to the final
# phase (@otherargs)
my @sources  = ();             # The C or C++ sources
my @ppargs = ();
my @otherargs = ();             # All the other arguments

my $compiler;                   # = "msvc" for MS VC and = "gcc" for GCC

my $gccIsNotLinking = 0;        # weimer : gcc -c or gcc -S means no link
my $doBox = 0;
my $boxOnly = 0;
my $doInferBox = 0;
my $doOptim=0;
my $doCil = 0;
my $doCabs = 0;
my $doCombine = 0;
my $doTV;
my $useRelease = 0;
my $keepDir;                    # Keep outputs here
my $extraArgs = "";
my @patchFiles = ();

###
### A few library routines to help with file name management
###
# Return true if paths should have backward slashes.
my $pathSep = ($^O eq "MSWin32") ? '\\' : '/';
my $dirListSep = ($^O eq "MSWin32") ? ';' : ':'; 

sub openWithMounts {
    my ($fileName, $fdesc) = @_;
    
    my $openFileName = $fileName;
    # Try it outright
    if(open($fdesc, "<$openFileName")) {
        return;
    }
    # If we fail then maybe we are using cygwin paths in a Win32 system
    if($compiler eq "gcc" && $^O eq 'MSWin32') {
        open(WINNAME, "cygpath -w $fileName|")
            || die "Cannot run cygpath to convert $fileName to a Windows name";
        my $newName = <WINNAME>;
        # print "Converted $fileName to $newName\n";
        close(WINNAME) || die "Cannot run cygpath to convert $fileName";
        if(open($fdesc, "<$newName")) {
            return;
        }
    }
    die "Cannot open file $fileName";
}

# Is absolute path name?
sub isAbsolute {
    my($name) = @_;
    if($^O eq "MSWin32") {
        return ($name =~ m%^([a-zA-Z]:)?[/\\]%);
    } else {
        return ($name =~ m%^[/\\]%);
    }
}

# Is it on the same volume?
sub sameVolume {
    my($f1, $f2) = @_;
    if($^O eq "MSWin32") {
        my($v1) = ($f1 =~ m%^([a-zA-Z]:)%);
        my($v2) = ($f2 =~ m%^([a-zA-Z]:)%);
        return (defined $v1 && defined $v2 && lc($v1) eq lc($v2));
    } else {
        return 1;
    }
}


if($^O eq 'MSWin32') {
    require Win32;
}
# Are we on Win95?
sub isWin95 {
    if($^O eq 'MSWin32') {
        no strict 'subs'; # To avoid compilation errors on Unix 
	return Win32::IsWin95;
    } else {
	return 0;
    }
}

my $useRelative;           # Whether to use relative paths and to which
                           # directory 
# Take a PATH and normalize it. If $useRelative is the name of a directory
# then relativize with respect to that directory
sub path {
    my ($name) = @_;
    # print "Path on $name is ";
    if ($^O eq "MSWin32") {
        $name =~ s%/%\\%g;
    }
    # Get rid of final /
    if($name =~ m%[/\\]$%) {
        chop $name;
    }
    # Get rid of /./
    $name =~ s|[/\\]\.[/\\]|$pathSep|g;
    $name =~ s|^\.[/\\]||;
    # print "Name = $name\n";
    if(defined($useRelative)) {
        # print " (relative to $PCC::useRelative) ";
        if(&isAbsolute($name) &&
           &isAbsolute($useRelative) &&
           &sameVolume($name, $useRelative)) {
            # print "\n====Here\n";
            my @pbase = split(m%[/\\]%, $useRelative);
            my @path  = split(m%[/\\]%, $name);
            while(@pbase && @path && lc($pbase[0]) eq lc($path[0])) {
                shift @pbase;
                shift @path;
            }
            # print "Here base=@pbase and this=@path\n";
            my $nname = join($pathSep, @path);
            my $nbase = '';
            my $cnt;
            if($#pbase == -1) {
                $nbase = '';
            } elsif ($#pbase == 0) {
                $nbase = '..';
            } else {
                $nbase = '.';
                foreach $cnt (@pbase) {
                    $nbase = "$nbase$pathSep..";
                }
            }
            if($nname eq '' || $nbase eq '') {
                $nname = "$nbase$nname";
                if($nname eq '') {
                    $nname = '.';
                }
            } else {
                $nname = "$nbase$pathSep$nname";
            }
            if(length($nname) < length($name)) {
                $name = $nname;
            }
        }
    }
    # print " $name\n";
    return $name;
}

# Read the user time (in milliseconds)
sub getTime {
    if($^O eq "MSWin32") {
	no strict 'subs';
	return Win32::GetTickCount;
    } else {
	my ($u,$s,$cu,$cs) = times;
	return int(1000 * ($cu + $cs));
    }
}
    

sub quoteIfNecessary {
    my($arg) = @_;
    if($arg =~ m|\s|) {
        return "\"$arg\"";
    } else {
        return $arg;
    }
}
while($#ARGV >= 0) {
    my $arg = shift @ARGV;
    # Handle first our own arguments
    if($arg =~ m|^--keep=(.+)$|) { # Where to keep the outputs
        $keepDir = $1;
        next;
    }
    if($arg =~ m|^--safec=(.*)$|) { # Extra arguments
        $extraArgs .= " $1";
        next;
    }
    if($arg =~ m|^--tr=(.*)$|) {    # Extra tracing arguments
        $extraArgs .= " -tr $1";
        next;
    }
    if($arg eq "--help") {
        &printHelp ();
        exit 1;
    }
    if($arg eq "--box") { # Whether to box or not
        $doBox = 1;
        next;
    }
    if($arg eq "--boxonly") { # Whether to box or not
        $doBox = 1; $boxOnly = 1;
        next;
    }
    if($arg eq "--combine") { # Whether to box or not
        $doCombine = 1;
        next;
    }
    if($arg eq "--optim") { # Whether to optimize the boxing code or not
        $doOptim = 1;
        next;
    }

    if($arg eq "--inferbox") { # Whether to infer the boxing or not
        $doInferBox = 1;
        next;
    }
    if($arg eq "--cil") { # Whether to generate the cil or not
        $doCil = 1;
        next;
    }
    if($arg =~ m|^--tv=?(.*)$|) { # Whether to use translation validation.
        $doTV = $1;
        next;
    }
    if($arg =~ m|^--patch=?(.*)$|) { # Whether to patch
        push @patchFiles, $1;
        next;
    }
    if($arg eq "--cabs") { # Whether to generate the cabs or not
        $doCabs = 1;
        next;
    }
    if($arg eq "--release") {
        $useRelease = 1;
        next;
    }
    # now detect whether we are in MSVC or in GCC
    if($arg =~ m|^-|) {
        &setCompilerMode("gcc", $arg);
    }
    # weimer: this regexp does not catch /unix/path/to/file
    # as an MSVC extension
    if($arg =~ m|^/[^/]+$|) {
        &setCompilerMode("msvc", $arg);
    }
    if(! defined $compiler) {     # Must be a source file
        push @sources, $arg;
        next;
    }
    # MICROSOFT VISUAL C
    if($compiler eq "msvc") {
        if($arg =~ m|^/|) {
            # All arguments in MSVC start with / and are one word only
            if($arg =~ m%^/F(e|o)%) {
                push @otherargs, $arg;
                # Some arguments can be two words
                if($arg =~ m%^/(Fe|Fo)$%) { # Only got the first word
                    $arg = shift @ARGV;
                    push @otherargs, &quoteIfNecessary($arg);
                }
                next;
            }
            push @ppargs, $arg;
            # Some arguments can be two words
            if($arg =~ m%^/(I|D)$%) { # Only got the first word
                $arg = shift @ARGV;
                push @ppargs, &quoteIfNecessary($arg);
            }
        } else {
            push @sources, &quoteIfNecessary($arg);
        }
        next;
    }
    # GCC
    if($compiler eq "gcc") {
        if ($arg eq "-c" || $arg eq "-S") {
            $gccIsNotLinking = 1; 
            print "GCC is not linking.\n" ;
        }
        if($arg =~ m|^-|) {
            if($arg =~ m|^-o(.*)$|) {
                if($arg eq "-o") {
                    $arg = shift @ARGV;
                    push @otherargs, "-o", &quoteIfNecessary($arg);
                } else {
                    push @otherargs, &quoteIfNecessary($arg);
                }
                next;
            }
            push @ppargs, &quoteIfNecessary($arg);
        } else {
            push @sources, &quoteIfNecessary($arg);
        }
    }
}

# Fixup some arguments
if(! $doBox) { $doCil = 1; }
if(defined $keepDir && $keepDir !~ m|[/\\]$|) {
    $keepDir = $keepDir . "/";
}
if($doBox) {
    if($compiler eq "msvc") {
        push @ppargs, " /DBEFOREBOX ";
    }
    if($compiler eq "gcc") {
        push @ppargs, " -DBEFOREBOX ";
    }
}

if($compiler eq "msvc") { print "Microsoft VC mode\n"; }
if($compiler eq "gcc") { print "GNU C mode\n"; }
if(defined $doTV && $doTV ne "") {
    $doTV .= " -debug";
}
if(defined $doTV && $compiler ne "gcc") {
    print "***WARNING: Translation validation only works with GCC\n";
    undef $doTV;
}

my $bindir = $FindBin::Bin;


my $tvCommand;
if(defined $doTV) {
  # Find the translation validator
    my $which = $doTV eq "" ? "asm" : "byte";
    my $tvdir = path("$bindir/../../../Source/TransVal");
    $tvCommand = path("$tvdir/obj/transval.$which.exe");
    if(-f $tvCommand) {
        print "Found $tvCommand\n";
    }
}

#
# Prepare for patching
#
my @patches = ();
my $pFile;
foreach $pFile (@patchFiles) {
    open(PFILE, "<$pFile") ||
        die "Cannot read patch file $pFile\n";
  NextPattern:
    while(<PFILE>) {
        if($_ !~ m|^<<<(.*)$|) {
            next;
        }
        # Process the flags
        my $patchflags = $1;
        my $patchglobal;
        if($patchflags =~ m|g|) {
            $patchglobal = 1;
        }
        # Now we have found the start
        my $pattern = <PFILE>;
        if($pattern eq "") {
            die "A pattern is missing in $pFile";
        }
        my @patterns = ($pattern);
        while(<PFILE>) {
            if($_ =~ m|^===|) {
                last;
            }
            push @patterns, $_;
        }
        if($_ !~ m|^===|) {
            die "No separator found after pattern $pattern in $pFile";
        }
        my $replacement = "";
        while(<PFILE>) {
            if($_ =~ m|^>>>|) {
                push @patches, { HEAD => $pattern,
                                 GLOBAL => $patchglobal,
                                 NRLINES   => 1 + $#patterns,
                                 PATTERNS => \@patterns,
                                 REPLACE => $replacement,
                             };
                next NextPattern;
            }
            $replacement .= $_;
        }
        die "Replacement for $pattern not ended in $pFile";
    }
    close(PFILE) ||
        die "Cannot close patch file $pFile\n";
}
#print "PATCHES   =\n", join("\n:", @patches), "\nDone\n";
#print "SOURCES   = ", join(' ', @sources), "\n";
#print "PPARGS = ", join(' ', @ppargs), "\n";
#print "OTHERARGS = ", join(' ', @otherargs), "\n";
#if($compiler eq "gcc") {
#    $bindir = "./lib";
#}

# Go through the sources and get the C ones
my $src;
my @newsources = ();
foreach $src (@sources) {
    my ($base, $dir, $ext) = fileparse($src, 
                                       "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    if(! defined($ext) || $ext eq "") { # Not a C source
        push @newsources, $src;
        next;
    }
    # print "Base=$base, ext=$ext, dir=$dir\n";
    # Now run the preprocessor only, with all the other arguments
    my $afterpp;
    my $origdir = $dir;
    if(defined $keepDir) {
        $dir     = $keepDir;
    }
    $afterpp = "$dir$base.i";
    &cpp($src, $afterpp);
    # Now see if we must need to patch the file
    my $afterPatchExtension = ".i";
    if($#patchFiles >= 0) {
        &patchFile($dir, $base, ".i");
    } 
    # Now run our own preprocessor
    my $src = &ourPreprocessor($dir, $base, $afterPatchExtension);
    push @newsources, $src;
}

if($boxOnly) {
    exit (0);
}

# Now run the real compilation
my $safeclib = "";
if($doBox) {
    $safeclib = " $FindBin::Bin/../obj/safec"; 
    my $libext;
    if($compiler eq "gcc") {
        $libext = "lib.a";
    }
    if($compiler eq "msvc") {
        $libext = ".lib";
    }
    if($useRelease) {
        $safeclib .= $libext;
    } else {
        $safeclib .= "debug$libext";
    }
}
if($compiler eq "msvc") {
    $cmd = "cl /W3 /D_MSVC $safeclib " .
        ($doBox ? " /I$FindBin::Bin " : "") .
        ($useRelease ? " /Ox /G6 " : " /Zi /MLd /DEBUG ") .  # Should be opt.
            join(' ', @ppargs) . " " . 
                join(' ', @otherargs) . " " .
                    join(' ', @newsources);
}
if($compiler eq "gcc") {
    $cmd = "gcc -Wall -D_GNUCC " . 
        ($useRelease ? " -O4 " : " -g -ggdb") .   # These should be optional
        ($doBox ? " -I $bindir " : "") .  
            join(' ', @ppargs) . " " . 
                join(' ', @otherargs) . " " 
                        . join(' ', @newsources) 
                        . ($gccIsNotLinking ? "" : " $safeclib " )
                        ;
}
&runShell($cmd);

sub runShell {
    my ($cmd) = @_;

    # sm: I want this printed to stderr instead of stdout
    # because the rest of 'make' output goes there and this
    # way I can capture to a coherent file
    print STDERR "Running $cmd\n";

    # weimer: let's have a sanity check
    if (system($cmd) ne 0) {
	die "Possible error with $cmd!\n";
    }
}

sub cpp {
    my ($src, $afterpp) = @_;
    my ($base, $dir, $ext) = fileparse($afterpp, 
                                       "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    my ($sbase, $sdir, $sext) = fileparse($src, 
                                       "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    if($compiler eq "msvc") {
        $cmd = 
            "cl /P /I$bindir /D_MSVC " .
                join(' ', @ppargs) . " /FIfixup.h";
    }
    if($compiler eq "gcc") {
        $cmd = "gcc -E -D_GNUCC " .
            join(' ', @ppargs) . " -o $afterpp -include $bindir/fixup.h";
    }
    &runShell("$cmd $src");
    if($compiler eq "msvc") {
        # MSVC cannot be told where to put the output. But we know that it
        # puts it in the current directory
        my $msvcout = "./$sbase.i";
        my @st1 = stat $msvcout;
        my @st2 = stat $afterpp;
        while($#st1 >= 0) {
            if(shift @st1 != shift @st2) {
#                print "$msvcout is NOT the same as $afterpp\n";
                print "Copying $msvcout to $afterpp\n";
                unlink $afterpp;
                &File::Copy::copy($msvcout, $afterpp);
                unlink $msvcout;
                return;
            }
        }
#        print "$msvcout is the same as $afterpp\n";
    }
}

sub ourPreprocessor {
    my ($dir, $base, $ext) = @_;
    my $src = "$dir$base$ext";
    my $version = $useRelease ? "asm" : "byte";
    my $safecc = "$FindBin::Bin/../obj/safec.$version.exe";
    my $cmd = "$safecc -cabsindent 2 -verbose $extraArgs ";
    if($compiler eq "msvc") {
        $cmd .= " -msvc ";
    }
    my $result;
    my $resultExt;
    if($doCabs) {
        $cmd .= " -cabsout $dir$base.cabs";
    }
    if($doCil) {
        $resultExt = "cil.c";
        $cmd .= " -cilout $dir$base$resultExt";
    }
    if($doInferBox) { # Do not change the result, but produce one more file
        $cmd .= (" -inferbox $dir$base" . "infer.c ");
    }
    if($doBox) {  # Do this last to override $result
        $resultExt = "box.c";
        $cmd .= " -boxout $dir$base$resultExt";
    }
    if($doOptim) {
        #$resultExt = "optim.c";
        #$cmd .= " -optimout $dir$base$resultExt";
        $cmd .= (" -optimout $dir$base" . "optim.c ");
    }
    $result = "$dir$base$resultExt";
    $cmd .= " $src";
    &runShell($cmd);

    # Now run translation validation
    if(defined $doTV) {
        print "Running GCC to extract RTL\n";
        my $makeRTL = "gcc -g -ggdb -c -dr ";
        my $rtlDir = defined($keepDir) ? $keepDir : "./";
        my $srcRTL = "$rtlDir/$base$ext.rtl";
        my $trgRTL = "$rtlDir/$base$resultExt.rtl";
        &runShell("$makeRTL $src");
        &runShell("$makeRTL $result");
        # The output is in the current directory
        if($rtlDir !~ m|^.[/\\]$|) {
            my $orig = "./$base$ext.trl";
            print "Copying $orig to $srcRTL\n";
            unlink $trgRTL;
            &File::Copy::copy($orig, $srcRTL);
            unlink $orig;

            my $orig = "./$base$resultExt.trl";
            print "Copying $orig to $trgRTL\n";
            unlink $trgRTL;
            &File::Copy::copy($orig, $trgRTL);
            unlink $orig;
        }
        print "Running the translation validator\n";
        &runShell("$tvCommand -L tv.log $doTV $src.rtl $result.rtl");
    }

    return $result;
}

sub setCompilerMode {
    my ($mode, $arg) = @_;
    if(defined $compiler && $compiler ne $mode) {
        die "Found a $mode argument ($arg) while in $compiler mode";
    }
    $compiler = $mode;
}


#
# Patching of files
#
sub patchFile {
    my($dir, $base, $inext) = @_;
    
    my $in = "$dir$base$inext";
    my $interm = "$dir$base" . "_ppp.c";
    my $out = $in;
    my  $linePattern;
    if($compiler eq "msvc") {
        $linePattern = "^#line\\s+(\\d+)\\s+\"(.+)\"";
    }
    if($compiler eq "gcc") {
        $linePattern = "^#\\s+(\\d+)\\s+\"(.+)\"";
    }
    print "Patching $in\n";
    if(1) {
        # Make a copy of the input file
        &File::Copy::copy($in, "$dir$base.origi");
    }
    # Initialize all the patches
    my $patch;
    foreach $patch (@patches) {
        $patch->{DONE} = 0;
    }

    open(INTERM, ">$interm") || die "Cannot open patch interm. file $interm";
    open(IN, "<$in") || die "Cannot open patch input file $in";
    my $inline = 0;
    while(<IN>) {
        $inline ++;
        # Find out how many lines from which file do we need to bring in
        my ($startLine, $fileName) = ($_ =~ m|$linePattern|o);
        my $nrLines   = 0;
        if(! defined($startLine)) {
            die "Expecting a \#line directive at $inline in $in";
        }
        my $currentLine = $startLine;
        while(<IN>) {
            if($_ =~ m|$linePattern|) {
                if($2 ne $fileName) {
                    $currentLine --;
                    my $newFileName = $2;
                    my $newStartLine = $1;
                    if($currentLine >= $startLine) {
                        &copyFileFragment($fileName, $startLine, $currentLine,
                                          \*INTERM);
                    }
                    $startLine = $newStartLine;
                    $fileName  = $newFileName;

                    $currentLine = $startLine;
                } else {
                    $currentLine = $1;
                }
            } else {
                $currentLine ++;
            }
        }
        $currentLine --;
        if($currentLine >= $startLine) {
            &copyFileFragment($fileName, $startLine, $currentLine,
                              \*INTERM);
        }
    }
    close(INTERM) || die "Cannot close patch interm file $interm";
    close(IN) || die "Cannot close patch input file $in";

    # Now call the preprocessor again
    &cpp($interm, $out);
}

my @includeReadAhead = ();
sub readIncludeLine {
    if($#includeReadAhead < 0) {
        my $newLine = <TOINCLUDE>;
        return $newLine;
    } else {
        return shift @includeReadAhead;
    }
}

sub undoReadIncludeLine {
    my($line) = @_;
    push @includeReadAhead, $line;
}


sub copyFileFragment {
    my ($fileName, $startLine, $lastLine, $out) = @_;
    # print "Copying $startLine->$lastLine from $fileName\n";

    &openWithMounts($fileName, \*TOINCLUDE);
    @includeReadAhead = ();

    my $lineno = 0;
    my $copying = 0;
    my $line;
    while($line = &readIncludeLine()) {
        $lineno ++;
        if($lineno == $startLine) {
            $copying = 1;
            # GCC sometimes gives us wrong numbers. We ignore an initial
            # include directive 
            if($compiler eq "gcc" &&
               $line =~ m|^\s*\#\s*include|o) {
                $line = &readIncludeLine();
                $lineno ++;
                if($lineno > $lastLine) {
                    print $out &lineDirective($fileName, $lineno);
                    goto DoneCopying;
                }
            } 
            print $out &lineDirective($fileName, $lineno);
        }
        if($copying) {
            # Now we have a line to print out. See if it needs patching
            my $patch;
            my @lines = ($line); # A number of lines
            my $nrLines = 1;     # How many lines
            my $toundo  = 0;
          NextPatch:
            foreach $patch (@patches) {
                if($patch->{DONE}) { next; } # We are done with this patch
                if($line eq $patch->{HEAD}) {
                    # Now see if all the lines match
                    my $patNrLines = $patch->{NRLINES};
                    if($patNrLines > 1) {
                        # Make sure we have enough lines
                        while($nrLines < $patNrLines) {
                            push @lines, &readIncludeLine();
                            $nrLines ++;
                            $toundo ++;
                        }
                        my @checkLines = @{$patch->{PATTERNS}};
                        my $i;
                        # print "check: ", join(":", @checkLines);
                        # print "with $nrLines lines: ", join("+", @lines);
                        for($i=0;$i<$patNrLines;$i++) {
                            if($checkLines[$i] ne $lines[$i]) {
                                # print "No match for $patch->{HEAD}\n";
                                next NextPatch;
                            }
                        }
                    }
                    # print "Check succeeded\n";
                    # Now replace
                    $lineno += ($patNrLines - 1);
                    $toundo -= ($patNrLines - 1);
                    $line = $patch->{REPLACE};
                    $line .= &lineDirective($fileName, $lineno + 1);
                    # And mark the patch as done
                    if(! $patch->{GLOBAL}) {
                        $patch->{DONE} = 1;
                    }
                    last;
                }
            }
            print $out $line;
            # Now undo all but the first line
            my $i;
            for($i=$nrLines - $toundo;$i<$nrLines;$i++) {
                &undoReadIncludeLine($lines[$i]);
            }
        }
#        print "Copied line $lineno\n";
        if($lineno >= $lastLine) {
          DoneCopying:
            close(TOINCLUDE) 
                || die "Cannot close file $fileName";
            return;
        }
    }
    die "Cannot find lines $startLine-$lastLine in $fileName. Last: $lineno";
}

sub lineDirective {
    my ($fileName, $lineno) = @_;
    if($compiler eq "msvc") {
        return "#line $lineno \"$fileName\"\n";
    } else {
        return "# $lineno \"$fileName\"\n";
    }
}


1;
