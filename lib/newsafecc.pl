#!/usr/bin/perl
# A Perl script that substitutes either the GCC or the MSVC compiler. Invokes
# the preprocessor, then another preprocessor and then the real compiler. 
#
use strict;
use File::Basename;
use File::Copy;
use FindBin;
if($^O eq 'MSWin32') {
    require Win32;
}

sub printHelp {
    print <<EOL;
Wrapper for GCC and MSVC. 
Usage: safecc [args] [cargs]

args: 
  --help       Prints this help message
  --verbose    Prints a lot of information about what is being done
  --mode=xxx   What tool to emulate: 
                gcc     - GNU CC
                mscl    - MS VC cl compiler
                mslink  - MS VC link linker
               By default it chooses between mscl and gcc based on the form of
               the command line arguments

  --patch=xxx  Patches before the preprocessor with the given specification
  --combine    Combine all the sources into one file

The rest are safec specific arguments:

  --cil        Uses the CIL version of the file (default)
  --box        Uses the BOX version of the file
  --inferbox   Infers the pointer types
  --optim      Removes redundant checks
  --cabs       Creates a CABS version of the file
  --release    Runs with the native code version
  --keep=dir   Keep intermediate files in dir
  --safec=xxx  Passes xxx to the safec executable. Multiple occurences of 
               --safec should be used to add more arguments.
  --tr=flag    Passes "-tr flag" (tracing flag) to the safec executable.

Send bugs to necula\@cs.berkeley.edu.
EOL
}

# Go through the arguments and separate the source names from the other
# arguments. Among the arguments separate those that can be passed to the
# preprocessor (@ppargs) and those that should be passed only to the final
# phase (@otherargs)
my @sources  = ();             # The C or C++ sources
my @ppargs = ();               # Arguments for the preprocessor
my @ccargs = ();               # Arguments for the compiler
my @linkargs = ();             # Arguments for the linker
my $outarg;                    # Argument that names the output file

my @otherargs = ();            # All the other arguments

my $compiler;                  # an instance of either MSVC or GCC packages

my $operation = "TOEXE";
   # One of "PREPROC"  - preprocess only
   #        "TOASM"    - compile to assembly language
   #        "TOOBJ"    - compile to object file
   #        "TOLIB"    - link to library
   #        "TOEXE"    - link to executable (default)
my $output = "";                # The file where the result must be placed

my $verbose = 0;
my $doBox = 0;
my $doInferBox = 0;
my $doOptim=0;
my $doCil = 0;
my $doCabs = 0;
my $doCombine = 0;
my $doTV;
my $tvCommand;
my $useRelease = 0;
my $keepDir;                    # Keep outputs here
my $extraArgs = "";

my @patches = (); # Keep here a list of patches

my $bindir = $FindBin::Bin;

my $safecc;

&main();


################# Now the subroutine definitions
sub main {
    # Get the command line arguments
    &collectArguments();
    # Process each source independently
    @sources = &processSources();
    # Finish the operation
    if($operation eq "PREPROC") {
        return;
    }
    if($operation eq "TOOBJ") {
        if($doCombine) {
            &combineSourcesCompile();
        } else {
            $compiler->compile(); 
        }
        return;
    }
    if($operation eq "TOEXE") {
        if($doCombine) {
            &combineSourcesLink();
        } else {
            $compiler->link();
        }
        return;
    }
    die "Don't know what to do: $operation\n";
}

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
    if($compiler->{NAME} eq "gcc" && $^O eq 'MSWin32') {
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

sub collectArguments {
    while($#ARGV >= 0) {
        my $arg = shift @ARGV;
        # Handle first our own arguments
        if($arg eq "--help") {
            &printHelp ();  exit 1;
        }
        if($arg =~ m|^--patch=?(.*)$|) { # Whether to patch
            &preparePatchFile($1); next;
        }
        if($arg =~ m|--mode|) {
            my $mode;
            if($arg =~ m|--mode=?(.+)$|)  {
                $mode = $1;
            } else {
                $mode = shift @ARGV;
            }
            # Now set the compiler mode
            &setCompilerMode($mode, $arg);
            next;
        }
        if($arg eq "--verbose") {
            $verbose = 1; next;
        }
        if($arg =~ m|^--keep=(.+)$|) { # Where to keep the outputs
            $keepDir = $1; next;
        }
        if($arg =~ m|^--safec=(.*)$|) { # Extra arguments
            $extraArgs .= " $1"; next;
        }
        if($arg =~ m|^--tr=(.*)$|) {    # Extra tracing arguments
            $extraArgs .= " -tr $1";  next;
        }
        if($arg eq "--box") { # Whether to box or not
            $doBox = 1;   next;
        }
        if($arg eq "--combine") { # Whether to combine or not
            $doCombine = 1;   next;
        }

        if($arg eq "--inferbox") { # Whether to infer the boxing or not
            $doInferBox = 1; next;
        }
        if($arg eq "--cil") { # Whether to generate the cil or not
            $doCil = 1; next;
        }
        if($arg =~ m|^--tv=?(.*)$|) { # Whether to use translation validation.
            $doTV = $1;  next;
        }
        if($arg eq "--cabs") { # Whether to generate the cabs or not
            $doCabs = 1;  next;
        }
        if($arg eq "--release") {
            $useRelease = 1;  next;
        }
        # now detect whether we are in MSVC or in GCC
        if($arg =~ m|^-|) {
            &setCompilerMode("gcc", $arg);
        }
        # weimer: this regexp does not catch /unix/path/to/file
        # as an MSVC extension
        if($arg =~ m|^/[^/]+$| && ! defined($compiler)) {
            &setCompilerMode("mscl", $arg);
        }
        if(! defined $compiler) {     # Must be a source file, since we have
                                      # not seen an option yet
            push @sources, $arg;
            next;
        }
        &classifyArguments($compiler->{OPTIONS}, $arg);
    }
    # Fixup some arguments
    if(defined $keepDir && $keepDir !~ m|[/\\]$|) {
        $keepDir = $keepDir . "/";
    }
    my $version = $useRelease ? "asm" : "byte";

    $safecc = "$bindir/../obj/safec.$version.exe";
    $safecc .= " -cabsindent 2 -verbose $extraArgs ";
    if($compiler->{NAME} eq "msvc") {
        $safecc .= " -msvc ";
    }

    if($doBox) {
        push @ppargs, $compiler->{DEFARG} . 'BEFOREBOX';
        if($operation eq "TOEXE") {
            push @sources, 
            "$bindir/../obj/safec" . 
                ($useRelease ? $compiler->{LIBEXT} : 
                               "debug$compiler->{LIBEXT}");
        }
        push @ccargs, $compiler->{INCARG} . $bindir;
        push @ccargs, $useRelease ? $compiler->{OPTIM} : $compiler->{DEBUG};
    } else {
        $doCil = 1;
    }
    print $compiler->{NAME}, " mode\n";
    
    if(defined $doTV) {
        if($doTV ne "") { $doTV .= " -debug"; }

        if($compiler->{NAME} ne "gcc") {
            print "***WARNING: Translation validation only works with GCC\n";
            undef $doTV;
        }

       # Find the translation validator
        my $which = $doTV eq "" ? "asm" : "byte";
        my $tvdir = path("$bindir/../../../Source/TransVal");
        $tvCommand = path("$tvdir/obj/transval.$which.exe");
        if(-f $tvCommand) {
            print "Found $tvCommand\n";
        }
    }
    
}



#
# Prepare for patching
#

sub processSources {
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
        push @ppargs, $compiler->forceIncludeArg("$bindir/fixup.h");
        $compiler->cpp($src, $afterpp);
        # Now see if we must patch the file
        my $afterPatchExtension = ".i";
        if($#patches >= 0) {
            &patchFile($dir, $base, ".i");
        } 
        # Now run our own preprocessor
        if(! $doCombine) {
            push @newsources, 
                 &safecPreprocessor($dir, $base, $afterPatchExtension);
        } else {
            push @newsources, "$dir$base$afterPatchExtension";
        }
    }
    @sources = @newsources;
}

# Now we have computed the new sources


################ Now the subroutines
    
sub classDebug {
    if(0) { print @_; }
}
sub classifyArguments {
    my($options, $arg) = @_;

    &classDebug("Classifying arg: $arg\n");
    my $idx = 0;
    for($idx=0; $idx < $#{$options}; $idx += 2) {
        my $key = ${$options}[$idx];
        my $action = ${$options}[$idx + 1];
        &classDebug("Try match with $key\n");
        if($arg =~ m|^$key|) {
            &classDebug(" match with $key\n");
            my $onemore;
            if(defined $action->{'ONEMORE'}) {
                &classDebug("  expecting one more\n");
                if($arg eq $key) { # Grab the next argument
                    $onemore = shift @ARGV;
                } else { # The next argument is actually attached
                    ($arg, $onemore) = ($arg =~ m|^($key)(.+)$|);
                }
                $onemore = &quoteIfNecessary($onemore);
            }
            &classDebug(" onemore=$onemore\n");
            my $fullarg = defined($onemore) ? "$arg $onemore" : $arg;
            # Now see what action we must perform
            if(defined $action->{'TYPE'}) {
                &classDebug("  type=$action->{TYPE}\n");
                if($action->{TYPE} eq "PREPROC") {
                    push @ppargs, $fullarg; return;
                }
                if($action->{TYPE} eq "CC") {
                    push @ccargs, $fullarg; return;
                }
                if($action->{TYPE} eq "LINK") {
                    push @linkargs, $fullarg; return;
                }
                if($action->{TYPE} eq "SOURCE") {
                    push @sources, $fullarg; return;
                }
                if($action->{TYPE} eq 'OUT') {
                    if(defined($outarg)) {
                        print "Warning: output file is multiply defined: $outarg and $fullarg\n";
                    }
                    $outarg = $fullarg; return;
                }
                print "  Do not understand TYPE\n"; return;
            }
            if(defined $action->{'RUN'}) {
                &{$action->{'RUN'}}($fullarg, $onemore); return;
            }
            print "Don't know what to do with $arg\n"; return;
        }
    }
    print "Unrecognized option $arg\n";
}


sub runShell {
    my ($cmd) = @_;

    # sm: I want this printed to stderr instead of stdout
    # because the rest of 'make' output goes there and this
    # way I can capture to a coherent file
    print STDERR "Running $cmd\n";

    # weimer: let's have a sanity check
    if (system($cmd)) {
	die "Possible error with $cmd!\n";
    }
}



sub setCompilerMode {
    my ($mode, $arg) = @_;
    if(defined $compiler && $compiler->{MODENAME} ne $mode) {
        die "Found a $mode argument ($arg) while in $compiler->{NAME} mode";
    }
    if($mode eq "mscl") {
        $compiler = MSCL->new; return;
    }
    if($mode eq "gcc") {
        $compiler = GCC->new; return;
    }
    if($mode eq "mslink") {
        $compiler = MSLINK->new; return;
    }
    die "Don't know about compiler $mode\n";
}


#####################################################################
# Patching of files
#
sub preparePatchFile {
    my ($pFile) = @_;
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

sub patchFile {
    my($dir, $base, $inext) = @_;
    
    my $in = "$dir$base$inext";
    my $interm = "$dir$base" . "_ppp.c";
    my $out = $in;
    my  $linePattern = $compiler->{LINEPATTERN};
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
    $compiler->cpp($interm, $out);
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
            if($compiler->{NAME} eq "gcc" &&
               $line =~ m|^\s*\#\s*include|o) {
                $line = &readIncludeLine();
                $lineno ++;
                if($lineno > $lastLine) {
                    print $out $compiler->lineDirective($fileName, $lineno);
                    goto DoneCopying;
                }
            } 
            print $out $compiler->lineDirective($fileName, $lineno);
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
                    $line .= $compiler->lineDirective($fileName, $lineno + 1);
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



########################################################################
###
### COMBINE SOURCES
###
###
sub combineSourcesCompile {
    my $src;
    foreach $src (@sources) {
        # Get the name of the output file
        my $outfile = $compiler->compileOutputFile($src);
        open(OUT, ">$outfile") 
            || die "Cannot create $outfile";
        print "Creating fake object file $outfile from $src\n";
        print OUT "#pragma combiner(\"$src\", \"" . 
            join(' ', @ccargs), "\")\n";
        open(IN, "<$src") 
            || die "Cannot read $src";
        print OUT <IN>;
        close(OUT);
        close(IN);
    }
}

sub combineSourcesLink {
    # Go over all the sources and separate those that are C
    my $src;
    my @objfiles = ();
    my @tocombine = ();
    foreach $src (@sources) {
        my ($base, $dir, $ext) = fileparse($src, "\\.[^\\.]+");
        if($ext eq ".obj" || $ext eq ".o" || $ext eq ".lib" || $ext eq ".a") {
            # Look inside and see if it is a C or not
            open(IN, "<$src") || die "Cannot read $src";
            my $fstline = <IN>;
            if($fstline =~ m|\#pragma combiner\(|) {
                push @tocombine, $src; next; 
            } else {
                push @objfiles, $src; next;
            }
        }
        if($ext eq ".c" || $ext eq ".i") {
            push @tocombine, $src; next;
        }
        die "I don't know how to combine (link) $src\n";
    }
    # Now call the combiner 
    my $outfile = $compiler->linkOutputFile();
    
    my $cmd = "$safecc -combine $outfile.c " . join(' ', @tocombine);
    &::runShell($cmd);
    # Now call the true linker
    @sources = ("$outfile.c", @objfiles);
    $compiler->link();
}

########################################################################
###
### SAFEC CODE
###
sub safecPreprocessor {
    my ($dir, $base, $ext) = @_;
    my $src = "$dir$base$ext";
    my $version = $useRelease ? "asm" : "byte";
    my $safecc = "$bindir/../obj/safec.$version.exe";
    my $cmd = "$safecc -cabsindent 2 -verbose $extraArgs ";
    if($compiler->{NAME} eq "msvc") {
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
        my $makeRTL = "gcc -g -c -dr ";
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

###########################################################################
####
#### MS CL specific code
####
package MSCL;

use strict;
use File::Basename;

sub new {
    my ($proto, %args) = @_;
    my $class = ref($proto) || $proto;
    # Create $self

    my $self = 
    { NAME => 'Microsoft cl compiler',
      MODENAME => 'mscl',
      DEFARG  => "/D",
      INCARG  => "/I",
      DEBUGARG => "/Zi /MLd /DEBUG",
      OPTIMARG => "/Ox /G6",
      OBJEXT => "obj",
      LIBEXT => ".lib",   # Library extension (without the .)
      EXEEXT => ".exe",  # Executable extension (with the .)
      LINEPATTERN => "^#line\\s+(\\d+)\\s+\"(.+)\"",

      OPTIONS => 
# Describe the compiler options as a list of patterns and associated actions. 
# The patterns are matched in order against the begining of the argument.
#
# If the action contains ONEMORE => 1 then the argument is expected to be
# parameterized by a following word. The word can be attached immediately to
# the end of the argument or in a separate word. 
#
# If the action contains TYPE => "..." then the argument is put into one of
# several lists, as follows: "PREPROC" in ppargs, "CC" in ccargs, "LINK" in
# linkargs, "SOURCE" in sources, "OUT" in outarg.
#
# If the TYPE is not defined but the RUN => sub { ... } is defined then the
# given subroutine is invoked with the argument and the (possibly empty)
# additional word.
#
          ["[^/]" => { TYPE => "SOURCE" },
           "/O" => { TYPE => "CC" },
           "/G" => { TYPE => "CC" },
           "/[DI]" => { TYPE => "PREPROC"},
           "/EH" => { TYPE => "CC" },
           "/G"  => { TYPE => "CC" },
           "/F[aA]"  => { TYPE => 'CC' },
           "/Fo"     => { TYPE => 'OUT' },
           "/F[e]"   => { TYPE => 'OUT' },
           "/F[dprR]" => { TYPE => "CC" },
           "/[CXu]" => { TYPE => "PREPROC" },
           "/U" => { ONEMORE => 1, TYPE => "PREPROC" },
           "/(E|EP|P)" => { RUN => sub { push @ppargs, $_[0]; 
                                         $operation = "PREPROC"; }},
           "/c" => { RUN => sub { $operation = "TOOBJ"; }},
           "/(Q|Z|J|nologo|TC|TP|w|W|Yd|Zm)" => { TYPE => "CC" },
           "/v(d|m)" => { TYPE => "CC" },
           "/[MF]" => { TYPE => "CC" },
           "/link" => { RUN => sub { push @linkargs, "/link", @ARGV;
                                     @ARGV = (); } },
           "/"  => { RUN => 
                         sub { print "Unimplemented MSVC argument $_[0]\n"; }},
           ],
      };
    bless $self, $class;
    return $self;
}


sub forceIncludeArg { 
    my($self, $what) = @_;
    return "/FI$what";
}

sub cpp {
    my ($self, $src, $afterpp) = @_;
#    my ($base, $dir, $ext) = fileparse($afterpp, 
#                                       "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    my ($sbase, $sdir, $sext) = fileparse($src, 
                                       "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    my $cmd = "cl /nologo /P /D_MSVC " . join(' ', @ppargs);
    &::runShell("$cmd $src");
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

# Now run the real compilation
sub compile {
    my $cmd = "cl /nologo /D_MSVC /c " .
        join(' ', @ccargs) . " " . 
            join(' ', @sources) . " $outarg";
    &::runShell($cmd);
}

sub link {
    my $cmd = "cl /nologo /D_MSVC " .
        join(' ', @ccargs) . " " . 
            join(' ', @linkargs) . " " .
                join(' ', @sources) . " $outarg";
    &::runShell($cmd);
}


# Emit a line # directive
sub lineDirective {
    my ($self, $fileName, $lineno) = @_;
    return "#line $lineno \"$fileName\"\n";
}

# The name of the output file
sub compileOutputFile {
    my($self, $src) = @_;
    if($outarg =~ m|/Fo(.+)|) {
        return $1;
    }
    my ($base, $dir, $ext) = fileparse($src, 
                                       "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    if(! defined($ext) || $ext eq "") { # Not a C source
        die "objectOutputFile: not a C source file\n";
    }
    return "$dir/$base.obj";
}

sub linkOutputFile {
    my($self, $src) = @_;
    if($outarg =~ m|/Fe(.+)|) {
        return $1;
    }
    die "I do not know what is the link output file\n";
}

#########################################################################
###
###  MS LINK speciific code
###
####
package MSLINK;

use strict;
use File::Basename;

sub new {
    my ($proto, %args) = @_;
    my $class = ref($proto) || $proto;
    # Create $self

    my $self = 
    { NAME => 'Microsoft linker',
      MODENAME => 'mslink',
      DEFARG  => " ??DEFARG",
      INCARG  => " ??INCARG",
      DEBUGARG => "/DEBUG",
      OPTIMARG => "",
      OBJEXT => "obj",
      LIBEXT => ".lib",   # Library extension (without the .)
      EXEEXT => ".exe",  # Executable extension (with the .)
      LINEPATTERN => "", 

      OPTIONS => 
          ["[^/]" => { TYPE => "SOURCE" },
           "/OUT:" => { TYPE => 'OUT' },
           "/"  => { TYPE => 'LINK' },
           ],
      };
    bless $self, $class;
    return $self;
}



## Now run the real compilation
#sub compile {
#    my $cmd = "cl /D_MSVC /c " .
#        join(' ', @ccargs) . " " . 
#            join(' ', @sources) . " $outarg";
#    &::runShell($cmd);
#}

# Compile and link
sub link {
    my $cmd = "cl /nologo /D_MSVC " .
        join(' ', @sources) . " /link $outarg " . join(' ', @linkargs) ;
    &::runShell($cmd);
}


## Emit a line # directive
#sub lineDirective {
#    my ($self, $fileName, $lineno) = @_;
#    return "#line $lineno \"$fileName\"\n";
#}

## The name of the output file
#sub compileOutputFile {
#    my($self, $src) = @_;
#    if($outarg =~ m|/Fo(.+)|) {
#        return $1;
#    }
#    my ($base, $dir, $ext) = fileparse($src, 
#                                       "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
#    if(! defined($ext) || $ext eq "") { # Not a C source
#        die "objectOutputFile: not a C source file\n";
#    }
#    return "$dir/$base.obj";
#}

sub linkOutputFile {
    my($self, $src) = @_;
    if($outarg =~ m|/OUT:(.+)|) {
        return $1;
    }
    die "I do not know what is the link output file\n";
}

##########################################################################
###
### GCC specific code
###
package GCC;

use strict;

sub new {
    my ($proto, %args) = @_;
    my $class = ref($proto) || $proto;
    # Create $self

    my $self = 
    { NAME => 'GNU CC',
      MODENAME => 'gcc',
      DEFARG  => "-D",
      INCARG => "-I",
      DEBUGARG => "-g",
      OPTIMARG => "-O4",
      OBJEXT => "o",
      LIBEXT => "a",
      EXEEXT => "",
      LINEPATTERN => "^#\\s+(\\d+)\\s+\"(.+)\"",
      
      OPTIONS => 
          [ "[^-]" => { TYPE => "SOURCE" },
            "-[DI]" => { ONEMORE => 1, TYPE => "PREPROC" },
            "-c" => { RUN => sub { $operation = "TOOBJ"; }},
            "-x" => { ONEMORE => 1, TYPE => "CC" },
            "-O" => { TYPE => "CC" },
            "-S" => { RUN => sub { $operation = "TOOBJ";
                                   push @ccargs, $_[0]; }},
            "-o" => { ONEMORE => 1, TYPE => 'OUT' },
            "-" => { RUN => sub { print "Unimplemented GCC arg: $_[0]\n";
                                  push @ccargs, $_[0]; }}
            ],
                                  
      };
    bless $self, $class;
    return $self;
}
#    # GCC
#    if($compiler eq "gcc") {
#        if($arg =~ m|^-|) {
#            if($arg eq "-P") {
#                $operation = "PREPROC";
#                next;
#            }
#            if ($arg eq "-S") {
#                $operation = "TOASM";
#                next;
#            }
#            if ($arg eq "-c") {
#                $operation = "TOOBJ";
#                next;
#            }
#            if($arg eq "-o") {
#                $arg = shift @ARGV;
#                $output = $arg;
#                next;
#            }
#            if($arg =~ m|^-o(.+)$|) {
#                $output = $1;
#                next;
#            }
#            push @ppargs, &quoteIfNecessary($arg);
#            next;
#        } 
#        push @sources, &quoteIfNecessary($arg);
#        next;
#    }
#}

sub forceIncludeArg { 
    my($self, $what) = @_;
    return "-include $what";
}

sub cpp {
    my ($self, $src, $afterpp) = @_;
    my $cmd = "gcc -E -D_GNUCC " .
        join(' ', @ppargs) . " -o $afterpp";
    &::runShell("$cmd $src");
}

sub compile {
    my $cmd = "gcc -D_GNUCC -c " .
        join(' ', @ccargs) . " " . 
            join(' ', @sources) . " $outarg";
    &::runShell($cmd);
}

sub link {
    my $cmd = "gcc -D_GNUCC " .
        join(' ', @ccargs) . " " . 
            join(' ', @linkargs) . " " .
                join(' ', @sources) . " $outarg";
    &::runShell($cmd);
}

# Emit a line # directive
sub lineDirective {
    my ($self, $fileName, $lineno) = @_;
    return "# $lineno \"$fileName\"\n";
}

1;



