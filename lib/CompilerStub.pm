# This module implements a compiler stub that parses the command line
# arguments of gcc and Microsoft Visual C (along with some arguments for the
# script itself) and gives hooks into preprocessing, compil;ation and linking. 
package CompilerStub;
@ISA = ();

use strict;
use File::Basename;
use File::Copy;
use FindBin;
if($^O eq 'MSWin32') {
    require Win32;
}

# Pass to new a list of command arguments
sub new {
    my ($proto, @args) = @_;
    my $class = ref($proto) || $proto;

    my $ref = 
    { CFILES => [], # C input files
      OFILES => [], # Other input files
      IFILES => [], # Already preprocessed files
      PPARGS => [], # Preprocessor args
      CCARGS => [], # Compiler args
      LINKARGS => [], # Linker args
      OPERATION => "UNKNOWN",
    };
    my $self = bless $ref, $class;

    # Scan and process the arguments
    while($#args >= 0) {
        $self->collectOneArgument(\@args);
    }
    if(!defined($self->{MODENAME})) { # No compiler was defined. Use gcc
        $self->setCompilerMode("gcc", ""); 
    }
    return $self;
}

# Set the compiler mode for the current stub
sub setCompilerMode {
    my ($self, $mode, $arg) = @_;
    if(defined $self->{COMPILER} && $self->{MODENAME} ne $mode) {
        die "Found a $mode argument ($arg) while in $self->{MODENAME} mode";
    }
    my $compiler;
    if($mode eq "mscl") {
        unshift @CompilerStub::ISA, qw(MSCL);
        $compiler = MSCL->new($self);
    } elsif($mode eq "gcc") {
        unshift @CompilerStub::ISA, qw(GCC);
        $compiler = GCC->new($self);
    } elsif($mode eq "mslink") {
        unshift @CompilerStub::ISA, qw(MSLINK);
        $compiler = MSLINK->new($self);
    } else {
        die "Don't know about compiler $mode\n";
    }
    # Now grab the fields from the compiler and put them inside self
    my $key;
    foreach $key (keys %{$compiler}) {
        $self->{$key} = $compiler->{$key};
    }
}



# Collecting arguments 
sub collectOneArgument {
    my($self, $pargs) = @_;
    my $arg = shift @{$pargs}; # Grab the next one
    if($arg eq "--help") {
        $self->printHelp(); exit 1;
    }
    if($arg =~ m|--mode|) {
        my $mode;
        if($arg =~ m|--mode=?(.+)$|)  {
            $mode = $1;
        } else {
            $mode = shift @{$pargs};
        }
        # Now set the compiler mode
        $self->setCompilerMode($mode, $arg);
        return;
    }
    if($arg eq "--verbose") {
        $self->{verbose} = 1; return;
    }
    # This is the end of the CompilerStub arguments. We must be dealing with
    # compiler arguments now
    # now detect whether we are in MSVC or in GCC
    if($arg =~ m|^-|) { # All gcc arguments start with -
        $self->setCompilerMode("gcc", $arg);
    }
    # all MSVC arguments start with /. But so do absolute names. We use here
    # the heuristic that if you invoke a compiler on an absolute file name
    # then it probably has another / in the name (which the options don't). If
    # this heuristic fails for you use the --mode option
    if($arg =~ m|^/[^/]+$|) {
        $self->setCompilerMode("mscl", $arg);
    }
    if(! defined $self->{MODENAME}) { # Must be a source file, since we have
        # not seen an option yet
        my ($base, $dir, $ext) = 
            fileparse($arg, "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
        if($ext eq ".i") {
            push @{$self->{IFILES}}, $arg;
        } elsif($ext ne "") {
            push @{$self->{CFILES}}, $arg;
        } else {
            push @{$self->{OFILES}}, $arg;
        }
        next;
    }
    # Now try to classify the argument and collect it appropriately
    $self->classifyArgument($self->{OPTIONS}, $arg, $pargs);
    return;
}



# PREPROCESSING
sub cpp {
    my ($self, $src, $dest, @args) = @_;
    my $res;
    if($self->{MODENAME} eq "mscl") {
        $self->MSCL::cpp($src, $dest, @args);
    } else {
        my $cmd = $self->{CPP} . " " . 
            join(' ', @args) .  " " .
                "$src " . $self->{CPPOUT} . $dest;
        $res = $self->runShell($cmd);
        
    }
    return $res;
}

# COMPILATION (with PREPROCESSING)
sub compile {
    my ($self, $src, $dest, @args) = @_;
    $dest = $dest eq "" ? "" : $self->{OUTOBJ} . $dest;
    my $forcec = $self->{FORCECSOURCE};
    my $cmd = $self->{CC} . " " . join(' ', @args) .  " $dest $forcec$src";
    return $self->runShell($cmd);
}

# LINKING (with COMPILATION and PREPROCESSING)
sub link {
    my ($self, $psrcs, $dest, @args) = @_;
    my @sources = ref($psrcs) ? @{$psrcs} : ($psrcs);
    $dest = $dest eq "" ? "" : $self->{EXEOUT} . $dest;
    my $cmd = $self->{CC} . " " . 
        join(' ', @args) .  " " . join(' ', @sources). " $dest";
    return $self->runShell($cmd);
}


# DO EVERYTHING
sub doit {
    my ($self) = @_;
    my $file;
    my $out;

    if($self->{OPERATION} eq "UNKNOWN") {
        print "Warning: CompilerStub does not understand the operation\n";
        my @allfiles = (@{$self->{CFILES}}, @{$self->{OFILES}});
        $self->link(\@allfiles, "", 
                    @{$self->{PPARGS}}, 
                    @{$self->{CCARGS}}, 
                    @{$self->{LINKARGS}});
        return;
    }
    # We'll first preprocess the C files
    my @ppfiles = (@{$self->{IFILES}});
    foreach $file (@{$self->{CFILES}}) {
        my ($base, $dir, $ext) = fileparse($file, "\\.[^.]+");
        # print "base=$base, dir=$dir, idir=$idir, ext=$ext\n";
        my $idir = $dir;
        if(defined $::keepDir) {
            $idir = $::keepDir;
        }
        $out    = "$idir$base.i";

        # Now call preprocessor
        $self->cpp($file, $out, @{$self->{PPARGS}});

        push @ppfiles, $out;
    }
    # See if we must stop here
    if($self->{OPERATION} eq 'PREPROC') {
        # Copy the preprocessed file to the desired output
        my $isstdout = 1;
        foreach $file (@ppfiles) {
            open(FILE, "<$file");
            print <FILE>;
            close(FILE);
        }
        return;
    }

    my @ofiles = @{$self->{OFILES}};
    if($#ofiles > 1 && defined($self->{OUTARG})) {
        die "Cannot specify an output file with multiple compilations\n";
    }
    # Now we compile the files one by one
    foreach $file (@ppfiles) {
        # Find out what output file to use
        $out = $self->compileOutputFile($file);
        # Now call the compiler
        $self->compile($file, $out, @{$self->{PPARGS}}, @{$self->{CCARGS}});
        push @ofiles, $out;
    }

    # See if we must stop after compilation
    if($self->{OPERATION} eq "TOOBJ") {
        return;
    }

    # Now link all of the files
    $out = $self->linkOutputFile();
    $self->link(\@ofiles,  $out, 
                @{$self->{PPARGS}}, @{$self->{CCARGS}}, @{$self->{LINKARGS}});
}

sub classDebug {
    if(0) { print @_; }
}

sub classifyArgument {
    my($self, $options, $arg, $pargs) = @_;
    
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
            $onemore = shift @{$pargs};
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
                    push @{$self->{PPARGS}}, $fullarg; return;
                }
                if($action->{TYPE} eq "CC") {
                    push @{$self->{CCARGS}}, $fullarg; return;
                }
                if($action->{TYPE} eq "LINKCC") {
                    push @{$self->{CCARGS}}, $fullarg; 
                    push @{$self->{LINKARGS}}, $fullarg; return;
                }
                if($action->{TYPE} eq "LINK") {
                    push @{$self->{LINKARGS}}, $fullarg; return;
                }
                if($action->{TYPE} eq "CSOURCE") {
                    push @{$self->{CFILES}}, $fullarg; return;
                }
                if($action->{TYPE} eq "OSOURCE") {
                    push @{$self->{OFILES}}, $fullarg; return;
                }
                if($action->{TYPE} eq "ISOURCE") {
                    push @{$self->{IFILES}}, $fullarg; return;
                }
                if($action->{TYPE} eq 'OUT') {
                    if(defined($self->{OUTARG})) {
                        print "Warning: output file is multiply defined: $self->{OUTARG} and $fullarg\n";
                    }
                    $self->{OUTARG} = $fullarg; return;
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
    my ($self, $cmd) = @_;

    # sm: I want this printed to stderr instead of stdout
    # because the rest of 'make' output goes there and this
    # way I can capture to a coherent file
    print STDERR "$cmd\n";

    # weimer: let's have a sanity check
    if (system($cmd)) {
	die "Possible error with $cmd!\n";
    }
}

sub quoteIfNecessary {
    my($arg) = @_;
    # If it contains spaces or "" then it must be quoted
    if($arg =~ m|\s| || $arg =~ m|\"|) {
        return "\'$arg\'";
    } else {
        return $arg;
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

# Return true if paths should have backward slashes.
my $pathSep = ($^O eq "MSWin32") ? '\\' : '/';
my $dirListSep = ($^O eq "MSWin32") ? ';' : ':'; 
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




###########################################################################
####
#### MS CL specific code
####
package MSCL;

use strict;
use File::Basename;

sub new {
    my ($proto, $stub) = @_;
    my $class = ref($proto) || $proto;
    # Create $self

    my $self = 
    { NAME => 'Microsoft cl compiler',
      MODENAME => 'mscl',
      CC => 'cl /nologo /D_MSVC /c',
      CPP => 'cl /nologo /D_MSVC /P',
      LD => 'cl /nologo /D_MSVC',
      DEFARG  => "/D",
      INCARG  => "/I",
      DEBUGARG => "/Zi /MLd /DEBUG",
      OPTIMARG => "/Ox /G6 ",
      OBJEXT => "obj",
      LIBEXT => "lib",   # Library extension (without the .)
      EXEEXT => ".exe",  # Executable extension (with the .)
      OUTOBJ => "/Fo",
      OUTEXE => "/Fe",
      FORCECSOURCE => "/Tc",
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
# linkargs, "LINKCC" both in ccargs and linkargs, "CSOURCE" in cfiles,
# "OSOURCE" in ofiles, "ISOURCE" in ifiles, "OUT" in outarg. 
#
# If the TYPE is not defined but the RUN => sub { ... } is defined then the
# given subroutine is invoked with the argument and the (possibly empty)
# additional word.
#
          ["[^/].*\\.(c|i|cpp|cc)" => { TYPE => 'CSOURCE' },
           "[^/].*\\.i" => { TYPE => 'ISOURCE' },
           "[^/]" => { TYPE => "OSOURCE" },
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
           "/(E|EP|P)" => { RUN => sub { push @{$stub->{PPARGS}}, $_[0]; 
                                         $stub->{OPERATION} = "PREPROC"; }},
           "/c" => { RUN => sub { $stub->{OPERATION} = "TOOBJ"; }},
           "/(Q|Z|J|nologo|TC|TP|w|W|Yd|Zm)" => { TYPE => "CC" },
           "/v(d|m)" => { TYPE => "CC" },
           "/F" => { TYPE => "CC" },
           "/M"   => { TYPE => 'LINKCC' },
           "/link" => { RUN => sub { push @{$stub->{LINKARGS}}, "/link", @ARGV;
                                     @ARGV = (); } },
           "/"  => { RUN => 
                         sub { print "Unimplemented MSVC argument $_[0]\n"; }},
           ],
      };
    bless $self, $class;
    return $self;
}


sub cpp {
    my($self, $src, $dest, @args) = @_;
    my $res;
    my ($sbase, $sdir, $sext) = 
        fileparse($src, 
                  "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    my $cmd = "cl /nologo /P /D_MSVC " . join(' ', @args);
    $res = $self->runShell("$cmd $src");
    # MSVC cannot be told where to put the output. But we know that it
    # puts it in the current directory
    my $msvcout = "./$sbase.i";
    my @st1 = stat $msvcout;
    my @st2 = stat $dest;
    while($#st1 >= 0) {
        if(shift @st1 != shift @st2) {
#                print "$msvcout is NOT the same as $afterpp\n";
            if($self->{VERBOSE}) {
                print "Copying $msvcout to $dest\n";
            }
            unlink $dest;
            &File::Copy::copy($msvcout, $dest);
            unlink $msvcout;
            return $res;
        }
    }
    return $res;
}

sub forceIncludeArg { 
    my($self, $what) = @_;
    return "/FI$what";
}


    # MSCL does not understand the extension .i, so we tell it it is a C file
sub fixupCsources {
    my (@csources) = @_;
    my @mod_csources = ();
    my $src;
    foreach $src (@csources) {
        my ($sbase, $sdir, $sext) = fileparse($src, 
                                              "\\.[^.]+");
        if($sext eq ".i") {
            push @mod_csources, "/Tc";
        }
        push @mod_csources, $src;
    }
    return @mod_csources;
}


# Emit a line # directive
sub lineDirective {
    my ($self, $fileName, $lineno) = @_;
    return "#line $lineno \"$fileName\"\n";
}

# The name of the output file
sub compileOutputFile {
    my($self, $src) = @_;
    if($self->{OUTARG} =~ m|/Fo(.+)|) {
        return $1;
    }
    my ($base, $dir, $ext) = fileparse($src, 
                                       "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    if(! defined($ext) || $ext eq "") { # Not a C source
        die "objectOutputFile: not a C source file\n";
    }
    return "$base.obj"; # In the current directory
}

sub linkOutputFile {
    my($self, $src) = @_;
    if($self->{OUTARG} =~ m|/Fe(.+)|) {
        return $1;
    }
    die "I do not know what is the link output file\n";
}

sub version {
    my($self) = @_;
    my $cversion = "";
    open(VER, "cl " . join(' ', @{$self->{PPARGS}}) . " 2>&1|") 
        || die "Cannot start Microsoft CL\n";
    while(<VER>) {
        if($_ =~ m|Compiler Version (\S+) |) {
            $cversion = "cl_$1";
            close(VER);
            return $cversion;
        }
    }
    die "Cannot find Microsoft CL version\n";
}

########################################################################
##
##  MS LINK specific code
##
###
package MSLINK;

use strict;

use File::Basename;

sub new {
    my ($proto, $stub) = @_;
    my $class = ref($proto) || $proto;
    # Create $self

    my $self = 
    { NAME => 'Microsoft linker',
      MODENAME => 'mslink',
      CC => 'no_compiler_in_mslink_mode',
      CPP => 'no_compiler_in_mslink_mode',
      LD => 'cl /nologo',
      DEFARG  => " ??DEFARG",
      INCARG  => " ??INCARG",
      DEBUGARG => "/DEBUG",
      OPTIMARG => "",
      OBJEXT => "obj",
      LIBEXT => "lib",   # Library extension (without the .)
      EXEEXT => ".exe",  # Executable extension (with the .)
      OUTOBJ => " ??OUTOBJ",
      OUTEXE => "/OUT:",
      LINEPATTERN => "", 

      OPTIONS => 
          ["[^/]" => { TYPE => 'OSOURCE' },
           "/OUT:" => { TYPE => 'OUT' },
           "/"  => { TYPE => 'LINK' },
           ],
      };
    bless $self, $class;
    return $self;
}


sub forceIncludeArg {  # Same as for CL
    my($self, $what) = @_;
    return "/FI$what";
}



sub linkOutputFile {
    my($self, $src) = @_;
    if($self->{OUTARG} =~ m|/OUT:(.+)|) {
        return $1;
    }
    die "I do not know what is the link output file\n";
}

#########################################################################
##
## GCC specific code
##
package GCC;

use strict;

use File::Basename;

sub new {
    my ($proto, $stub) = @_;
    my $class = ref($proto) || $proto;
    # Create $self

    my $self = 
    { NAME => 'GNU CC',
      MODENAME => 'gcc',  # do not change this since it is used in code
      CC => "gcc -D_GNUCC -c ",
      LD => "gcc -D_GNUCC",
      CPP => "gcc -D_GNUCC -P",
      DEFARG  => "-D",
      INCARG => "-I",
      DEBUGARG => "-g",
      OPTIMARG => "-O4",
      CPROFILEARG => "-pg ",
      LPROFILEARG => "-pg ",
      OBJEXT => "o",
      LIBEXT => "a",
      EXEEXT => "",
      OUTOBJ => "-o ",
      OUTEXT => "-o ",
      OUTCPP => "-o ",
      LINEPATTERN => "^#\\s+(\\d+)\\s+\"(.+)\"",
      
      OPTIONS => 
          [ "[^-].*\\.(c|cpp|cc)" => { TYPE => 'CSOURCE' },
            "[^-].*\\.i" => { TYPE => 'ISOURCE' },
            "[^-]" => { TYPE => 'OSOURCE' },
            "-[DI]" => { ONEMORE => 1, TYPE => "PREPROC" },
            "-include" => { ONEMORE => 1, TYPE => "PREPROC" },  # sm
            "-c" => { RUN => sub { $stub->{OPERATION} = "TOOBJ"; }},
            "-x" => { ONEMORE => 1, TYPE => "CC" },
            "-O" => { TYPE => "CC" },
            "-S" => { RUN => sub { $stub->{OPERATION} = "TOOBJ";
                                   push @{$stub->{CCARGS}}, $_[0]; }},
            "-o" => { ONEMORE => 1, TYPE => 'OUT' },
            "-p" => { TYPE => 'LINKCC' },
            "-W" => { TYPE => 'CC' },
            "-g" => { RUN => sub { push @{$stub->{CCARGS}}, $_[0];
                                   push @{$stub->{LINKARGS}}, $_[0]; }},
            "-l" => { TYPE => 'LINK' },
            "-f" => { TYPE => 'LINKCC' },
            "-r" => { TYPE => 'LINK' },
            "-m" => { TYPE => 'LINKCC' },
            "-traditional" => { TYPE => 'PREPROC' },
            "-" => { RUN => sub { print "Unimplemented GCC arg: $_[0]\n";
                                  push @{$stub->{CCARGS}}, $_[0]; }}
            ],
                                  
      };
    bless $self, $class;
    return $self;
}

sub forceIncludeArg { 
    my($self, $what) = @_;
    return "-include $what";
}


# Emit a line # directive
sub lineDirective {
    my ($self, $fileName, $lineno) = @_;
    return "# $lineno \"$fileName\"\n";
}

# The name of the output file
sub compileOutputFile {
    my($self, $src) = @_;
    if($self->{OUTARG} =~ m|-o (.+)|) {
        return $1;
    }
    my ($base, $dir, $ext) = fileparse($src, 
                                       "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    if(! defined($ext) || $ext eq "") { # Not a C source
        die "objectOutputFile: not a C source file\n";
    }
    return "$base.o"; # In the current directory
}

sub linkOutputFile {
    my($self, $src) = @_;
    if($self->{OUTARG} =~ m|-o (.+)|) {
        return $1;
    }
    return "a.out";
    die "I do not know what is the link output file\n";
}

sub version {
    my($self) = @_;
    my $cversion = "";
    open(VER, "gcc -dumpversion " . join(' ', @{$self->{PPARGS}}) ." |") 
        || die "Cannot start GCC";
    while(<VER>) {
        if($_ =~ m|^(\d+\S+)|) {
            $cversion = "gcc_$1";
            close(VER) || die "Cannot start GCC\n";
            return $cversion;
        }
    }
    die "Cannot find GCC version\n";
}

1;


__END__



