# This module implements a compiler stub that parses the command line
# arguments of gcc and Microsoft Visual C (along with some arguments for the
# script itself) and gives hooks into preprocessing, compil;ation and linking. 
package CompilerStub;
@ISA = ();

use strict;
use File::Basename;
use File::Copy;
use FindBin;
use Data::Dumper;
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
      DEBUG => 0,
      VERBOSE => 0,
      OPERATION => 'TOEXE', # This is the default for all compilers
    };
    my $self = bless $ref, $class;

    # Expect the --mode argument first. Else it is GCC
    {
        my $mode;
        if($args[0] =~ m|--mode=(.+)$|) {
            shift @args; $mode = $1;
        } else { $mode = 'GNUCC'; }
        if(defined $self->{MODENAME} && $self->{MODENAME} ne $mode) {
            die "Cannot re-specify the compiler";
        }
        my $compiler;
        if($mode eq "MSVC") {
            unshift @CompilerStub::ISA, qw(MSVC);
            $compiler = MSVC->new($self);
        } elsif($mode eq "GNUCC") {
            unshift @CompilerStub::ISA, qw(GNUCC);
            $compiler = GNUCC->new($self);
        } elsif($mode eq "MSLINK") {
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
    # Scan and process the arguments
    while($#args >= 0) {
        my $arg = shift @args; # Grab the next one
        if(! $self->collectOneArgument($arg, \@args)) {
            print "Warning: Unknown argument $arg\n";
            push @{$self->{CCARGS}}, $arg;
        }
    }
#    print Dumper($self);
    return $self;
}



# Collecting arguments. Take a look at one argument. If we understand it then
# we return 1. Otherwise we return 0. Might pop soem more arguments from pargs.
sub collectOneArgument {
    my($self, $arg, $pargs) = @_;
    my $res;
    # Maybe it is a compiler option or a source file
    if($self->compilerArgument($self->{OPTIONS}, $arg, $pargs)) { return 1; }

    if($arg eq "--help") {
        $self->printHelp(); exit 1;
    }
    if($arg eq "--debug") {
        $self->{DEBUG} = 1; return 1;
    }
    if($arg eq "--verbose") {
        $self->{VERBOSE} = 1; return 1;
    }
    if($arg =~ m|--keep=(.+)$|) {
        $self->{KEEPDIR} = $1; 
        if(! -d $self->{KEEPDIR}) {
            die "Cannot find directory $self->{KEEPDIR}";
        }
        return 1;
    }
    return 0;
}



sub printHelp {
    my($self) = @_;
    $self->usage();
    print <<EOF;
Options:
  --mode=xxx   What tool to emulate: 
                GNUCC   - GNU CC
                MSVC    - MS VC cl compiler
                MSLINK  - MS VC link linker
               This option must be the first one! If it is not found there
               then GNUCC mode is assumed.
  --help       Prints this help message
  --debug      Set a debug flag
  --verbose    Prints a lot of information about what is being done
  --keep=xxx   Keep temporary files in the given directory
EOF
    $self->helpMessage();
}

# For printing the first line of the help message
sub usage {
    my ($self) = @_;
    print "<No usage is defined>";
}

# The rest of the help message
sub helpMessage {
    my ($self) = @_;
    print <<EOF;
Send bugs to necula\@cs.berkeley.edu.
EOF
}


# PREPROCESSING
sub preprocess {
    my ($self, $src, $dest, $ppargs) = @_;
    my $res;
    if($self->{VERBOSE}) { print "Preprocessing $src\n"; }
    if($self->{MODENAME} eq "MSVC") {
        $self->MSVC::msvc_preprocess($src, $dest, $ppargs);
    } else {
        my $cmd = $self->{CPP} . " " . 
            join(' ', @{$ppargs}) .  " -DUSE_GC " .
                "$src " . $self->{OUTCPP} . $dest;
        $res = $self->runShell($cmd);
        
    }
    return $res;
}


# COMPILATION (with PREPROCESSING)
sub compile {
    my ($self, $src, $dest, $ppargs, $ccargs) = @_;
    if($self->{VERBOSE}) { print "Compiling $src\n"; }
    $dest = $dest eq "" ? "" : $self->{OUTOBJ} . $dest;
    my $forcec = $self->{FORCECSOURCE};
    my $cmd = $self->{CC} . " " . join(' ', @{$ppargs}, @{$ccargs}) .  
        " $dest $forcec$src";
    return $self->runShell($cmd);
}

# Find the name of the preprocessed file
sub preprocessOutputFile {
    my($self, $src) = @_;
    my ($base, $dir, $ext) = fileparse($src, "\\.[^.]+");
    my $idir = $dir;
    if(defined $self->{KEEPDIR}) { $idir = $self->{KEEPDIR} . "/"; }
    return "$idir$base.i";
}

# Invoke the cpp and then the compile method.
sub preprocess_compile {
    my ($self, $src, $dest, $ppargs, $ccargs) = @_;
    my ($base, $dir, $ext) = fileparse($src, "\\.[^.]+");
    if($ext eq ".c" || $ext eq ".cpp") {
        my $out    = $self->preprocessOutputFile($src);
        $self->preprocess($src, $out, $ppargs);
        $src = $out;
        $ext = ".i";
    }
    if($ext eq ".i") {
        $self->compile($src, $dest, $ppargs, $ccargs);
    }
}

# LINKING (with COMPILATION and PREPROCESSING)
sub link {
    my ($self, $psrcs, $dest, $ppargs, $ccargs, $ldargs) = @_;
    my @sources = ref($psrcs) ? @{$psrcs} : ($psrcs);
    $dest = $dest eq "" ? "" : $self->{OUTEXE} . $dest;
    # Pass the linkargs last because some libraries must be passed after
    # the sources
    my $cmd = $self->{LD} . " " . 
        join(' ', @{$ppargs}, @{$ccargs}, @sources, @{$ldargs}) .  
            " $dest";
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
        $self->link(\@allfiles, "", $self->{PPARGS}, 
                    $self->{CCARGS}, 
                    $self->{LINKARGS});
        return;
    }
    my @tolink = (@{$self->{OFILES}});

    foreach $file (@{$self->{IFILES}}, @{$self->{CFILES}}) {
        $out = $self->compileOutputFile($file);
        $self->preprocess_compile($file, $out, 
                                  $self->{PPARGS}, $self->{CCARGS});
        push @tolink, $out;
    }

    # See if we must stop after compilation
    if($self->{OPERATION} eq "TOOBJ") {
        return;
    }

    # Now link all of the files
    $out = $self->linkOutputFile();
    $self->link(\@tolink,  $out, 
                $self->{PPARGS}, $self->{CCARGS}, $self->{LINKARGS});
}

sub classDebug {
    if(0) { print @_; }
}

sub compilerArgument {
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
          my $argument_done = 1;
            if(defined $action->{'RUN'}) {
                &{$action->{'RUN'}}($fullarg, $onemore);
                $argument_done = 1;
            }
            if(defined $action->{'TYPE'}) {
                &classDebug("  type=$action->{TYPE}\n");
                if($action->{TYPE} eq "PREPROC") {
                    push @{$self->{PPARGS}}, $fullarg; return 1;
                }
                if($action->{TYPE} eq "CC") {
                    push @{$self->{CCARGS}}, $fullarg; return 1;
                }
                if($action->{TYPE} eq "LINKCC") {
                    push @{$self->{CCARGS}}, $fullarg; 
                    push @{$self->{LINKARGS}}, $fullarg; return 1;
                }
                if($action->{TYPE} eq "LINK") {
                    push @{$self->{LINKARGS}}, $fullarg; return 1;
                }
                if($action->{TYPE} eq "CSOURCE") {
                    push @{$self->{CFILES}}, $fullarg; return 1;
                }
                if($action->{TYPE} eq "OSOURCE") {
                    push @{$self->{OFILES}}, $fullarg; return 1;
                }
                if($action->{TYPE} eq "ISOURCE") {
                    push @{$self->{IFILES}}, $fullarg; return 1;
                }
                if($action->{TYPE} eq 'OUT') {
                    if(defined($self->{OUTARG})) {
                        print "Warning: output file is multiply defined: $self->{OUTARG} and $fullarg\n";
                    }
                    $self->{OUTARG} = $fullarg; return 1;
                }
                print "  Do not understand TYPE\n"; return 1;
            }
            if($argument_done) { return 1; }
            print "Don't know what to do with option $arg\n"; 
            return 0;
        }
    }
    return 0;
}


sub runShell {
    my ($self, $cmd) = @_;

    # sm: I want this printed to stderr instead of stdout
    # because the rest of 'make' output goes there and this
    # way I can capture to a coherent file
    if($self->{VERBOSE}) { print STDERR "$cmd\n"; }

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
package MSVC;

use strict;
use File::Basename;

sub new {
    my ($proto, $stub) = @_;
    my $class = ref($proto) || $proto;
    # Create $self

    my $self = 
    { NAME => 'Microsoft cl compiler',
      MODENAME => 'MSVC',
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
          ["[^/].*\\.(c|cpp|cc)" => { TYPE => 'CSOURCE' },
           "[^/].*\\.i" => { TYPE => 'ISOURCE' },
           "[^/-]" => { TYPE => "OSOURCE" },
           "/O" => { TYPE => "CC" },
           "/G" => { TYPE => "CC" },
           "/[DI]" => { TYPE => "PREPROC"},
           "/EH" => { TYPE => "CC" },
           "/G"  => { TYPE => "CC" },
           "/F[aA]"  => { TYPE => 'CC' },
           "/Fo"     => { TYPE => 'OUT' },
           "/Fe"   => { TYPE => 'OUT',
                        RUN => sub { $stub->{OPERATION} = "TOEXE" }},
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
                         sub { print "Unimplemented MSVC argument $_[0]\n";}},
           ],
      };
    bless $self, $class;
    return $self;
}


sub msvc_preprocess {
    my($self, $src, $dest, $ppargs) = @_;
    my $res;
    my ($sbase, $sdir, $sext) = 
        fileparse($src, 
                  "(\\.c)|(\\.cc)|(\\.cpp)|(\\.i)");
    my $cmd = "cl /nologo /P /D_MSVC " . join(' ', @{$ppargs});
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


    # MSVC does not understand the extension .i, so we tell it it is a C file
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
    return "a.exe";
}

sub setVersion {
    my($self) = @_;
    my $cversion = "";
    open(VER, "cl 2>&1|") || die "Cannot start Microsoft CL\n";
    while(<VER>) {
        if($_ =~ m|Compiler Version (\S+) |) {
            $cversion = "cl_$1";
            close(VER);
            $self->{VERSION} = $cversion;
            return;
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
          ["[^/-]" => { TYPE => 'OSOURCE' },
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
## GNUCC specific code
##
package GNUCC;

use strict;

use File::Basename;

sub new {
    my ($proto, $stub) = @_;
    my $class = ref($proto) || $proto;
    # Create $self

    my $self = 
    { NAME => 'GNU CC',
      MODENAME => 'GNUCC',  # do not change this since it is used in code
      CC => "gcc -D_GNUCC -c ",
      LD => "gcc -D_GNUCC ",
      CPP => "gcc -D_GNUCC -E ",
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
      OUTEXE => "-o ",
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
    if($self->{OUTARG} =~ m|-o\s*(\S+)| && $self->{OPERATION} eq 'TOOBJ') {
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
}

sub setVersion {
    my($self) = @_;
    my $cversion = "";
    open(VER, "gcc -dumpversion " . join(' ', @{$self->{PPARGS}}) ." |") 
        || die "Cannot start GNUCC";
    while(<VER>) {
        if($_ =~ m|^(\d+\S+)| || $_ =~ m|^(egcs-\d+\S+)|) {
            $cversion = "gcc_$1";
            close(VER) || die "Cannot start GNUCC\n";
            $self->{VERSION} = $cversion;
            return;
        }
    }
    die "Cannot find GNUCC version\n";
}

1;


__END__



