#
#
# Copyright (c) 2001 by
#  George C. Necula	necula@cs.berkeley.edu
#  Scott McPeak        smcpeak@cs.berkeley.edu
#  Wes Weimer          weimer@cs.berkeley.edu
#   
# All rights reserved.  Permission to use, copy, modify and distribute
# this software for research purposes only is hereby granted, 
# provided that the following conditions are met: 
# 1. Redistributions of source code must retain the above copyright notice, 
# this list of conditions and the following disclaimer. 
# 2. Redistributions in binary form must reproduce the above copyright notice, 
# this list of conditions and the following disclaimer in the documentation 
# and/or other materials provided with the distribution. 
# 3. The name of the authors may not be used to endorse or promote products 
# derived from  this software without specific prior written permission. 
#
# DISCLAIMER:
# THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR 
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#


package Merger;
use strict;

use FindBin;
use Data::Dumper;
use File::Basename;

use lib "$FindBin::Bin"; # The libraries are in the same directory

use CompilerStub;

BEGIN {
    $::iswin32 = $^O eq 'MSWin32' || $^O eq 'cygwin';
    @Merger::ISA = qw(CompilerStub);
    $Merger::combext = "_comb.c";
    $Merger::combbase = $FindBin::Bin . "/../obj";
    if($::iswin32) {
        $Merger::combbase .= '/x86_WIN32';
    } else {
        $Merger::combbase .= '/x86_LINUX';
    }
    $Merger::combbase .= "/merger";
    # Pick the most recent merger
    $Merger::mtime_asm = int((stat("$Merger::combbase.asm.exe"))[9]);
    $Merger::mtime_byte = int((stat("$Merger::combbase.byte.exe"))[9]);
    $Merger::merger = 
        $Merger::combbase . 
            ($Merger::mtime_asm >= $Merger::mtime_byte 
             ? ".asm.exe" : ".byte.exe");
    # For some strange reason on Windows the bytecode versions must be passed
    # as an argument to themselves
#    if($::iswin32 && ($Merger::mtime_asm < $Merger::mtime_byte)) {
#        $Merger::merger .= " " . $Merger::merger;
#    }
#    print "asm: $Merger::mtime_asm, byte: $Merger::mtime_byte. Choose: $Merger::merger\n";
}


# We need to customize the collection of arguments
sub collectOneArgument {
    my($self, $arg, $pargs) = @_;
    if($arg =~ m|--merge|)  {
        $self->{MERGE} = 1; return 1;
    }
    if($arg =~ m|--trueobj|) {
        $self->{TRUEOBJ} = 1; return 1;
    }
    if($arg eq "--bytecode") {
        $self->{NATIVECAML} = 0; return 1;
    }
    if($arg eq "--no-idashi") {
        $self->{IDASHI} = 0; return 1;
    }
    if($arg eq "--no-idashdot") {
        $self->{IDASHDOT} = 0; return 1;
    }
    if($arg =~ m|--leavealone=(.+)$|)  {
        push @{$self->{LEAVEALONE}}, $1; return 1;
    }
    if($arg =~ m|--includedir=(.+)$|)  {
        push @{$self->{INCLUDEDIR}}, $1; return 1;
    }
    if($arg =~ m|--usecabs|) {
        $self->{USECABS} = 1; return 1;
    }
    if($arg =~ m|--stages|) {
        $self->{SHOWSTAGES} = 1; 
        push @{$self->{CILARGS}}, $arg;
        return 1;
    }
    # See if the super class understands this
    if($self->SUPER::collectOneArgument($arg, $pargs)) { return 1; }
    # All other arguments starting with -- are passed to CIL
    if($arg =~ m|^--|) {
        # Split the ==
        if($arg =~ m|^(--\S+)=(.+)$|) {
            push @{$self->{CILARGS}}, $1, $2; return 1;
        } else {
            push @{$self->{CILARGS}}, $arg; return 1;
        }
    }

    return 0;
}

sub usage {
    print "Usage: merger [options] [gcc_or_mscl_options]\n";
}



sub helpMessage {
    my($self) = @_;
    # Print first the original
    $self->SUPER::helpMessage();
    print <<EOF;

Merger specific options:
  --merge            Use in merge mode. Applies the cure after the 
                     source files (except the nocure ones) have been
                     merge
  --trueobj          Do not write preprocessed sources in .obj files but
                     create some other files.
 
  --leavealone=xxx   Leave alone files whose base name is xxx. This means
                     they are not merged and not processed with CIL.
  --includedir=xxx   Adds a new include directory to replace existing ones
  --usecabs          Emit and use the CABS
  --stages           Show the processing stages
  --bytecode         Invoke the bytecode (as opposed to native code) system

EOF
}


# Customize the preprocessor
sub preprocess {
    my($self, $src, $dest, $ppargs) = @_;

    if($self->leaveAlone($src)) {
        # We leave this alone. So just preprocess as usual
        return $self->SUPER::preprocess($src, $dest, $ppargs);
    }
    # Just preprocess like before CIL
    return $self->preprocess_beforecil($src, $dest, $ppargs);
}

sub preprocess_beforecil {
    my ($self, $src, $dest, $ppargs) = @_;
    my @args = @{$ppargs};

    # See if we must force some includes
    if(defined $self->{INCLUDEDIR}) {
        # And force the other includes. Put them at the begining
        if(($self->{MODENAME} eq 'GNUCC') &&
           # sm: m88k doesn't work if I pass -I.
           $self->{IDASHDOT}) {
            unshift @args, "-I.";
        }
        unshift @args,
            map { my $dir = $_;
                  $self->{INCARG} . $dir . "/" . $self->{VERSION} }
            @{$self->{INCLUDEDIR}};
        if($self->{MODENAME} eq 'GNUCC') {
            # sm: this is incompatible with wu-ftpd, but is apparently needed
            # for apache.. more investigation is needed
            # update: now when I try it, apache works without -I- also.. but
            # I'll make this into a switchable flag anyway
            if ($self->{IDASHI}) {
                unshift @args, "-I-";
            }
        }
    }

    return $self->SUPER::preprocess($src, $dest, \@args);
}


# Customize the compilation
sub compile {
    my($self, $src, $dest, $ppargs, $ccargs) = @_;
    if($self->{VERBOSE}) { print "Merger is compiling $src into $dest\n"; }
    if($self->leaveAlone($src)) {
        # We leave this alone. So just compile as usual
        return $self->SUPER::compile($src, $dest, $ppargs, $ccargs);
    }
    if(! $self->{MERGE}) {
        # Now invoke CIL and compile after wards
        return $self->applyCilAndCompile($src, $dest, $ppargs, $ccargs); 
    }
    # We are merging
    # If we are merging then we just save the preprocessed source
    my ($mtime, $res, $outfile);
    if(! $self->{TRUEOBJ}) {
        $outfile = $dest; $mtime = 0; $res   = $dest;
    } else {
        # Do the real compilation
        $res = $self->SUPER::compile($src, $dest, $ppargs, $ccargs);
        # Now stat the result 
        my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
            $atime,$mtime_1,$ctime,$blksize,$blocks) = stat($dest);
        if(! defined($mtime_1)) {
            die "Cannot stat the result of compilation $dest";
        }
        $mtime = $mtime_1;
        $outfile = $dest . $Merger::combext;
    }
    open(OUT, ">$outfile") || die "Cannot create $outfile";
    my $toprintsrc = $src; $toprintsrc =~ s|\\|/|g;
    print OUT "#pragma merger($mtime, \"$toprintsrc\", \"" . 
        join(' ', @{$ccargs}), "\")\n";
    open(IN, "<$src") || die "Cannot read $src";
    print OUT <IN>; 
    close(OUT);
    close(IN);
    return $res;
} 


# Customize the linking
sub link {
    my($self, $psrcs, $dest, $ppargs, $ccargs, $ldargs) = @_;
    if($self->{VERBOSE}) { print "Merger is linking into $dest\n"; }
    if(! $self->{MERGE}) {
        # Not merging. Regular linking.
        return $self->link_aftercil($psrcs, $dest, $ppargs, $ccargs, $ldargs);
    }
    # We are merging

    # Now collect the files to be merged
    my $src;
    my @sources = ref($psrcs) ? @{$psrcs} : ($psrcs);
    # Go through the sources and replace all libraries with the files that
    # they contain
    my @sources1 = ();
    while($#sources >= 0) {
        my $src = shift @sources;
#        print "Looking at $src\n";
        # See if the source is a library. Then maybe we should get instead the 
        # list of files
        if($src =~ m|\.$self->{LIBEXT}$|) {
            if(-f "$src.files") {
                open(FILES, "<$src.files") || die "Cannot read $src.files";
                while(<FILES>) {
                    # Put them back in the sources to process them recursively
                    if($_ =~ m|\n$|) {
                        chop;
                    }
                    unshift @sources, $_;
                }
                close(FILES);
                next;
            }
        }
        push @sources1, $src;
        next;
    }
    @sources = @sources1;
#    print "Sources are @sources\n";
    my @tomerge = ();
    my @othersources = ();
    foreach $src (@sources) {
        my ($combsrc, $mtime);
        if(! $self->{TRUEOBJ}) {
            $combsrc = $src;
            $mtime = 0;
            } else {
                $combsrc = $src . $Merger::combext;
                if(-f $combsrc) { 
                    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
                        $atime,$mtime_1,$ctime,$blksize,$blocks) = stat($src);
                    $mtime = $mtime_1;
                } else {
                    $mtime = 0;
                }
            }
        # Look inside and see if it is one of the files created by us
        open(IN, "<$combsrc") || die "Cannot read $combsrc";
        my $fstline = <IN>;
        if($fstline =~ m|\#pragma merger\((\d+)|) {
            if($1 == $mtime) { # It is ours
                push @tomerge, $combsrc; 
                next;
            }
        }
        push @othersources, $src;
    }
    # Now invoke the merger
    my $merged = $dest . $Merger::combext;
    my $cmd = "$Merger::merger --o $merged ";
    if($self->{MODENAME} eq "MSVC") {
        $cmd .= " --MSVC ";
    }
    if($self->{VERBOSE}) {
        $cmd .= " --verbose ";
    }
    # If the number of files to merge is larger than 25 then put them into a
    # file 
    if(@tomerge > 20) {
        my $extraFile = "__merger_extra_files";
        open(TOMERGE, ">$extraFile") || die $!;
        foreach my $fl (@tomerge) {
            print TOMERGE "$fl\n";
        }
        close(TOMERGE);
        $cmd .= " --extrafiles $extraFile";
    } else {
        $cmd .= join(' ', @tomerge);
    }
    $self->runShell($cmd);
    

    my $mergedobj = $self->compileOutputFile($merged);
    $self->applyCilAndCompile($merged, $mergedobj, $ppargs, $ccargs); 
    push @othersources, $mergedobj;

    # And finally link
    $self->link_aftercil(\@othersources, $dest, $ppargs, $ccargs, $ldargs);

}

# Customize the linking into libraries
sub linktolib {
    my($self, $psrcs, $dest, $ppargs, $ccargs, $ldargs) = @_;
    if($self->{VERBOSE}) { print "Merger is linking into library $dest\n"; }
    if(! $self->{MERGE}) {
        # Not merging. Regular linking.
        return $self->linktolib($psrcs, $dest, $ppargs, $ccargs, $ldargs);
    }
    # We are merging

    # Now collect the files to be merged
    my @sources = ref($psrcs) ? @{$psrcs} : ($psrcs);

    # Write the names of the files into a file with the extension files
    open(FILES, ">$dest.files") || die("Cannot open $dest.files");
    print FILES join("\n", @sources);
    if($self->{VERBOSE}) {
        print "Saved to $dest.files the list of names: ",
        join(" ", @sources);
    }
    close(FILES);

    # Now link as usual, without calling CIL
    return $self->SUPER::linktolib($psrcs, $dest, $ppargs, $ccargs, $ldargs);
}


sub applyCilAndCompile {
    my ($self, $ppsrc, $dest, $ppargs, $ccargs) = @_;

    my $aftercil = $self->applyCil($ppsrc, $ppargs);

    # Now preprocess
    my $aftercilpp = $self->preprocessOutputFile($aftercil);
    $self->preprocess_aftercil($aftercil, $aftercilpp, $ppargs);

    # Now compile
    return $self->compile_cil($aftercilpp, $dest, $ppargs, $ccargs);
}

sub preprocess_aftercil {
    my ($self, $src, $dest, $ppargs) = @_;
    return $self->SUPER::preprocess($src, $dest, $ppargs);
}

sub compile_cil {
    my ($self, $src, $dest, $ppargs, $ccargs) = @_;
    return $self->SUPER::compile($src, $dest, $ppargs, $ccargs);
}

# Linking after CIL
sub link_aftercil {
    my ($self, $psrcs, $dest, $ppargs, $ccargs, $ldargs) = @_;
    return $self->SUPER::link($psrcs, $dest, $ppargs, $ccargs, $ldargs);
}

# See if we must merge this one
sub leaveAlone {
    my($self, $filename) = @_;
    my ($base, $dir, $ext) = fileparse($filename, "(\\.[^.]+)");
    if(grep { $_ eq $base } @{$self->{LEAVEALONE}}) {
        return 1;
    } else {
        return 0;
    }
}


1;




