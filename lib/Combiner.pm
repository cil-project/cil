package Combiner;
use strict;

use FindBin;
use Data::Dumper;

use lib "$FindBin::Bin"; # The libraries are in the same dirctory

use CompilerStub;

BEGIN {
    @Combiner::ISA = qw(CompilerStub);
    $Combiner::combext = "_comb.c";
    $Combiner::combbase = $FindBin::Bin . "/../obj/combiner";
    # Pick the most recent combiner
    $Combiner::mtime_asm = int((stat("$Combiner::combbase.asm.exe"))[9]);
    $Combiner::mtime_byte = int((stat("$Combiner::combbase.byte.exe"))[9]);
    $Combiner::combiner = 
        $Combiner::combbase . 
            ($Combiner::mtime_asm > $Combiner::mtime_byte 
             ? ".asm.exe" : ".byte.exe");
#    print "asm: $Combiner::mtime_asm, byte: $Combiner::mtime_byte. Choose: $Combiner::combiner\n";
}

sub usage {
    print "Usage: combiner [options] [gcc_or_mscl_options]\n";
}

# Customize the compilation
sub compile {
    my($self, $src, $dest, $ppargs, $ccargs) = @_;
    if($self->{VERBOSE}) { print "Combiner is compiling $src into $dest\n"; }
    my $mtime;
    my $res;
    my $outfile;
    if($self->{COMBINE_INOBJ}) {
        $outfile = $dest;
        $mtime = 0;
        $res   = $dest;
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
        $outfile = $dest . $Combiner::combext;
    }
    open(OUT, ">$outfile") || die "Cannot create $outfile";
    my $toprintsrc = $src; $toprintsrc =~ s|\\|/|g;
    print OUT "#pragma combiner($mtime, \"$toprintsrc\", \"" . 
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
    if($self->{VERBOSE}) { print "Combiner is linking into $dest\n"; }
    # Now collect the files to be combined
    my $src;
    my @sources = ref($psrcs) ? @{$psrcs} : ($psrcs);
    my @tocombine = ();
    my @othersources = ();
    foreach $src (@sources) {
        my ($combsrc, $mtime);
        if($self->{COMBINE_INOBJ}) {
            $combsrc = $src;
            $mtime = 0;
        } else {
            $combsrc = $src . $Combiner::combext;
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
        if($fstline =~ m|\#pragma combiner\((\d+)|) {
            if($1 == $mtime) { # It is ours
                push @tocombine, $combsrc; next;
            }
        }
        push @othersources, $src;
    }
    # Now invoke the combiner
    my $combined = $dest . $Combiner::combext;
    my $cmd = "$Combiner::combiner --o $combined ";
    if($self->{MODENAME} eq "MSVC") {
        $cmd .= " --MSVC ";
    }
    if($self->{VERBOSE}) {
        $cmd .= " --verbose ";
    }
    $cmd .= join(' ', @tocombine);
    $self->runShell($cmd);

    $self->linkAfterCombine($psrcs, $dest, $combined, \@othersources, 
                            $ppargs, $ccargs, $ldargs);
}

# A function that is invoked after each linking
sub linkAfterCombine {
    my($self, $psrcs, $dest, $combined, $pothers, 
       $ppargs, $ccargs, $ldargs) = @_;
    print "Left the combined sources in $combined\n";
    # Do the actual linking. Ignore the combined file
    $self->CompilerStub::link($psrcs, $dest, $ppargs, 
                              $ccargs, $ldargs);
}

1;




