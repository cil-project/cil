#!/usr/bin/perl
#
# A script that impersonates the compiler and it collects all the sources into
# one file 
#
use strict;
use FindBin;
use Data::Dumper;

use lib "$FindBin::Bin"; # The libraries are in the same dirctory

use CompilerStub;

my $comb = Combiner->new(@ARGV);

# print Dumper($comb);

$comb->doit();


# Define here your favorite compiler by overriding CompilerStub methods
package Combiner;
use strict;
BEGIN {
    @Combiner::ISA = qw(CompilerStub);
    $Combiner::combext = "_comb.c";
    $Combiner::combiner = $FindBin::Bin . "/../obj/combiner.asm.exe";
}


# Customize the compilation
sub compile {
    my($self, $src, $dest, @args) = @_;
    print "Combiner is compiling $src into $dest\n";
    # Do the real compilation
    my $res = $self->SUPER::compile($src, $dest, @args);
    # Now stat the result 
    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
        $atime,$mtime,$ctime,$blksize,$blocks) = stat($dest);
    if(! defined($mtime)) {
        die "Cannot stat the result of compilation $dest";
    }
    my $outfile = $dest . $Combiner::combext;
    open(OUT, ">$outfile") || die "Cannot create $outfile";
    my $toprintsrc = $src; $toprintsrc =~ s|\\|/|g;
    print OUT "#pragma combiner($mtime, \"$toprintsrc\", \"" . 
        join(' ', @args), "\")\n";
    open(IN, "<$src") || die "Cannot read $src";
    print OUT <IN>; 
    close(OUT);
    close(IN);
    return $res;
}

# Customize the linking
sub link {
    my($self, $psrcs, $dest, @args) = @_;
    print "Combiner is linking into $dest\n";
    # Do the linking
    my $res = $self->SUPER::link($psrcs, $dest, @args);
    # Now collect the files to be combined
    my $src;
    my @sources = ref($psrcs) ? @{$psrcs} : ($psrcs);
    my @tocombine = ();
    my @othersources = ();
    foreach $src (@sources) {
        my $combsrc = $src . $Combiner::combext;
        if(-f $combsrc) { 
            my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
                $atime,$mtime,$ctime,$blksize,$blocks) = stat($src);
            # Look inside and see if it is one of the files created by us
            open(IN, "<$combsrc") || die "Cannot read $combsrc";
            my $fstline = <IN>;
            if($fstline =~ m|\#pragma combiner\((\d+)|) {
                if($1 == $mtime) { # It is ours
                    push @tocombine, $combsrc; next;
                }
            }
        }
        push @othersources, $src;
    }
    # Now invoke the combiner
    my $cmd = "$Combiner::combiner -o $dest" . $Combiner::combext . 
        join(' ', @tocombine);
    $self->runShell($cmd);

    print "Created the combined file $dest$Combiner::combext\n";
    # Now we have combined things. Call a linker
}






