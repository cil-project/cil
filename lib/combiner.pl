#!/usr/bin/perl
#
# A script that impersonates the compiler and it collects all the sources into
# one file 
#
use strict;
use FindBin;
use Data::Dumper;

use lib "$FindBin::Bin"; # The libraries are in the same dirctory

use Combiner;

my $comb = Combiner->new(@ARGV);

# print Dumper($comb);

$comb->doit(); # Will combine and then print a message where the combined file
               # is 

1;
