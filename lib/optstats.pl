#!/usr/bin/perl -w

use strict;
use Getopt::Long;

# A perl script to report statistics about performance loss
# contributed by individual CHECK type. The base measurement is the
# OPTIM=1 result. The numbers in the output represent the fraction of
# usertime saved by removing all occurances of the CHECK.

$|++; # Don't buffer stdout

# Set a signal handler
my $interrupt = 0;
sub intHandler {
    my $signame = shift;
    print "I got a SIG$signame\n";
    $interrupt = 1;
}

                                # Create an exception handler
$SIG{'INT'} = \&intHandler;

my $matchtime = "user\\s*(\\d+)m([0-9.]+)s";
my $calctime  = "\$1 * 60 + \$2";
if (`sh -c "time echo" 2>&1` !~ m/$matchtime/) {
    $matchtime = "([0-9]+\.[0-9]*)user";
    $calctime = "\$1";
}

if (`sh -c "time echo" 2>&1` !~ m/$matchtime/) {
    die "I can't figure out the right regexp for user time in the output of 'time'";
}
# print "Regexp for user time is: $matchtime\n\n";

my $common = " RELEASE=1 _GNUCC=1 OPTIM=1";

# The cases to run. Make sure all --optimvariant test cases come right after 
# the base case in an uninterrupted sequence. All tests cases can have a
# numeric prefix that is used only for finding the right order to run them
# 
my @allcaseskeys = 
    ('nocure', 'base', 'nochecks', 'noNULL', 'noADVANCE',
     'noADVANCE', 'noUB_NULL', 'noUBOUND', 'noFETCHLEN',
     'noLEANSTACK', 'noBELOWSTACK');
     

my %allcases = 
    ( 'nocure'     => " INFERBOX=none $common",
      'base'       => " EXTRAARGS=--optimvariants INFERBOX=infer $common",
      'nochecks'   => "-optimvariant.nochecks $common",
      'noNULL'     => "-optimvariant.noNULL $common",
      'noBOUNDS'   => "-optimvariant.noBOUNDS $common",
      'noADVANCE'  => "-optimvariant.noADVANCE $common",
      'noUB_NULL'  => "-optimvariant.noUBOUNDNULL $common",
      'noUBOUND'   => "-optimvariant.noUBOUND $common",
      'noFETCHLEN' => "-optimvariant.noFETCHLEN $common",
      'noLEANSTACK' => "-optimvariant.noLEANSTACK $common",
      'noBELOWSTACK' => "-optimvariant.noBELOWSTACK $common",
      );

my @alltests =  
    ( "perimeter",
      "bh",
      "compress",
      "go",
      "health",
      "mst",
      "li",
      "power",
      "tsp",
      );

# -------------------------------------------------------------------

# Command line args
my @tests = ();
my @optcaseskeys = ('nocure', 'base'); # These are always run
&GetOptions('help' => \&printHelp,
            't=s' => \@tests,
            'c=s' => \@optcaseskeys);
if($#tests == -1) {
    @tests = @alltests;
}
my @caseskeys; # Construct cases by as the intersection of allcaseskeys and 
               # optcases
               # but maintain the order from allcaseskeys

if(grep { $_ eq 'all'} @optcaseskeys) {
    @optcaseskeys = @allcaseskeys;
} else {
    if($#optcaseskeys > 1) { # At least a checks was requested
        # Make sure that nochecks is run
        push @optcaseskeys, 'nochecks';
    }
}

foreach my $case (@allcaseskeys) {
    if(grep {$_ eq $case } @optcaseskeys) {
        push @caseskeys, $case;
     }
}

print "Running cases: ", join(',', @caseskeys), "\n";

open(LOG, ">optstats.log") || die "Cannot open the optstats.log\n";
print "Logging results to optstats.log\n";

sub printHelp {
    print <<EOF;
    optstats [-t test] ... [-t test] [-c case] ... [-c case]

    If no cases are requested then 'nocure' and 'base' will be run.
    If the 'all' case is requested then all cases are run.
EOF
    print "Tests available:\n\t" , join("\n\t", @alltests), "\n";
    print "Cases available:\n\t", join("\n\t",  @allcaseskeys), "\n";
    &printCasesExplanation();
    exit 0;
}

sub printCasesExplanation {
print <<EOF;
 The following cases will be run for each test case:

 nocure:   regular C code
 base:     regular CCured with inference and optimizations
 nochecks: like base but we eliminate all CHECK_ function calls. The remaining
           cost over nocure is that some data is 64-bits now.
 noNULL:   like base but we force the elimination of all CHECK_NULL functions
 noxxx :   like base but we force the elimination of all CHECK_xxx functions

EOF
}

&printCasesExplanation();

my $cmd;
my $res;

my %allresults;

my $torun = ($#tests + 1) * ($#caseskeys + 1);
my $count = 1;
# Now run all the tests
foreach my $tst (@tests) {
    my %results;
    # Run the test cases in order 
    foreach my $case (@caseskeys) {
        my $cmdtail = $allcases{$case};
        $cmd = "make $tst$cmdtail";
        my $msg = "$count/$torun on " . 
                         localtime(time) . " : $tst ($case): $cmd\n";
        print $msg;
        print LOG "\n================================================\n";
        print LOG $msg;
        $count ++;
        $res = `$cmd 2>&1`;
        print LOG $res;
        if ($res !~ m/$matchtime/) { 
            print $res;
            warn "Cannot find the time for $cmd"; 
            next;
        }
        $results{$case} = eval $calctime;
        print "   $results{$case}s\n";
        print LOG "   $results{$case}s\n";
    }
    if(defined $results{'nochecks'}) {
        # Compute the noFATS = base - (nocheck - nocure)
        $results{'noFATS'} = 
            $results{'base'} - &max(0.0, 
                                    $results{'nochecks'} - $results{'nocure'});
        # Compute the noOTHER = nochecks + Sum_xxx (base - noxxx)
        $results{'noOTHER'} = $results{'nochecks'};
        foreach my $case (keys %results) {
            if(grep { $_ eq $case } 
               ('nochecks', 'nocure', 'base', 'noFATS', 'noOTHER')) {
                next;
            }
            $results{'noOTHER'} += &max(0.0, 
                                        $results{'base'} - $results{$case});
        #    print "now noOther = $results{'noOTHER'} (after $case)\n";
        }
    }
    # Now save the results
    $allresults{$tst} = \%results;
}

# Now compute the gains with respect to the base case
my %allbenefits = ();
foreach my $tst (@tests) {
    my $results = $allresults{$tst};
    my $base = $results->{'base'};
    my %benefits;
    foreach my $case (keys %{$results}) {
        if($case eq 'base') { next ;}
        if($results->{$case} > $base) { 
            $benefits{$case} = 0.0;
        } else {
            $benefits{$case} = ($base - $results->{$case}) / $base;
        }
        push @{$allbenefits{$case}}, $benefits{$case};
    }
    &printOneTest($tst, %benefits);
}

# Now compute the mean costs
my %meanbenefits;
foreach my $case (keys %allbenefits) {
    my $nr = 0; my $total = 1.0;
    foreach my $elem (@{$allbenefits{$case}}) {
        $total *= (1 - $elem); $nr ++;
    }
    if($total <= 0 || $nr == 0) { 
       $meanbenefits{$case} = 0; 
   } else {
       $meanbenefits{$case} = 1 - exp(log($total) / $nr);
   }
}

&printOneTest('OVERALL', %meanbenefits);

# now print an explanation

print <<EOF; 

  Explanation: All percentages are savings in running time over the
base version: CCured with inference. We expect that "nocure" has the most
savings and it is what you would get if you just ran the straight C code. The
following elements are as follows:
  
  nochecks: the savings if we remove all CHECK_ function calls
  noTAGS: the savings if we remove all costs except CHECK_ function calls.
         This is always "nocure - nochecks"
  noNULL:   the savings if we remove all calls to CHECK_NULL
  noUBOUND: the savings if we remove all calls to CHECK_NULL
  ...
  noOTHER: the savings if we remove all CHECK_ function calls that are not
           specifically broken down in other categories. This is always
           nochecks - (noNULL + noUBOUND + no... )

EOF


exit 0;


###
### SUBROUTINES
###

sub printOneTest {
    my ($tst, %benefits) = @_;
    # Sort in decreasing order of benefits
    my @sortedcases = sort { $benefits{$b} <=> $benefits{$a} } 
                           (keys %benefits);
    print "$tst :\n";
    foreach my $case (@sortedcases) {
        printf "\t%-20s: %6.2f%%\n", $case, 100.0 * $benefits{$case};
    }
}

sub max {
    my($a, $b) = @_;
    return ($a >= $b) ? $a : $b;
}
