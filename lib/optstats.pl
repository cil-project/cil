#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Data::Dumper;

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
sub setInterruptHandler {
    $SIG{'INT'} = \&intHandler;
}

&setInterruptHandler();

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

my $commoncmd = " RELEASE=1 _GNUCC=1 OPTIM=1";

# The cases to run. Make sure all --optimvariant test cases come right after 
# the base case in an uninterrupted sequence. All tests cases can have a
# numeric prefix that is used only for finding the right order to run them
# 
my @allcases = 
    ('original', 'ccured', 'CHECKS', 'NULL', 'ADVANCE',
     'UBOUNDNULL', 'BOUNDS', 'UBOUND', 'LBOUND', 'FETCHLEN', 'STOREPTR', 
     'RETURNPTR', 'STOREFATPTR', 'RETURNFATPTR');
  
my @defaultcases = 
    ('original', 'ccured', 'CHECKS', 'NULL', 'ADVANCE',
     'UBOUNDNULL', 'BOUNDS' );
    

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
      "bc",
      "yacr",
      "anagram",
      "ft",
      "ks",
      "btreetest",
      "rbtest",
      "hufftest",
      "hashtest"
      );

# -------------------------------------------------------------------

# Command line args
my @tests = ();
my @optcases = ('original', 'ccured'); # These are always run
&GetOptions('help' => \&printHelp,
            't=s' => \@tests,
            'c=s' => \@optcases);
if($#tests == -1) {
    @tests = @alltests;
}
my @cases; # Construct cases by as the intersection of allcaseskeys and 
               # optcases
               # but maintain the order from allcaseskeys

if(grep { $_ eq 'all'} @optcases) {
    push @optcases, @defaultcases;
} else {
    if($#optcases > 1) { # At least a check was requested
        # Make sure that nochecks is run
        push @optcases, 'CHECKS';
    }
}

# Check that all checks specified are known
foreach my $case (@optcases) {
    if($case eq 'original' || $case eq 'ccured') { next; }
    if(! grep { $_ eq $case } @allcases) {
        warn "I don't recognize case $case\n";
    }
}

foreach my $case (@allcases) {
    if($case eq 'original' || $case eq 'ccured' ||
       grep {$_ eq $case } @optcases) {
        push @cases, $case;
    }
}  

print "Running cases: ", join(',', @cases), "\n";
  
my $logFile = 'test/optstats.log';
open(LOG, ">$logFile") || die "Cannot open the $logFile\n";
print "Logging results to $logFile\n";

my $resultFile = 'test/optstats.results';
&deleteOrRenameLog($resultFile, 0);
open(RESULTS, ">$resultFile") || die "Cannot create results file $resultFile";

sub printHelp {
    print <<EOF;
    optstats [-t test] ... [-t test] [-c case] ... [-c case]

    If no cases are requested then 'original' and 'ccured' will be run.
    If the 'all' case is requested then all cases are run.

    Press CTRL-C if you want to skip the current test. 

EOF
    print "Tests available:\n\t" , join("\n\t", @alltests), "\n";
    print "Cases available:\n\t", join("\n\t",  grep { $_ ne 'original' &&
                                                       $- ne 'ccured'} @allcases), "\n";
    &printCasesExplanation();
    exit 0;
}

sub printCasesExplanation {
print <<EOF;
 The following cases will be run for each test case:

 original: the original C code
 ccured:   regular CCured with inference and optimizations
 CHECKS:   like ccured but we eliminate all CHECK_ function calls. The remaining
           cost over original is that some data is 64-bits now.
 NULL:     like ccured but we force the elimination of all CHECK_NULL functions
 xxx :     like ccured but we force the elimination of all CHECK_xxx functions

EOF
}

&printCasesExplanation();

my $cmd;
my $res;

my %allresults;

my $torun = ($#tests + 1) * ($#cases + 1);
my $count = 1;
# Now run all the tests
foreach my $tst (@tests) {
    my %results;
    # Run the test cases in order 
    $results{'ccured'} = 0.0;
    foreach my $case (@cases) {
        # Construct the command
        if($case eq 'original') {
            $cmd = "make $tst INFERBOX=none $commoncmd";
        } elsif($case eq 'ccured') { 
            $cmd = "make $tst INFERBOX=infer $commoncmd ";
            # Find out which variants we need
            if($#cases > -1) {
                $cmd .= " EXTRAARGS=\"";
                foreach my $variant (@cases) {
                    if($variant eq 'original' || $variant eq 'ccured') { 
                        next; 
                    }
                    $cmd .= " --optimelimall=$variant ";
                }
                $cmd .= "\"";
            }
        } else {
            # This is a regular variant
            $cmd = "make $tst-optimvariant.no$case $commoncmd";
        }
        my $msg = "$count/$torun on " . 
                         localtime(time) . " : $tst ($case): $cmd\n";
        print LOG "\n================================================\n";
        print         $msg;
        print LOG     $msg;
        print RESULTS $msg;
        $count ++;
        open(RUN, "$cmd 2>&1 |") || { warn "Cannot run $cmd"; next; }
        while(<RUN>) {
            print LOG $_;
            if($_ =~ m/$matchtime/) { 
                $results{$case} = eval $calctime;
                next;
            }
            if($_ =~ m|\s+CHECK_(\S+)\s+(\d+)\s*\(\s*([\d.]+)%\)|) {
                $results{"perc$1"}  = $3;
            }
        }
        close(RUN) || { warn "Cannot run $cmd"; next; }
        &processInterrupt();
        if(! defined($results{$case})) {
            print "Cannot find the time for $cmd\n";
            next;
        } 
        print         "   $results{$case}s\n";
        print LOG     "   $results{$case}s\n";
        print RESULTS "   $results{$case}s\n"; 
    }
    if(defined $results{'CHECKS'}) {
        # Compute the noFATS = ccured - (nocheck - original)
        $results{'FATS'} = 
            $results{'ccured'} - &max(0.0, 
                                    $results{'CHECKS'} - $results{'original'});
        # Compute the noOTHER = nochecks + Sum_xxx (ccured - noxxx)
        $results{'OTHER'} = $results{'CHECKS'};
        foreach my $case (keys %results) {
            if(grep { $_ eq $case } 
               ('CHECKS', 'original', 'ccured', 'FATS', 'OTHER')) {
                next;
            }
            if($case =~ m|^perc|) { next; }
            $results{'OTHER'} += &max(0.0, 
                                      $results{'ccured'} - $results{$case});
        #    print "now noOther = $results{'noOTHER'} (after $case)\n";
        }
        # Compute the percentage of OTHER checks
        my $percother = 0.0;
        foreach my $key (keys %results) {
            if($key =~ m|^perc(.+)|) {
                my $check = $1;
                if(! grep { $_ eq $check } (keys %results)) {
                    $percother += $results{$key};
                }
            }
        }
        $results{'percOTHER'} = $percother;
        $results{'percCHECKS'} = 100.0;
    }
    # Now save the results
    $allresults{$tst} = \%results;
}

# Now compute the gains with respect to the ccured case
my %alldata = ();
foreach my $tst (@tests) {
    my $results = $allresults{$tst};
    my $ccured = $results->{'ccured'};
    if($ccured == 0.0) {
        warn "*** Ccured case for $tst takes 0s\n";
        next;
    }
    my %data;
    # keep in mind that stuff in data will be subject to geometric mean. So
    # keep those values neutral at 1.0 and 0.0 better not appear in there.
    foreach my $case (keys %{$results}) {
        if($case eq 'original') { next ;}
        my $key = $case;
        if($case =~ m|^perc|) {
            # Just copy the percentages
            $data{$case} = $results->{$case};
        } else {
            if($case eq 'ccured') { $key = 'slowdown'; }
            $data{$key} = &ratio($results->{$case},
                                 $results->{'original'});
        }
        push @{$alldata{$key}}, $data{$key};
    }
    &printOneTest($tst, %data);
#    print Dumper("Results=",$results);
#    print Dumper("Data=", \%data);
}

# Now compute the mean costs
my %meandata;
foreach my $dt (keys %alldata) {
    if($dt =~ m|^perc|) {
        # Arithmetic mean
        my $nr = 0; my $total = 0.0;
        foreach my $elem (@{$alldata{$dt}}) {
            $total += $elem; $nr ++;
        }
        if($nr == 0) { 
            $meandata{$dt} = 0.0; 
        } else {
            $meandata{$dt} = $total / $nr;
        }
    } else {
        # Geometric mean for everything else
        my $nr = 0; my $total = 1.0;
        foreach my $elem (@{$alldata{$dt}}) {
            $total *= $elem; $nr ++;
        }
        if($total <= 0 || $nr == 0) { 
            $meandata{$dt} = 1.0; 
        } else {
            $meandata{$dt} = exp(log($total) / $nr);
        }
    }
}

&printOneTest('OVERALL', %meandata);

# now print an explanation

print <<EOF; 

 Explanation:
  Slowdown: percentage slowdown of the cured version over the original. 
            Computed as ccured_time / original_time - 1

  All other percentages try to attribute difference "ccured_time - original_time"
to various factors. 
  
  CHECKS: the portion attributable to calls to CHECK_ functions
          Computed as (ccured_time - nochecks_time) / (ccured_time - original_time)
  TAGS:   the portion non-attributable to CHECK_functons. 
          Computed as (nochecks_time - original_time) / (ccured_time - original_time)

  NULL:  the portion attributable to CHECK_NULL
          Computed as (ccured_time - noNULL_time) / (ccured_time - original_time)
  UBOUND: the savings if we remove all calls to CHECK_UBOUND
  ...
  OTHER: the portion attributable to other CHECK_ functions
          Computed as (ccured_time - (ccured_time - noNULL_time)
                                 - (ccured_time - noUBOUND_time)
                                 - ...) / (ccured_time - original_time)

  Overall: shows geometric means of all the results.
EOF

print "Results were written to $resultFile\n";

exit 0;


###
### SUBROUTINES
###

sub printOneTest {
    my ($tst, %data) = @_;
    # Extract the slowdown
    my $slow = $data{'slowdown'};
    # Sort in increasing order of data
    my @sorteddt = sort { $data{$a} <=> $data{$b} } (keys %data);
    my $msg = sprintf "$tst (Slowdown: %6.2f%%)\n", 100.0 * ($slow - 1);
    print $msg; print LOG $msg; print RESULTS $msg;
    foreach my $dt (@sorteddt) {
        if($dt eq 'slowdown') { next; }
        if($dt =~ m|^perc|) { next; }
        my $msg = sprintf"\t%-20s: %6.2f%% of ccured_time - original_time, %6.2f%% of CHECKs\n", 
             $dt, 100.0 * &ratio($slow - $data{$dt}, $slow - 1),
             (defined($data{"perc$dt"}) ? $data{"perc$dt"} : 0.0);
        print $msg; print LOG $msg; print RESULTS $msg;
    }
    # find out what checks are in OTHER
    my @otherchecks = ();
    foreach my $key (keys %data) {
        if($key =~ m|^perc(.+)$|) {
            my $check = $1;
            if(! grep { $_ eq $check } (keys %data)) {
                push @otherchecks, $check;
            }
        }
    }
    if($#otherchecks > -1) {
        # Sort the otherchecks
        @otherchecks = 
            sort { $data{"perc$b"} <=> $data{"perc$a"} } @otherchecks;
        print "\t\t OTHER checks are : ";
        foreach my $other (@otherchecks) {
            print "$other (", $data{"perc$other"}, "%), ";
        }
        print "\n";
    }
}

sub max {
    my($a, $b) = @_;
    return ($a >= $b) ? $a : $b;
}

sub processInterrupt {
    my($self) = @_;
    if($interrupt) {
        $interrupt = 0;
        print "\n";
        while(1) {
            print "You pressed CTRL-C. Want to continue? (y/n): ";
            my $answer = <STDIN>; chop $answer;
            if($answer eq "") { next; }
            if($answer eq "y" || $answer eq "Y") {
                &setInterruptHandler();
                return;
            }
            last;
        }
        die "I'm outta here\n";
    }
}

# A subroutine that deletes or renames a version of the log file 
# The first version is called <base>.1, ... where <base> is the current log
# file
sub deleteOrRenameLog {
    my($logbase, $version) = @_;
    my $verlogname = $version == 0 ? $logbase : "$logbase.$version";
    if(! -f $verlogname) {
        return;
    }
    if($version >= 5) {
        # delete it 
        unlink $verlogname;
        return;
    }
    # See if we can rename it to a higher version
    my $newversion = $version + 1;
    &deleteOrRenameLog($logbase, $newversion);
    rename $verlogname, "$logbase.$newversion";
}

# Compute a ratio 0 <= $a / $b
# Make sure the ratio is neutral at 1 so that we 
# can use geometric means
sub ratio {
    my($a, $b) = @_;
    if($b <= 0) { return 0.0; }
    return $a / $b;
}

