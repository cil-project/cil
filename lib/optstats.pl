#!/usr/bin/perl -w

# A perl script to report statistics about performance loss
# contributed by individual CHECK type. The base measurement is the
# OPTIM=1 result. The numbers in the output represent the fraction of
# usertime saved by removing all occurances of the CHECK.

$|++; # Don't buffer stdout

my %tests;

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

# The cases to run. Make sure all
# REDREMOVEALL cases must be run after infer.
my %cases = 
    ( '00nocure'     => " INFERBOX=none $common",
      # The base case must come before all REDREMOVEALL
      '01base'       => " EXTRAARGS=--optimvariants INFERBOX=infer $common",
      '10nochecks'   => "-removednochecks $common",
      '11noNULL'     => "-removednoNULL $common",
      '12noBOUNDS'  => "-removednoBOUNDS $common",
      '13noADVANCE' => "-removednoADVANCE $common",
      '14noUB_NULL' => "-removednoUBOUNDNULL $common",
      '15noUBOUND'  => "-removednoUBOUND $common",
      );

# -------------------------------------------------------------------

# Command line args
while (@ARGV) {
    $arg = shift (@ARGV);
    if ($arg =~ /^-t/) {
	@tests = (split (/[\s,;]/, shift (@ARGV)),"");
    }
    elsif ($arg =~ /-[h?]$/) {
	print "Syntax: optstats.pl [-t tests] [-c checks]\n";
	exit 0;
    }
}

if($#tests == -1) {
    @tests =  (
#	   "hola",     # Just a sanity check
#           "perimeter",
#           "bh",
#           "compress",
#           "go",
#           "health",
#           "mst",
#           "li",
  	   "power",
#  	   "tsp",
#	   ""
               );
}


# -------------------------------------------------------------------


# -------------------------------------------------------------------

my $cmd;
my $res;

my %allresults;

# Now run all the tests
foreach my $tst (@tests) {
    my %results;
    # Run the test cases in order 
    foreach my $case (sort (keys %cases)) {
        $cmd = "make $tst$cases{$case}";
        print "Running $cmd\n";
        $res = `$cmd 2>&1`;
        if ($res !~ m/$matchtime/) { 
            print $res;
            die "Cannot find the time for $cmd"; 
        }
        $results{$case} = eval $calctime;
        print "   $results{$case}\n";
        print $res;
    }

    # Now save the results
    $allresults{$tst} = \%results;
}


# Now print a header
my $testnameformat = "%-20s";
printf $testnameformat, "TEST";
foreach my $case (sort (keys %cases)) {
    printf "%8s", $case;
}

print "====================================================================\n";

# Now print the results for each test

