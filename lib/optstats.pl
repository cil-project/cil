#!/usr/bin/perl

# A perl script to report statistics about performance loss
# contributed by individual CHECK type. The base measurement is the
# OPTIM=1 result. The numbers in the output represent the fraction of
# usertime saved by removing all occurances of the CHECK.

# A list of CHECKs to be removed individually and tested
@checks = (
#  	   "cil",         # The pure unboxed cil version
#  	   "1",           # All CHECKs removed
#  	   "CHECK_UBOUND",
#  	   "CHECK_UBOUND_OR_NULL",
#  	   "CHECK_LBOUND",
#  	   "CHECK_BOUNDS",
#  	   "CHECK_BOUNDS_TRUE",
	   "CHECK_BOUNDS_LEN",
	   "CHECK_BOUNDS_INDEX",
	   "CHECK_POSITIVE",
	   "CHECK_NULL",
	   "CHECK_STRINGMAX",
	   "CHECK_STRINGLEN",
	   "CHECK_NRTAGWORDS",
	   "CHECK_FETCHTAGADDR",
	   "CHECK_NRTAGBITS",
	   "CHECK_ZEROTAGS",
	   "CHECK_COPYTAGSFORW",
	   "CHECK_COPYTAGSBACK",
	   "CHECK_FATPOINTERREAD",
	   "CHECK_FATPOINTERWRITE",
	   "CHECK_REGISTERAREA",
	   "CHECK_UNREGISTERFRAME",
	   "CHECK_FINDHOME",
	   "CHECK_FINDHOMEEND",	   
	   "CHECK_GETFRAME",
	   "CHECK_NOTBELOWSTACK",
	   "CHECK_NOTBELOWSTACKFAT",
	   "CHECK_LEANSTACKPOINTER",
	   "CHECK_FATSTACKPOINTER",
	   "CHECK_SAFERETFAT",
	   "CHECK_FETCHLENGTH",
	   "CHECK_FUNCTIONPOINTER",
	   "CHECK_FETCHEND",
	   "CHECK_FETCHSTRINGEND",
	   ""
	   );

@tests  = (
	   "hola",     # Just a sanity check
	   "perimeter",
	   "bh",
	   "compress",
	   "go",
	   "health",
	   "li",
	   "power",
	   "tsp",
	   ""
	   );

$makecmd = "INFERBOX=4 OPTIM=1 RELEASE=1";
$makecil = "RELEASE=1";

$matchtime = "([0-9]+\.[0-9]*)user";
$calctime = "\$1";

#$matchtime = "user\\s*(\\d+)m([0-9.]+)s";
#$calctime  = "\$1 * 60 + \$2";

$iterations = 2;


# -------------------------------------------------------------------


$|++; # Don't buffer stdout

print "optstats.pl: makecmd=$makecmd, $#tests tests, $#checks checks, $iterations iterations\n\n";

print "      \t";
for ($j=0; $j<$#tests; $j++) {
    print "$tests[$j]\t";
}
print "\n";

for ($i=0; $i<$#checks; $i++) {
    print "$checks[$i]\t";
    for ($j=0; $j<$#tests; $j++) {
	$diff_frac_sum = 0;	
	for ($iter=0; $iter<$iterations; $iter++) {
	    if (`make $tests[$j] $makecmd 2>&1` =~ m/$matchtime/) {
		$original_time=eval ($calctime);
		if ($checks[$i] eq "cil") {
		    $op=`make $tests[$j] $makecil 2>&1`;
		} else {
		    $op=`make $tests[$j] $makecmd REDREMOVEALL=$checks[$i] 2>&1`;
		}
		if ($op =~ m/$matchtime/) {
		    $stripped_time=eval ($calctime);
		    $diff_time=$original_time-$stripped_time;

		    if ($original_time < 0.001) {
			print "*";
			$diff_frac=$diff_time;
		    } else {
			$diff_frac=$diff_time / $original_time;
		    }
		    $diff_frac_sum += $diff_frac;
		}
		else {
		    print "X";
		}
	    }
	    else {
		print "x";
	    }
	}
	$diff_frac_sum /= $iterations;
	if ($diff_frac_sum < 0.001) {
	    print "nglgbl\t";
	} else {
	    printf "%.3f\t", $diff_frac_sum;
	}
    }
    print "\n";
}
