#!/usr/bin/perl -w

# A perl script to report statistics about performance loss
# contributed by individual CHECK type. The base measurement is the
# OPTIM=1 result. The numbers in the output represent the fraction of
# usertime saved by removing all occurances of the CHECK.

# A list of CHECKs to be removed individually and tested
@checks = (
  	   "cil",         # The pure unboxed cil version
  	   "1",           # All CHECKs removed
  	   "CHECK_UBOUND",
  	   "CHECK_UBOUND_OR_NULL",
  	   "CHECK_LBOUND",
  	   "CHECK_BOUNDS",
  	   "CHECK_BOUNDS_TRUE",
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

%tests  = (
#	   "hola",     # Just a sanity check
	   "perimeter",
  	   "bh",
  	   "compress",
  	   "go",
  	   "health",
           "mst",
  	   "li",
  	   "power",
  	   "tsp",
	   ""
	   );

$makecmd = "INFERBOX=4 OPTIM=1 RELEASE=1 _GNUCC=1";
$makecil = "RELEASE=1 _GNUCC=1";

$matchtime = "user\\s*(\\d+)m([0-9.]+)s";
$calctime  = "\$1 * 60 + \$2";
if (`sh -c "time echo" 2>&1` !~ m/$matchtime/) {
    $matchtime = "([0-9]+\.[0-9]*)user";
    $calctime = "\$1";
}

if (`sh -c "time echo" 2>&1` !~ m/$matchtime/) {
    die "I can't figure out the right regexp for user time in the output of 'time'";
}

$infinity   = 1000000;
$iterations = 2;
$maxtime    = $infinity;

# -------------------------------------------------------------------

# Command line args
while (@ARGV) {
    $arg = shift (@ARGV);
    if ($arg =~ /^-t/) {
	@tests = (split (/[\s,;]/, shift (@ARGV)),"");
    }
    if ($arg =~ /^-c/) {
	@checks = (split (/[\s,;]/, shift (@ARGV)),"");
    }
    elsif ($arg =~ /^-i$/) {
	$iterations = shift (@ARGV);
	$maxtime = $infinity;
    }
    elsif ($arg =~ /^-ii/) {
	$maxtime = shift (@ARGV); 
	$iterations = $infinity;
    }
    elsif ($arg =~ /-[h?]$/) {
	print "Syntax: optstats.pl [-t tests] [-c checks] [-i max-iterations | -ii max-time]\n";
	exit 0;
    }
}


# -------------------------------------------------------------------

$|++; # Don't buffer stdout
print "optstats.pl: makecmd=$makecmd, $#tests tests, $#checks checks,";
if ($iterations >= $infinity) {
    print "max time=$maxtime sec\n";
} elsif ($maxtime >= $infinity) {
    print "$iterations iterations\n";
} else {die "Either iterations or maxtime must be infinity";}

print "Regexp for user time is: $matchtime\n\n";

# -------------------------------------------------------------------


$max_rowname_length=0;
for ($i=0; $i<$#checks; $i++) {
    $max_rowname_length = max ($max_rowname_length, length($checks[$i]));
}

print_pad_rowhead (" ");

# Squeeze the column names to fit in (tab-size - 1) width
$max_header_wordlength = 7;
for ($j=0; $j<$#tests; $j++) {
    $shortname = $tests[$j];
    while (length($shortname) > $max_header_wordlength) {
	if ($shortname =~ /(.*)[aeiou]([^aeiou]*)/) {
	    $shortname = "$1$2";
	} else {
	    $shortname = substr($shortname,0,$max_header_wordlength)
	}
    }
    print "$shortname\t";
}

print "\n";

for ($i=0; $i<$#checks; $i++) {
    if ($checks[$i] eq "1"){
	print_pad_rowhead ("all");
    } else {
	print_pad_rowhead ("$checks[$i]");
    }
    for ($j=0; $j<$#tests; $j++) {
	$diff_frac_sum = 0;	
	$abort_time = time + $maxtime;
	for ($iter=0; ($iter<$iterations) && (time < $abort_time); $iter++) {
            my $res = `make $tests[$j] $makecmd 2>&1`;
	    if ($res =~ m/$matchtime/) {
		$original_time=eval ($calctime);
		if ($original_time < 0.001) {
		    print "*";
		    last;
		}
		if (time >= $abort_time) {last;}
		if ($checks[$i] eq "cil") {
		    $op=`make $tests[$j] $makecil 2>&1`;
		} else {
		    $op=`make $tests[$j] $makecmd REDREMOVEALL=$checks[$i] 2>&1`;
		}
		if ($op =~ m/$matchtime/) {
		    $stripped_time=eval ($calctime);
		    $diff_time=$original_time-$stripped_time;
		    $diff_frac=$diff_time / $original_time;
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

	if ($iter <= 0) {
	    print "?\t";
	}
	else {
	    $diff_frac_sum /= $iter;
	    if ($diff_frac_sum < 0.001) {
		print "-\t";
	    } else {
		printf "%.3f\t", $diff_frac_sum;
	    }
	}
    }
    print "\n";
}

# ----------------------------------------------------------------------
sub max {
    my ($a,$b) = @_;
    if ($a > $b) {return $a} else {return $b};
}

sub print_pad_rowhead {
    my ($s) = $_[0];
    my ($len) = length ($s);
    print $s;
    while ($len<$max_rowname_length) {
	print " ";
	$len++;
    }
    print "\t";
}
