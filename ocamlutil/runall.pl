#!/bin/perl

use strict;
use File::Basename;

# usage runtests.pl filename
#
# The file is expected to contain the code for the test
# The test file is scanned for lines that match some keywords. In 
# all cases the rest of the line following the keywords may be a test
# specification, of the form:
#
#  TestSpec ::= ["testname"] [: (error|success)[= "message"]]
#
# "testname" is the (optional) name of the test. If it is missing
#   then a fresh new numeric name is made up. 
# If this specification is not the first for the given test name then the rest
# of the line may be empty. Otherwise there must be at least [: error] or [:
# success], to say whether this test should fail or should succeed. 
# Both for success and for failure you can define some text that must appear 
# in the output of the test. 

#
# In a first pass, the file is scanned to collect a list of tests.
#
# Then for each test we process the file and we comment out some lines based
# on the keywords that appear in the file:
#
#  TESTDEF TestSpec  - defines a test, must appear alone on a line
#  DROP TestSpec     - this line is dropped ONLY during that test
#  KEEP TestSpec     - this line is kept ONLY during that test
#  IFTEST TestSpec   - keeps a whole bunch of lines ONLY for this test.
#                      IFTEST, ELSE and ENDIF must appear alone on a line
#   ...
#  [ ELSE
#    ...
#    ENDIF ]
#
#  IFNTEST ...       - same syntax as IFTEST. 
#
#  The lines containing TESTDEF, IFTEST, ELSE, ENDIF will always 
#  be commented. The DROP and the KEEP keywords must appear after a comment
#  character. 
#
#  The result of processing the file for each test is obtained from the 
#  directory and base names of the file along with "-tmp" followed by the 
#  original extension. Thus, for "foo/test.s" we get "foo/test-tmp.s".
#
#  If the environment variable RUNONLY is set to "t", then only the test named
#  "t" is run.
#
#  If the environment variable KEEPGOING is set, then we continue after
#  errors.
#
#  The COMMAND variable must contain the command to run for each test. The 
#  word __FILE__ is substituted with the actual file name if present,
#  otherwise the filename is appended to the command. 
#
#

my %testnames;

my ($base, $dir, $ext) = fileparse($ARGV[0], qr{\.[^.]+});

my $outfile = "$dir$base-tmp$ext";

my $action = 'COLLECT';

my $count;


my $debug = 1;

# Collect the test cases
&scanTestFile("");

$action = 'PROCESS';

if(defined $ENV{'RUNONLY'}) {
    if(! defined $testnames{$ENV{'RUNONLY'}}) {
        die "Test $ENV{'RUNONLY'} does not exist";
    }
    &runOneTest($ENV{'RUNONLY'});
} else {
    # Now run over all tests
    foreach my $t (sort (keys %testnames)) {
        &runOneTest($t);
    }
}
unlink $outfile;
1;


#
sub parseTestDef {
    my ($text, $line) = @_;
    my ($name, $success, $msg);

    if($text =~ m|^\s*(\S+)?\s*(:\s*(error\|success)\s*(\S.*)?)?$|) {
        $name = $1;
        my $istestdef = defined $2;
        $success = $3 eq "success";
        my $rest = $4;
        if(!defined $name) {
            $name = $count ++;
        }
        if(! $istestdef) {
            if(! defined $testnames{$name}) {
                die "Test definition must have : error or : success";
            } 
            return $name;
        }
        if(defined $rest) {
            if($rest =~ m|^=\s*(\S.*\S)\s*$|) {
                $msg = $1;
            } else {
                die "After success or error there must be =";
            }
        } else {
            $msg = "";
        }
        # We have found a test
        if($action eq 'COLLECT') {
            if(defined $testnames{$name} &&
               $testnames{$name}->SUCCESS != $success) {
                die "Test $name is defined both success and error";
            }
            if(defined $testnames{$name} &&
               $testnames{$name}->{'MSG'} ne "" && 
               $msg ne "") {
                warn "Ignoring duplicate message for $name: $msg";
            } else {
                print "Found test $name with msg:$msg\n";
                $testnames{$name} = { SUCCESS => $success,
                                      LINE => $line,
                                      MSG => $msg };
            }
        }
        return $name;
    } else {
        print "Invalid test specification: $text";
    }
}

# Populate the test data
sub scanTestFile {
    my($current) = @_;

    $count = 0;
    open(IN, "<$ARGV[0]") || die "Cannot open file $ARGV[1]";

    my @ifenv = ();  # The IF statements we are in: IFTEST:x or IFNTEST:x 

    my $keep = 1; # Whether to keep this lines or not

    my $line = 0;
    while(<IN>) {
        $line ++;
        my $name; 

        if($_ =~ m|^\s*TESTDEF(.*)$|) {
            $name = &parseTestDef($1, $line);
            if($action eq 'PROCESS') { print OUT "## "; }
        } elsif($_ =~ m|DROP(.*)$|) {
            $name = &parseTestDef($1, $line);
            if($action eq 'PROCESS' &&
               ($name eq $current || !$keep)) 
            { print OUT "## "; }

        } elsif($_ =~ m|KEEP(.*)$|) {
            $name = &parseTestDef($1, $line);
            if($action eq 'PROCESS' &&
                ($name ne $current || !$keep)) 
            { print OUT "## "; }

        } elsif($_ =~ m|^\s*IFTEST(.*)$|) {
            $name = &parseTestDef($1, $line);
            if($action eq 'PROCESS') {
                unshift @ifenv, ($name eq $current), $keep;
                $keep = $keep && ($name eq $current);
                if($debug) { 
                    print "IFTEST($name): Current=$current, Keep=$keep on line $line: env=", 
                    join(',', @ifenv), "\n"; 
                }
                print OUT "## ";
            }
        } elsif($_ =~ m|^\s*IFNTEST(.*)$|) {
            $name = &parseTestDef($1, $line);
            if($action eq 'PROCESS') {
                unshift @ifenv, ($name ne $current), $keep;
                $keep = $keep && ($name ne $current);
                if($debug) { 
                    print "IFNTEST($name): Current=$current, Keep=$keep on line $line: env=", 
                    join(',', @ifenv), "\n"; 
                }
                print OUT "## ";
            }
        } elsif($_ =~ m|^\s*ELSE\s*$|) {
            if($action eq 'PROCESS') {
                if($#ifenv < 1) { die "Found ELSE without IF"; }
                $keep = (! $ifenv[0] && $ifenv[1]) ? 1 : 0;
                if($debug) { print "ELSE: Keep=$keep on line $line\n"; }
                print OUT "## ";
            }
        } elsif($_ =~ m|^\s*ENDIF\s*$|) {
            if($action eq 'PROCESS') {
                if($#ifenv < 1) { die "Found ENDIF without IF"; }
                shift @ifenv; 
                $keep = shift @ifenv;
                if($debug) { print "ENDIF: Keep=$keep on line $line\n"; }
                print OUT "## ";
            }
        }

        # Just print the line if we do not recognize this line
        if($action eq 'PROCESS') {
            if(! $keep) { print OUT "## "; }
            print OUT $_;
        }
    }
}

sub runOneTest {
    my($t) = @_;
    my $ti = $testnames{$t};

    print "\n********* Running test $t from line $ti->{LINE}\n";
    open(OUT, ">$outfile\n") 
        || die "Cannot run cpp";
    &scanTestFile($t);
    close(OUT) || die "Cannot close file $outfile";
    # Now we run the command
    if(! defined $ENV{COMMAND}) {
        die "You forgot to set the COMMAND";
    }
    my $command = $ENV{COMMAND};
    # Substitute __FILE__ with the current file
    if($command =~ m|__FILE__|) {
        $command =~ s|__FILE__|$outfile|g;
    } else {
        $command .= " $outfile";
    }
    print "$command\n";
    my $msgfile = "runall_out";
    my $code = system("$command >$msgfile 2>&1");
    open(MSG, "<$msgfile") || die "Cannot read $msgfile";
    my @msgs = <MSG>;
    close(MSG) || die "Cannot close $msgfile";
    print @msgs;
    unlink $msgfile;
    if(($code == 0) != $ti->{SUCCESS}) {
        if($code == 0) {
            warn "Test case $t succeeds and it is supposed to fail";
        } else {
            warn "Test case $t fails and it is supposed to succeed";
        }            
        if(! defined($ENV{KEEPGOING})) {
            die "";
        }
    } else {
        # Now we check the output for the message
        if($ti->{MSG} ne "" &&
           ! grep(/$ti->{MSG}/, @msgs)) {
            warn "Cannot find $ti->{MSG} in output of test $t";
            if(! defined($ENV{KEEPGOING})) {
                die "";
            }
        } else {
            print "Test $t was successful\n";
            unlink $msgfile;
        }
    }
}

1;


