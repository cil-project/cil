#!/bin/perl

use strict;
use File::Basename;

# usage runtests.pl filename
#
# The file is expected to contain the code for the test.
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
#  If the environment variable COMMENT is set, then this string is used to
#  comment out a line. Otherwise, the lines that must be dropped are not
#  printed.
#
#  If the environment variable KEEP is set, then we do not delete the 
#  files that are generated.
#
#  The COMMAND variable must contain the command to run for each test. The 
#  following substitutions are done: 
#   __FILE__ with the name of the transformed file
#  __BASENAME__ with the basename (no directory, no extension)
#  __EXT__ with the extension
#  __DIR__ with the directory
#
#  If none of the above substitutions can be performed, then the 
#  name of the file is appended to the command.
#
#

my %testnames;

my ($base, $dir, $ext) = fileparse($ARGV[0], qr{\.[^.]+});

my $outbasename = "$base-tmp";
my $outext = $ext;
my $outdir = $dir;
my $outfile = "$dir$base-tmp$ext";

my $action = 'COLLECT';

my $countFreshName;

my $hadErrors = 0;

my $debug = 0;

# Collect the test cases
&scanTestFile("");

$action = 'PROCESS';

my $countTests = 0;

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
if(not defined $ENV{'KEEP'}) { 
    unlink $outfile;
}
if($hadErrors) { 
    print "There were errors!\n";
    exit 1;
} else {
    print "All $countTests tests were successful!\n";
    exit 0;
}
1;


##############################

sub parseTestDef {
    my ($text, $line) = @_;
    my ($name, $success, $msg);

    # All the way to : is the name of the test
    if($text !~ m|^([^:]+):(.*)$|) {
        # There is no :. All of it is the name of the test. Trim spaces
        ($name) = ($text =~ m|^\s*(\S.*)$|);
        ($name) = ($name =~ m|^(.*\S)\s*$|);
        if($name eq "") {
            die "Test definition with no name and no : error or : success";
        }
        if(! defined $testnames{$name}) {
            die "Unknown test";
        } 
        return $name;
    } else {
        # We have a :  This is a test definition
        my $rest = $2;
        ($name) = ($1 =~ m|^\s*(\S.*)$|);
        ($name) = ($name =~ m|^(.*\S)\s*$|);
        if($name eq "") {
            $name = $countFreshName ++;
        }
        # See if this is success
        if($rest !~ m|^\s*(error\|success)(.*)$|) {
            die "After success or error there must be =\n";
        }
        $success = $1 eq "success";
        $rest = $2;

        # See if there is a message. Must be at least two chars long
        if($rest =~ m|^\s*=\s*(\S.*\S)\s*$|) {
            $msg = $1;
        } else {
            $msg = "";
        }
            

        # We have found a test
        if($action eq 'COLLECT') {
            if(defined $testnames{$name} &&
               $testnames{$name}->{SUCCESS} != $success) {
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
    }
}

# Populate the test data
sub scanTestFile {
    my($current) = @_;

    $countFreshName = 0;
    open(IN, "<$ARGV[0]") || die "Cannot open file $ARGV[1]";

    my @ifenv = ();  # The IF statements we are in: IFTEST:x or IFNTEST:x 

    my $keep = 1; # Whether to keep this lines or not

    my $line = 0;
    while(<IN>) {
        $line ++;
        my $name; 

        my $comment = 0;

        if($_ =~ m|^\s*TESTDEF(.*)$|) {
            $name = &parseTestDef($1, $line);
            if($action eq 'PROCESS') { $comment = 1; }
        } elsif($_ =~ m|DROP(.*)$|) {
            $name = &parseTestDef($1, $line);
            if($action eq 'PROCESS' &&
               ($name eq $current || !$keep)) { $comment = 1; }

        } elsif($_ =~ m|KEEP(.*)$|) {
            $name = &parseTestDef($1, $line);
            if($action eq 'PROCESS' &&
                ($name ne $current || !$keep))  { $comment = 1; }

        } elsif($_ =~ m|^\s*IFTEST(.*)$|) {
            $name = &parseTestDef($1, $line);
            if($action eq 'PROCESS') {
                unshift @ifenv, ($name eq $current), $keep;
                $keep = $keep && ($name eq $current);
                if($debug) { 
                    print "IFTEST($name): Current=$current, Keep=$keep on line $line: env=", 
                    join(',', @ifenv), "\n"; 
                }
                $comment = 1;
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
                $comment = 1;
            }
        } elsif($_ =~ m|^\s*ELSE\s*$|) {
            if($action eq 'PROCESS') {
                if($#ifenv < 1) { die "Found ELSE without IF"; }
                $keep = (! $ifenv[0] && $ifenv[1]) ? 1 : 0;
                if($debug) { print "ELSE: Keep=$keep on line $line\n"; }
                $comment = 1;
            }
        } elsif($_ =~ m|^\s*ENDIF\s*$|) {
            if($action eq 'PROCESS') {
                if($#ifenv < 1) { die "Found ENDIF without IF"; }
                shift @ifenv; 
                $keep = shift @ifenv;
                if($debug) { print "ENDIF: Keep=$keep on line $line\n"; }
                $comment = 1;
            }
        }

        # Just print the line if we do not recognize this line
        if($action eq 'PROCESS') {
            if(! $keep || $comment) { 
                if(defined $ENV{'COMMENT'}) {
                    print OUT $ENV{'COMMENT'};
                    print OUT " ";
                    print OUT $_;
                } else {
                    print OUT "\n";
                }
            } else {
                print OUT $_;
            }
        }
    }
}

sub runOneTest {
    my($t) = @_;
    my $ti = $testnames{$t};

    $countTests ++;

    print "\n********* Running test $t from line $ti->{LINE}\n";
    open(OUT, ">$outfile\n") 
        || die "Cannot write $outfile";
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
    }
    if($command =~ m|__DIR__|) { 
        $command =~ s|__DIR__|$outdir|g;
    } 
    if($command =~ m|__BASENAME__|) { 
        $command =~ s|__BASENAME__|$outbasename|g;
    }
    if($command =~ m|__EXT__|) { 
        $command =~ s|__EXT__|$outext|g;
    }
    if($command eq $ENV{COMMAND}) {
        $command .= " $outfile";
    }
    print "$command\n";
    my $msgfile = "runall_out";
    my $code = system("($command) >$msgfile 2>&1");
    open(MSG, "<$msgfile") || die "Cannot read $msgfile";
    my @msgs = <MSG>;
    close(MSG) || die "Cannot close $msgfile";
    print @msgs;
    unlink $msgfile;
    if(($code == 0) != $ti->{SUCCESS}) {
        if($code == 0) {
            warn "Test case $t (line $ti->{LINE}) succeeds and it is supposed to fail";
        } else {
            warn "Test case $t (line $ti->{LINE}) fails and it is supposed to succeed";
        } 
        $hadErrors = 1;
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


