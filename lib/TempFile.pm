package TempFile;
use OutputFile;
@ISA = (OutputFile);

use strict;
use Carp;
use File::Temp qw(tempfile);


########################################################################


sub new {
    croak 'bad argument count' unless @_ == 3;
    my ($proto, $basis, $suffix) = @_;
    my $class = ref($proto) || $proto;

    my (undef, $filename) = tempfile('cil-XXXXXXXX',
				     DIR => File::Spec->tmpdir,
				     SUFFIX => ".$suffix",
				     UNLINK => 1);

    my $self = $class->SUPER::new($basis, $filename);
    return $self;
}


########################################################################


1;
