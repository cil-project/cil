package KeptFile;
use OutputFile;
@ISA = (OutputFile);

use strict;
use Carp;
use File::Basename;
use File::Spec;


########################################################################


sub new {
    croak 'bad argument count' unless @_ == 4;
    my ($proto, $basis, $suffix, $dir) = @_;
    my $class = ref($proto) || $proto;

    $basis = $basis->basis if ref $basis;
    my ($basename, undef, $basefix) = fileparse($basis, qr{\.[^.]+});
    my $filename = File::Spec->catfile($dir, "$basename.$suffix");

    my $self = $class->SUPER::new($basis, $filename);
    return $self;
}


########################################################################


1;
