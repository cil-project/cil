package OutputFile;
@ISA = ();

use strict;
use Carp;
use File::Basename;
use File::Spec;


########################################################################


my $debug = 0;


sub new {
    croak 'bad argument count' unless @_ == 3;
    my ($proto, $basis, $filename) = @_;
    my $class = ref($proto) || $proto;

    $basis = $basis->basis if ref $basis;
    my $ref = { filename => $filename,
		basis => $basis };
    my $self = bless $ref, $class;

    $self->checkRef($filename);
    $self->checkRef($basis);
    $self->checkProtected();
    $self->checkTemporary();

    Carp::cluck "OutputFile: filename == $filename, basis == $basis" if $debug;
    return $self;
}


sub filename {
    my ($self) = @_;
    return $self->{filename};
}


sub basis {
    my ($self) = @_;
    return $self->{basis};
}


########################################################################


sub checkRef {
    my ($self, $filename) = @_;
    confess "ref found where string expected: $filename" if ref $filename;
    confess "stringified ref found where string expected: $filename" if $filename =~ /\w+=HASH\(0x[0-9a-f]+\)/;
}


sub checkTemporary {
    my ($self) = @_;
    my ($basename, $path) = fileparse $self->filename;
    return if $path eq File::Spec->tmpdir . '/';
    confess "found temporary file in wrong directory: ", $self->filename
	if $basename =~ /^cil-[a-zA-Z0-9]{8}\./;
}


########################################################################


my @protected = ();


sub checkProtected {
    my ($self) = @_;
    my $abs = File::Spec->rel2abs($self->filename);

    foreach (@protected) {
	confess "caught attempt to overwrite protected file: ", $self->filename
	    if $_ eq $abs;
    }
}


sub protect {
    my ($self, @precious) = @_;
    push @protected, File::Spec->rel2abs($_)
	foreach @precious;
}


########################################################################


1;
