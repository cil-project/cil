use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'cilly',
    VERSION_FROM        => 'App/Cilly.pm',
    PM                  => {
        'App/Cilly.pm'      => '$(INST_LIBDIR)/App/Cilly.pm',
        'App/Cilly/CilConfig.pm' => '$(INST_LIBDIR)/App/Cilly/CilConfig.pm',
        'App/Cilly/KeptFile.pm' => '$(INST_LIBDIR)/App/Cilly/KeptFile.pm',
        'App/Cilly/OutputFile.pm' => '$(INST_LIBDIR)/App/Cilly/OutputFile.pm',
        'App/Cilly/TempFile.pm' => '$(INST_LIBDIR)/App/Cilly/TempFile.pm',
    },
    EXE_FILES           => [ 'cilly' ],
    PREREQ_PM => {
        'Carp'              => 0,
        'Data::Dumper'      => 0,
        'File::Basename'    => 0,
        'File::Copy'        => 0,
        'File::Spec'        => 0,
        'File::Temp'        => 0,
        'FindBin'           => 0,
        'Getopt::Long'      => 0,
        'Text::ParseWords'  => 0,
    },
    PL_FILES            => {},
    MAN3PODS            => {}, # no need for man pages for any of the .pm files
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => '' },
);

package MY;

sub MY::top_targets {
    my $str = shift->SUPER::top_targets(@_);

    $str =~ s/^pure_all.+/$& fix-cilhome/m;

    return $str;
}

sub MY::postamble {
    my $postamble = <<'MAKE_FRAG';

.PHONY: fix-cilhome

fix-cilhome:
ifdef CILHOME
	perl -p -i -e "s,^.*cilhome.*$$,\\$$::cilhome = \"$(CILHOME)\";," $(INST_LIBDIR)/App/Cilly/CilConfig.pm
endif

MAKE_FRAG

    return $postamble;
}

1;
