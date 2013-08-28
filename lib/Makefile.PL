use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

my $cilly = 'cilly';
my $Cilly = ucfirst($cilly);

WriteMakefile(
    NAME                => $cilly,
    VERSION_FROM        => 'App/'.$Cilly.'.pm',
    PM                  => {
        'App/'.$Cilly.'.pm'      => '$(INST_LIBDIR)/App/'.$Cilly.'.pm',
        'App/'.$Cilly.'/CilConfig.pm' => '$(INST_LIBDIR)/App/'.$Cilly.'/CilConfig.pm',
        'App/'.$Cilly.'/KeptFile.pm' => '$(INST_LIBDIR)/App/'.$Cilly.'/KeptFile.pm',
        'App/'.$Cilly.'/OutputFile.pm' => '$(INST_LIBDIR)/App/'.$Cilly.'/OutputFile.pm',
        'App/'.$Cilly.'/TempFile.pm' => '$(INST_LIBDIR)/App/'.$Cilly.'/TempFile.pm',
    },
    EXE_FILES           => [ '../bin/'.$cilly ],
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

sub MY::postamble {
    my $postamble = <<'MAKE_FRAG';

.PHONY: real-uninstall

force_uninstall:
ifeq ($(INSTALLDIRS), site)
	$(RM_F) $(DESTINSTALLSITEBIN)/$(NAME)
	cd $(DESTINSTALLSITELIB) && $(RM_F) $(TO_INST_PM)
	$(RM_RF) $(DESTINSTALLSITEARCH)/auto/$(FULLEXT)
else
	echo "force_uninstall works only if INSTALLDIRS=site"
endif

MAKE_FRAG

    return $postamble;
}

1;
