use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'cilly',
    VERSION_FROM        => 'Cilly.pm',
    PM                  => {
        'Cilly.pm'      => '$(INST_LIBDIR)/App/Cilly.pm',
        'CilConfig.pm' => '$(INST_LIBDIR)/App/Cilly/CilConfig.pm',
        'KeptFile.pm' => '$(INST_LIBDIR)/App/Cilly/KeptFile.pm',
        'OutputFile.pm' => '$(INST_LIBDIR)/App/Cilly/OutputFile.pm',
        'TempFile.pm' => '$(INST_LIBDIR)/App/Cilly/TempFile.pm',
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
