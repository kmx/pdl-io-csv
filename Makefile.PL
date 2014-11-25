use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
  NAME          => 'PDL::IO::CSV',
  AUTHOR        => 'KMX <kmx@cpan.org>',
  VERSION_FROM  => 'lib/PDL/IO/CSV.pm',
  ABSTRACT_FROM => 'lib/PDL/IO/CSV.pm',
  LICENSE       => 'perl',
  PREREQ_PM     => {
        'PDL'          => 0,
        'Text::CSV_XS' => 0,
        'Scalar::Util' => 0,
        'Exporter'     => '5.59',  # we need: use Exporter 'import';
  },
  BUILD_REQUIRES => {
        'PDL'                 => 0,
        'Text::CSV_XS'        => 0,
        'Scalar::Util'        => 0,
        'Exporter'            => '5.59',  # we need: use Exporter 'import';
        'Test::More'          => '0.88',  # we need: done_testing()
        'Test::Number::Delta' => '1.05',  # we need a fix for relative deep delta_ok()
  },
  META_MERGE => {
      resources    => {
          repository   => 'https://github.com/kmx/pdl-io-csv',
      },
  },
  dist => {
      ($^O eq 'MSWin32' ? (TAR => 'ptar', TARFLAGS => '-c -C -f') : ()),
      #PREOP => 'pod2text lib/PDL/IO/CSV.pm > README'
      PREOP => 'perldoc -u lib/PDL/IO/CSV.pm | pod2markdown > README.md',
  },
);
