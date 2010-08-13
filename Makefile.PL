use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Language::GolfScript',
    AUTHOR              => q{Marty O'Brien <mob@cpan.org>},
    VERSION_FROM        => 'lib/Language/GolfScript.pm',
    ABSTRACT_FROM       => 'lib/Language/GolfScript.pod',
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
	'Filter::Simple' => 0.79,
        'File::Slurp' => 9999.0,
	'Math::BigInt' => 1.87, 
            # or whenever use Math::BigInt try=> is supported
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Language-GolfScript-*' },
);
