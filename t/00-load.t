#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'Language::GolfScript' ) || print "Bail out!
";
}

diag( "Testing Language::GolfScript $Language::GolfScript::VERSION, Perl $], $^X" );
