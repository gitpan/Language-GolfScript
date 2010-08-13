
# so we don't need to import all the L::GS methods ...
package Language::GolfScript;  

use Test::More tests => 11;

BEGIN {
  # Test::More exports the function &is, and Language::GolfScript redefines it.
  # Fix the symbol table to avoid a prototype mismatch warning ...
  delete $Language::GolfScript::{'is'};
}
use Language::GolfScript;
use strict;
use warnings;

# exercise the low-level function in lib/Language/GolfScript.pm

my $number = to_number( 42 );
my $string = to_string( 'foo' );
my $block = to_block( '.*' );
my $array = to_array( [ $number, $string, $block ] );

ok(is_number($number) && !is_number($string) 
   && !is_number($block) && !is_number($array));
ok(!is_string($number) && is_string($string) 
   && !is_string($block) && !is_string($array));
ok(!is_block($number) && !is_block($string) 
   && is_block($block) && !is_block($array));
ok(!is_array($number) && !is_array($string) 
   && !is_array($block) && is_array($array));

ok(is($number) eq 'number' && is($string) eq 'string'
	&& is($block) eq 'block' && is($array) eq 'array');

my $a = $number;
my $b = to_number(13);

__order($a,$b);
ok(get_number($a) == get_number($number) && get_number($b) == 13);

__order($a=$string,$b=$number);
ok(is($a) eq 'string' && is($b) eq 'number');

__order($a=$number,$b=$string);
ok(is($a) eq 'string' && is($b) eq 'number');

__coerce($a=$number, $b=$string);
ok(is($a) eq 'string' && get_string($a) == get_number($number));

__coerce($a=$string, $b=$number);
ok(is($b) eq 'string' && get_string($b) == get_number($number));

__coerce($a=$string, $b=$array);
ok(is_string($a) && is_string($b));
