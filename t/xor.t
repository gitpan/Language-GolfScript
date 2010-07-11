use Language::GolfScript;
use Test::More tests => 11;
use strict;
use warnings;

# test of the built-in functions. Make sure behavior matches all the
# documentation in  http://www.golfscript.com/golfscript/builtins.html
#

sub test {
  # evaluate some GolfScript code. Assert that the output matches
  # some expected value.
  my ($code, $expected, $msg) = @_;

  my $output = Language::GolfScript::test($code);
  if ($output eq $expected) {
    if (defined $msg) {
      ok(1, $msg);
    } else {
      ok(1, "$code => $expected");
    }
  } else {
    if (defined $msg) {
      ok(0, "$msg ; $code =>\n\t\t $output\n   expected\t $expected");
    } else {
      ok(0, "$code =>\n\t\t $output\n   expected\t $expected");
    }
  }
}


#   and or xor
test("5 0and", "0", "and numbers");
test("'cat'5and", "5", "and word number");
test("[] 4and", "[ ]", "and empty array");
test("0 5or", "5", "or numbers");
test("'cat'5or", "\"cat\"", "or word number");
test("[]0or", "0", "or empty array");
test("0[]or", "[ ]", "or empty array");
test("0 'asdf'xor","\"asdf\"", "xor 0B->B");
test("14 9 xor","0", "xor AB->0");
test("5 0 xor","1", "xor A0->1");
test("0 0 xor","0", "xor 00->0");


