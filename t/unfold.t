use Language::GolfScript;
use Test::More tests => 1;
use strict;
use warnings;

sub test {
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

test('0 1 {100<} {.@+} /', "89[ 1 1 2 3 5 8 13 21 34 55 89 ]", "unfold");


#   %%%


#   |||


#   &&&


#   ^^^


#   '''


#   """


#   \\\


#   :::


#   ;;;


#   <<<


#   >>>


#   ===


#   ,,,


#   ...


#   ???


#   (((


#   )))


#   and or xor


#   print p n puts


#   rand


#   do


#   while until


#   if


#   abs


#   zip


#   base

