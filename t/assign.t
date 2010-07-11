use Language::GolfScript;
use Test::More tests => 5;
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

test("15:q;q`", "\"15\"", ": assign number");
test("'15':q;q`", "\"\"15\"\"", ": assign string");
test("[1 5]:q;q`", "\"[1 5]\"", ": assign array");
test("4:a;{3+}:b;a b", "7", ": assign to block");
test("4:a;{3+}:b;a{b}3*", "13", ": evaluate assigned block");

__END__

Other tests to do?

  Assignment to a number?
  Assignment to a string?
