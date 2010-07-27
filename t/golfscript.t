use Language::GolfScript;
use Test::More tests => 190;
use strict;
use warnings;
use Carp;

# test of the built-in functions. Make sure behavior matches all the
# documentation in  http://www.golfscript.com/golfscript/builtins.html

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
    Carp::cluck "\n\n\ntest failed: $code // $msg\n\n";
    if (defined $msg) {
      ok(0, "$msg ; $code =>\n\t\t $output\n   expected\t $expected");
    } else {
      ok(0, "$code =>\n\t\t $output\n   expected\t $expected");
    }
  }
}


#  ~~~
test("5~","-6","~ bitwise not");
test("-6~","5", "~ bitwise not");
test('"1 2+"~',"3", "~ evaluate string");
test('{2 3*}~',"6", "~ evaluate block");
test("[1 5 9]~","159", "~ dump array");


#  ```
test("1`","\"1\"", "` unevaluate int");
test("'1'`","\"\"1\"\"", "` unevaluate string");

test(q!'asdf
asdf'`!,'""asdf\\nasdf""', "` unevaluate escaped string");
test("[1 2  3]`",'"[1 2 3]"', "` unevaluate simple array");
test("[1 [2] 'asdf' {.+2*}]`",
     '"[1 [2] "asdf" {.+2*}]"', "` unevaluate mixed array");



#  !!!
test("0!","1", "! not");
test("[]!","1", "! not");
test("{}!","1", "! not");
test("''!","1", "! not");
test("42!","0", "! not");
test("[0]!","0", "! not");
test("{0}!","0", "! not");
test("'0'!","0", "! not");



#  @@@
test("1 2 3@","231", "\@ rot3");
test("[1][2 3][4 5 6]@","[ 2 3 ][ 4 5 6 ][ 1 ]", "\@ rot3");
test("7 8@","78", "\@ rot3/2");



#  ###
test("# comment\n3", "3", "# comment");
test("4#comment\n5+", "9", "# comment");



#  $$$
test("1 2 3 4 2\$", "12342", "\$ copy n-th item");
test("'asdf'\$", '"adfs"', "\$ sort string");
test("[5 6 9 14 4]\$", "[ 4 5 6 9 14 ]", "\$ sort array");
test("['toy' 'cat' 'dog']\$", '[ "cat" "dog" "toy" ]', "\$ sort string array");
test("['{block2}' {Block1} [123 80]]\$",
     '[ {Block1} [ 123 80 ] "{block2}" ]', "\$ sort mixed array");
test("[5 6 9 14 4]{-1*}\$", "[ 14 9 6 5 4 ]", "\$ sort array by map");
test("'ABCabc'{33^}\$", "\"acbACB\"", "\$ sort string by map"); 



#  +++
# number+
test("4 14+","18","+ add numbers");
test("'4' 14+", "\"414\"", "+ concat number string");
test("{do} 14+", "{do14}", "+ concat number block");
test("[1 2 3] 14+","[ 1 2 3 14 ]", "+ concat number array");
# string+
test("4 '14'+", "\"414\"", "+ concat string number");
test("'cat' 'dog'+", "\"catdog\"", "+ concat string string");
test("{hello} 'world'+", "{helloworld}", "+ concat string block");
test("[70 111 111] 'bar'+", "\"Foobar\"", "+ concat string array");
test("[70 512111 111] 'bar'+", "\"Foobar\"", "+ concat string array w/bignum");
# block+
test("4 {foo}+", "{4 foo}", "+ concat block number");
test("'bar'{foo}+", "{bar foo}", "+ concat block string");
test("{qu}{ux}+", "{qu ux}", "+ concat block block");
test("[5 10 15]{ack}+", "{5 10 15 ack}", "+ concat block array");
test("[5 [48 49] 20]{beep}+", 
     "{5 01 20 beep}", "+ concat block mixed-array");
# array+
test("10 [1 2 3]+", "[ 10 1 2 3 ]", "+ concat array number");
test("'asdf'[65 66 67]+","\"asdfABC\"", "+ concat array string");
test("{+-+-}[77]+","{+-+- 77}", "+ concat array block");
test("[1 2 3][4 5 6]+", "[ 1 2 3 4 5 6 ]", "+ concat array array");
test("[1 [2 3] [4 5 6]][[7 8 9][10][11]]+",
     "[ 1 [ 2 3 ] [ 4 5 6 ] [ 7 8 9 ] [ 10 ] [ 11 ] ]","+ concat array array");


#   ---
test("20 30-", "-10", "- subtraction");
test("20'0'-", "\"2\"", "- number string subtraction");
test("20{0}-", "{2}", "- number block subtraction");
test("20[0 3 6 9]-", "[ 20 ]", "- number array subtraction");
test("'as789df'78-","\"as9df\"", "- string number subtraction");
test("'as789df''fd9'-", "\"as78\"", "- string string subtraction");
test("'as789df'{fad}-", "{s789}", "- string block subtraction");
test("'as789df'[56 55]-", "\"as9df\"", "- string array subtraction");
test("{as789df}78-","{as9df}", "- block number subtraction");
test("{as789df}'fd9'-", "{as78}", "- block string subtraction");
test("{as789df}{fad}-", "{s789}", "- block block subtraction");
test("{as789df}[7 55]-", "{as89df}", "- block array subtraction");
test("[48 49 50 51]50-", "[ 48 49 51 ]", "- array number subtraction");
test("[48 49 50 51]'12'-", "\"03\"", "- array string subtraction");
test("[48 49 50 51]{12}-", "{03}", "- array block subtraction");
test("[48 49 50 51][51 49]-", "[ 48 50 ]", "- array array subtraction");


#   ***
test("12 34*", "408", "* multiplication");
test("12'34'*", "\"" . "34"x12 . "\"", "* string repeat");
test("'12'34*", "\"" . "12"x34 . "\"", "* string repeat");
test("[1 2 3]4*", "[ 1 2 3 1 2 3 1 2 3 1 2 3 ]", "* array repeat");
test("4[1 2 3]*", "[ 1 2 3 1 2 3 1 2 3 1 2 3 ]", "* array repeat");
test("2{2*}5*", "64", "* block repeat");
test("'0123'{+}*", "198", "* fold string");
test("['abc' 'ghi' 'def']{+}*", "\"abcghidef\"", "* fold string array");
test("'str1''str2'*", "\"sstr2tstr2rstr21\"", "* string join");
test("[48 49 50]'xxx'*", "\"48xxx49xxx50\"", "* array-string join");
test("'xxx'[48 49 50]*", "\"48xxx49xxx50\"", "* string-array join");
test("[1 2 5][3 4]*", "[ 1 3 4 2 3 4 5 ]", "* array join");
test("[1 [2] [3 [4 [5]]]]'-'*",
     "\"1-\002-\003\004\005\"", "* complex array-string join");


#   ///
test("10 3/", "3", "/ integer division");
test("[1 2 3 4 2 3 5][2 3]/", "[ [ 1 ] [ 4 ] [ 5 ] ]", "/ array split");
test("'a s d f'' '/",'[ "a" "s" "d" "f" ]', "/ string split");
test("[1 2 3 4 5]2/", "[ [ 1 2 ] [ 3 4 ] [ 5 ] ]", "/ subdivide array");
test("[1 2 3]{1+}/", "234", "/ each");
test('0 1 {100<} {.@+} /', "89[ 1 1 2 3 5 8 13 21 34 55 89 ]", "/ unfold");
test("'assdfs''s'/",'[ "a" "" "df" "" ]', "/ split array with empty");


#   %%%
test("5943 1000%", "943", "% modulus");
test("'assdfs''s'%",'[ "a" "df" ]', "% split array no empty");
test("[1 2 3 4 5]2%","[ 1 3 5 ]","% select positive index");
test("[1 2 3 4 5]-1%","[ 5 4 3 2 1 ]", "% select negative index");
test("[1 2 3 4 5 6 7]-3%","[ 7 4 1 ]", "% select negative index");
test("[1 2 3]{..}%", "[ 1 1 1 2 2 2 3 3 3 ]", "% map");



#   |||
test("5 3|", "7", "| bitwise or");
test("[1 2 3 4 5][7 6 5 4]|", "[ 1 2 3 4 5 7 6 ]", "| set union");


#   &&&
test("5 3&", "1", "& bitwise and");
test("[1 2 3 4 5][7 6 5 4]&", "[ 4 5 ]", "& set intersection");
test("[1 3 5 7 9][2 4 6 8]&", "[ ]", "& null set intersection");



#   ^^^
test("5 3^", "6", "^ bitwise xor");
test("[1 2 3 4 5][7 6 5 4]^", "[ 1 2 3 7 6 ]", "^ symmetric set difference");
test("[1 2 3 4][1 2 4]^", "[ 3 ]", "^ symmetric set difference");
test("'cat''toy'^", "\"caoy\"", "^ symmetric set difference of strings");
test("[65 66 67 68]'CAT'^", "\"BDT\"", "^ symmetric set diff array^string");
test("'CAT'[65 66 67 68]^", "\"TBD\"", "^ symmetric set diff string^array");
test("{CAT}[65 66 67 68]^", "{CAT65 78}", "^ symmetric set diff block^array");



#   '''
test("'abc'", "\"abc\"", "' simple string");
test("'abc\\'def\\'ghi'", "\"abc'def'ghi\"", "' string with apos");
test("'abc\\ndef\\nghi'", "\"abc\\ndef\\nghi\"", "' string with \\n");
test("'abc\\\\ndef\\nghi'", "\"abc\\ndef\\nghi\"", "' string with \\\\n");



#   """
test("\"abc\\t\"", "\"abc\t\"", "\" string with tab");
test('"def\nghi"', "\"def\nghi\"", "\" string with newline");
test(q{"\060\061\062"}, "\"012\"", "\" string with octal codes");

$::var = 14;
test('"The value is #{$::var++}"', "\"The value is 14\"",
     "\" string with #{expr}");
ok($::var == 15, "\" #{expr} evaluated");

eval { Language::GolfScript::test('"a string with #{unparsable (} expression"',
			      "this test should fail", "") };
ok($@, "\" unparsable #{expression}");



#   \\\
test("1 2\\", "21", "\\ swap");
test("[1 2 3]7\\","7[ 1 2 3 ]", "\\ swap");


#   :::
# see  t/assign.t



#   ;;;
test("4 3;", "4", "; pop");



#   <<<
test("4 5<", "1", "< less than");
test("4 4<", "0", "< equal is not less than");
test("5 4<", "0", "< greater than is not less than");
test("'cat''dog'<", "1", "< less than string compare");
test("'foo''foo'<", "0", "< equal strings are not less than");
test("[1 2 3][1 2 4]<", "1", "< array compare");
test("[2][1 2 4]<", "0", "< array compare");
test("['foo' 'cat']['foo' 'dog']<", "1", "< array compare");
test("['foo' 'rat']['foo' 'dog']<", "0", "< array compare");
test("[1 2 3 4]2<","[ 1 2 ]", "< select");
test("{asdf}-1<", "{asd}", "< select neg");


#   >>>
test("4 5>", "0", "> less than is not greater than");
test("4 4>", "0", "> equal is not greater than");
test("5 4>", "1", "> greater than");
test("'cat''dog'>", "0", "> greater than string compare");
test("'foo''foo'>", "0", "> equal strings are not less than");
test("[1 2 3][1 2 4]>", "0", "> array compare");
test("[2][1 2 4]>", "1", "> array compare");
test("['foo' 'cat']['foo' 'dog']>", "0", "> array compare");
test("['foo' 'rat']['foo' 'dog']>", "1", "> array compare");
test("[1 2 3 4 5]2>","[ 3 4 5 ]", "> select");
test("{qwerty}-3>", "{rty}", "> select neg");


#   ===
test("4 5=", "0", "= number equality");
test("75 50 25+=", "1", "= number equality");
test("[2 4 8 16 32]3=", "16", "= array element");
test("3[2 4 8 16 32]=", "16", "= array element");
test("'chocolate'5=", ord("l"), "= string element");
test("5'chocolate'=", ord("l"), "= string element");
test("[2 4 8 10 12]-2=", "10", "= array neg element");



#   ,,,
test("5,", "[ 0 1 2 3 4 ]", ", create ordered array");
test("[6 6 6 6 6],", "5", ", array size");
test("74,,", "74", ", array size");
test("10,{3%!},", "[ 0 3 6 9 ]", ", map block+array");



#   ...
test("'b'.", "\"b\"\"b\"", ". duplicate");
test("{7.*}.", "{7.*}{7.*}", ". duplicate block");
test("['foo' 'bar'].","[ \"foo\" \"bar\" ][ \"foo\" \"bar\" ]", ". dup array");



#   ???
test("2 8?", "256", "? exponentiation");
test("5[4 3 5 1]?", "2", "? find in array");
test("5[4 3 6 1]?", "-1", "? find in array not");
test("[1 2 3 4 5 6]{.*20>}?", "5", "> find first condition");
test("[1 2 3 4 5 6]{2?20>}?", "5", "?? exponentiation,find first condition");



#   (((
test("1(", "0", "( decrement number");
test("4.*(", "15", "( decrement number");
test("[1 2 3](", "[ 2 3 ]1", "( uncons left");
test("'D'(", "\"\"68", "( uncons left string like ord");
test("'toy'(", "\"oy\"116", "( uncons left string");



#   )))
test("7)", "8", ") increment number");
test("-1)", "0", ") increment number");
test("[7 8 9])", "[ 7 8 ]9", ") uncons right");
test("['asdf'])","[ ]\"asdf\"", ") uncons right");
test("'asdf')", "\"asd\"102", ") uncons right string");


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




#   print p n puts


#   rand
my $min = 9E9;
my $max = -9E9;
for (my $i=0; $i<100; $i++) {
  my $output = Language::GolfScript::test("100rand");
  $max = $output if $max < $output;
  $min = $output if $min > $output;
}
ok($min < $max && $min >= 0 && $max <= 99, "rand 0<= $min < $max <=99");



#   do
test("5{.1-.}do", "543210", "do");



#   while until


#   if
test("0 'true' 'false' if", "\"false\"", "if false");
test("'foo' 'true' 'false' if", "\"true\"", "if true");
test("4 [] {1+} {1-} if", "3", "if block");


#   abs
test("5abs","5","abs pos");
test("-5abs","5","abs neg");



#   zip
test("[[1 2 3][4 5 6][7 8 9]]zip",
     "[ [ 1 4 7 ] [ 2 5 8 ] [ 3 6 9 ] ]", "zip 3x3");
test("['cat''dog''toy']zip", "[ \"cdt\" \"aoo\" \"tgy\" ]", "zip strings");



#   base
test("[1 1 0]2 base", "6", "base from array");
test("[1 1 0]3 base", "12", "base from array");
test("17 2 base", "[ 1 0 0 0 1 ]", "base to array");


__END__


