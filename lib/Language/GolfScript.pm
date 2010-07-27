
# placeholders for big integer package.
# Math::BigInt works well enough but 
# I thought I might roll my own someday.

sub bigi::add { $_[0] + $_[1] }
sub bigi::mult { $_[0] * $_[1] }
sub bigi::sub { $_[0] - $_[1] }
sub bigi::mod { $_[0] % $_[1] }
sub bigi::div { int($_[0] / $_[1]) }
sub bigi::bitwise_or { $_[0] | $_[1] }
sub bigi::bitwise_and { $_[0] & $_[1] }
sub bigi::bitwise_xor { $_[0] ^ $_[1] }
sub bigi::neg { $_[0] * -1 }
sub bigi::pow { $_[0] ** $_[1] }
sub bigi::cmp { $_[0] <=> $_[1] }


use Math::BigInt try => 'GMP';


package Language::GolfScript;

use Carp;
use Time::HiRes;
use warnings;
use strict;
use vars qw(@STACK @LB %DISPATCH);

our $VERSION = 0.03;
our $DEBUG = $ENV{G_DEBUG} || 0;
our $INPUT = '__NOT_INITIALIZED__';
our $COUNT = 0;
our $TEST_OUTPUT;
our $TIMEOUT = 1800;

sub import {
  if ("@_" =~ /debug/i) {
    $DEBUG = 1;
  }
  if ("@_" =~ /verbose/i) {
    $DEBUG = 2;
  }
  if ("@_" =~ /count/i) {
    $COUNT = 1;
  }
  if ("@_" =~ /timeout\s*=?\s*(\d+)/i) {
    $TIMEOUT = $1;
  }
}

##################################################################

# GolfScript stack elements come in four types: int, block, string, and array. 
# In ruby, classes are used and the type of an element is evident by what
# class it is an instance of.
# In perl let's do it a little differently. On the stack, we'll identify
# element types as follows:
# Array:   normal perl array reference
# String:  scalar that begins with "
# Block:   scalar that begins with {
# Integer: any other scalar

sub is {
  my $element = shift;
  return 
    is_array($element) ? 'array' :
    is_string($element) ? 'string' :
    is_block($element) ? 'block' : 'number';
}

# GolfScript type manipulation routines:
# is_XXX:   true if element from stack is of type XXX
# to_XXX:   convert a raw scalar so it is suitable to put on the stack
# get_XXX:  convert element from stack of type XXX to raw, usable scalar

sub is_array { return ref $_[0] eq 'ARRAY' }
sub to_array { return $_[0] }
sub get_array { return $_[0] }

sub is_string { return $_[0] =~ /^\"/ }
sub to_string { return "\"" . $_[0] }
sub get_string { return substr($_[0], 1) }

sub is_number { return ref $_[0] ne 'ARRAY' && $_[0] !~ /^["{]/ } # ... }"]/;}
sub to_number { return $_[0] }
sub get_number { return $_[0] }

sub is_block { return $_[0] =~ /^\{/ }
sub to_block { return "{" . $_[0] }
sub get_block { return substr($_[0], 1) }

#############################################################################

sub gspush {
  if (@_ == 1 && !defined $_[0]) {
    if ($DEBUG) {
      print STDERR "push [", scalar @STACK, 
	"]: called for undefined element!\n";
    }
    return;
  }
  foreach (@_) {
    if ($DEBUG) {
      print STDERR "push [", scalar @STACK, "]: ", display_element($_), "\n";
    }
    push @STACK, $_;
  }
  display_stack() if $DEBUG>1;
}

sub gspop {
  my $sz = @STACK;
  for (my $i=$#LB; $i>=0; $i--) {
    last if $LB[$i] < $sz;
    $LB[$i]--;
  }

  if ($sz > 0) {
    my $elem = pop @STACK;
    if ($DEBUG) {
      print STDERR "pop [", scalar @STACK, "]: ", display_element($elem),"\n";
    }
    display_stack() if $DEBUG > 1;
    return $elem;
  } elsif ($DEBUG) {
    print STDERR "pop on empty stack!\n";
    display_stack() if $DEBUG > 1;
  }
  return;
}

sub gssplice {
  my ($lb) = @_;
  my @c = splice @STACK, $lb;
  if ($DEBUG) {
    print STDERR "splice from [$lb] ==> ", display_element(\@c), "\n";
  }
  return @c;
}

sub gscroak {
  display_stack() if $DEBUG;
  if ($DEBUG) {
    Carp::confess(@_);
  } else {
    croak @_;
  }
}

sub ___ {
  # for $] < 5.12 without ... operator
  Carp::cluck "Unimplemented method.\n";
}

# display a stack element in easily readable form.
# This is a debugging method and is NOT used for any transformation
# within the implementation or to produce the final output.
sub display_element {
  my $a = shift;
  my $output = "";
  if (is_array($a)) {
    $output = "[ ";
    $output .= display_element($_) . " " foreach @$a;
    $output .= "]";
  } elsif (is_block($a)) {
    $output .= "{" . get_block($a) . "}";
  } elsif (is_string($a)) {
    $output .= "\"" . get_string($a) . "\"";
  } elsif (is_number($a)) {
    $output .= get_number($a);
  }
  return $output;
}

sub display_stack {
  print STDERR "\n" if $DEBUG > 1;
  foreach (0..$#STACK) {
    print STDERR "STACK[$_]: ", display_element($STACK[$_]), "\n";
  }
  print STDERR "\n";
}

our %DISPATCH = 
(
 '~' => \&tilde_operator,
 '`' => \&backquote_operator,
 '!' => \&exclamation_operator,
 '@' => \&at_operator,
 "\$" => \&dollar_operator,
 '+' => \&plus_operator,
 '-' => \&minus_operator,
 '*' => \&star_operator,
 '/' => \&slash_operator,
 '%' => \&percent_operator,
 '|' => \&pipe_operator,
 '&' => \&ampersand_operator,
 '^' => \&caret_operator,
 '\\' => \&backslash_operator,
 ';' => sub { gspop() },
 '<' => \&less_than_operator,
 '>' => \&greater_than_operator,
 '=' => \&equal_operator,
 ',' => \&comma_operator,
 '.' => \&dot_operator,
 '?' => \&question_operator,
 '(' => \&open_paren_operator,
 ')' => \&close_paren_operator,
 '[' => sub { 
   my $sz = @STACK; 
   if ($DEBUG) { print STDERR "        Open bracket sz=$sz\n"; }
   push @LB, $sz 
 },
 ']' => sub { 
   my $sz = pop @LB || 0; 
   my @c = gssplice $sz;
   if ($DEBUG) { print STDERR "        Close bracket [ @c ]\n"; }
   gspush [ @c ] 
 },
 'and' => sub { evaluate('1$if') },
 'or' => sub { evaluate('1$\\if') },
 'xor' => \&xor_function, 
 'if' => \&if_function,
 'print' => \&print_function,
 'p' => sub { evaluate('`puts') },
 'n' => sub { evaluate('"\n"') },
 'puts' => sub { evaluate("print n print") },
 'rand' => \&rand_function,
 'do' => \&do_function, 
 'while' => \&while_function,
 'until' => \&until_function,
 'abs' => \&abs_function,          # should be a way to express this in GS.
 'zip' => \&zip_function,
 'base' => \&base_function,
);

# initialize $INPUT and @STACK
sub _load_input_onto_stack {
  if ($INPUT eq '__NOT_INITIALIZED__') {
    if (-t STDIN) {
      $INPUT = '';
    } else {
      local $/ = undef;
      $INPUT = <STDIN>;
    }
  }
  @STACK = (to_string($INPUT));
  print STDERR "Stack initialized: $INPUT\n" if $DEBUG;
}

sub test {
  my ($code, $optional_input) = @_;

  local @STACK = ();
  local $INPUT = $optional_input || '';

  alarm $TIMEOUT if $TIMEOUT > 0;
  evaluate($code);
  alarm 0;

  return $TEST_OUTPUT = join '', map { display_element($_) } @STACK;
}

sub run {
  my $code = shift;
  my $mode = shift || 'normal';
  _load_input_onto_stack();

  if ($COUNT) {
    if ($Math::BigInt::VERSION) {
      print STDERR "Math::BigInt::GMP library in use version $Math::BigInt::VERSION\n";
    } else {
      print STDERR "Math::BigInt::GMP library not loaded\n";
    }
  }
  my $start_time = Time::HiRes::gettimeofday();

  alarm $TIMEOUT if $TIMEOUT > 0;
  evaluate($code);
  alarm 0;
  if ($mode eq 'test') {
    my @output = @STACK;
    return @output;
  }
  gsoutput();
  print STDOUT "\n";

  if ($COUNT) {
    my $elapsed_time = Time::HiRes::gettimeofday() - $start_time;
    print "Character count: ", length($code), "\n";
    printf "Run time: %.3fs\n", $elapsed_time;
  }
}

# called at end of program. Outputs stack to STDOUT.
sub gsoutput {
  foreach my $elem (@STACK) {
    if (is_array($elem)) {
      local @STACK = @$elem;
      &gsoutput;
    } elsif (is_string($elem)) {
      print STDOUT get_string($elem);
    } elsif (is_block($elem)) {
      print STDOUT "{" . get_block($elem) . "}";
    } else {
      print STDOUT get_number($elem);
    }
  }
}

# evaluate a block of GolfScript
sub evaluate {
  my $input = shift;
  my (@block_stack, $active_block) = ();

  print STDERR "Evaluating: $input\n" if $DEBUG;

  my @tokens = tokenize($input);
  while (defined (my $token = shift @tokens)) {
    print STDERR "    Parsing: \"$token\"\n" if $DEBUG;
    if ($token eq '#') {      # comment
      my $comment = $token;
      do {
	my $token2 = shift @tokens;
	$comment .= $token2;
      } while (@tokens > 0 && substr($comment,-1) ne "\n");
      if ($DEBUG) {
	chomp $comment;
	print STDERR "    Comment:  $comment\n";
      }
    } elsif ($token eq '}') { # end block
      my $finished_block = $active_block;
      $active_block = pop @block_stack;
      if (defined $active_block) {
	$active_block .= "{" . $finished_block . "}";
      } else {
	gspush to_block($finished_block);
      }
    } elsif ($token eq '{') {
      if (defined $active_block) {
	push @block_stack, $active_block;
      }
      $active_block = "";
    } elsif (defined $active_block) {
      $active_block .= $token;
    } elsif ($token eq ':') {  # assign.
      my $var_name = shift @tokens;
      my $element = $STACK[-1];
      if ($DEBUG) {
	print STDERR "  __ASSIGN__:  {$var_name} <= sub { gspush ",
	  display_element($element), " }\n";
      }
      if (is_block($element)) {
	my $block = get_block($element);
	$DISPATCH{$var_name} = sub { evaluate($block) };
      } else {
	$DISPATCH{$var_name} = sub { gspush $element };
      }
    } elsif (defined $DISPATCH{$token}) {
      $DISPATCH{$token}->();
    } else {
      my $element = parse_token($token);
      if (defined $element) {
	gspush $element;
      }
    }
  }
}

# interpret a raw token as a string, number, or "variable"
sub parse_token {
  my $token = shift;
  if ($token =~ /^'/ && $token =~ /'$/) {
    chop $token;
    return to_string( unescaped_string(substr($token,1)) );
  } elsif ($token =~ /^"/ && $token =~ /"$/) {
    chop $token;
    return to_string( escaped_string(substr($token,1)) );
  } elsif ($token =~ /^-?\d+$/) {
    return to_number(Math::BigInt->new($token));
  } elsif ($token eq " ") {
    return;
  } else {
    # carp "unparsed token: ", ord($token), " $token\n";
    return;
  }
}

sub unescaped_string {
  my $input = shift;
  $input =~ s/\\(['\\])/$1/g;
  return $input;
}

# unimplemented escapes that Perl recognizes -- not sure
# how/if these translate to ruby's interpolated string eval.
# c u x
# \L..\E, \U..\E, \Q..\E
# \N{}
my %escapes = ( a => "\a", b => "\b", e => "\e", f => "\f", 
		l => "\l", n => "\n", r => "\r", t => "\t", 
		E => "\e", 
		"\\" => "\\", "'" => "'", '"' => "\"",  );
sub escaped_string {
  my $input = shift;
  my $output = "";
  my @chars = split //, $input;
  while (@chars) {
    my $char = shift @chars;

    if ($char eq '#' && $chars[0] eq '{') {
      # parse #{expr} expression
      # allows single quoted strings and nested { }s
      my @stack = ("}");
      my $e = "";
      shift @chars;
      while (@stack && @chars) {
	$char = shift @chars;
	if ($char eq $stack[-1]) {
	  pop @stack;
	  last if @stack == 0;
	} elsif ($char eq "\\") {
	  $char .= shift @chars;
	} elsif ($char eq "'") {
	  push @stack, $char;
	} elsif ($char eq "{" && $stack[-1] eq "}") {
	  push @stack, "}";
	}
	$e .= $char;
      }
      if ($DEBUG) {
	print STDERR "Evaluating #{$e}\n";
      }
      $output .= eval $e;
      if ($@) {
	gscroak "Eval error string \"$e\": $@\n";
      }
      next;
    }

    if ($char ne "\\") {
      $output .= $char;
      next;
    }
    if (@chars == 0) {
      $output .= "\\";  # this probably can't happen.
      next;
    }
    my $escaped_char = shift @chars;
    if (defined $escapes{$escaped_char}) {
      $output .= $escapes{$escaped_char};
      next;
    }

    if ($escaped_char =~ /[ceux]/) {
      # ... not implemented
    }

    if ($escaped_char !~ /[0-7]/) {
      $output .= $escaped_char;
      next;
    }
    my $octal = $escaped_char;
    if (@chars && $chars[0] =~ /[0-7]/) {
      $octal = 8 * $octal + (shift @chars);
      if ($chars[0] =~ /[0-7]/) {
	$octal = 8 * $octal + (shift @chars);
      }
    }
    $output .= chr($octal);
  }
  return $output;
}

# tokenize an arbitrary string of GolfScript
sub tokenize {
  my $input = shift;
  my @tokens = grep { length } 
               $input =~ /(          # capture
		 [a-zA-Z_][a-zA-Z0-9_]* |   # alphanumeric-string
		 '(?:\\.|[^'])*'? |         # single-quoted-string
                 "(?:\\.|[^"])*"? |         # double-quoted-string
                 -?[0-9]+ |                 # integer
                 . | $               # any single character including newline
                )/mgsx;
  return @tokens;
}

##################################################################

sub tilde_operator {
  my $a = gspop();
  if (is_array($a)) {        # DUMP
    gspush @$a;
  } elsif (is_string($a)) {  # EVALUATE STRING
    evaluate(get_string($a));
  } elsif (is_block($a)) {   # EVALUATE BLOCK
    $a = get_block($a);
    if ($DEBUG > 1) { print STDERR "Evaluate block: {{ $a }}\n"; }
    evaluate($a);
  } elsif (is_number($a)) {                   # BITWISE NOT
    gspush to_number(_bitwise_not(get_number($a)));
  }
}

sub backquote_operator {
  my $a = gspop();
  if (is_array($a)) {
    gspush to_string( _de_evaluate_array($a) );
  } elsif (is_string($a)) {
    gspush to_string( _de_evaluate_string(get_string($a)) );
  } elsif (is_block($a)) {
    gspush to_string( _de_evaluate_block(get_block($a)) );
  } elsif (is_number($a)) {
    gspush to_string( _de_evaluate_number(get_number($a)) );
  }
}

sub exclamation_operator {
  my $a = gspop();
  gspush to_number(is_false($a));
}

sub at_operator {             # ROTATE3
  my $a = gspop(); 
  my $b = gspop(); 
  my $c = gspop();
  gspush $b;
  gspush $a;
  gspush $c;
}

sub by_golfscript_order {
  my $d = _element_compare($a,$b);
  if ($DEBUG>1) {
    print STDERR "_element_compare ",
      display_element($a),
	" <=> ",
	  display_element($b),
	    "      ==> $d\n";
  }
  return $d;
}

sub dollar_operator {
  my $aa = gspop();
  if (is_array($aa)) {
    $aa = [ sort by_golfscript_order @$aa ];
    gspush $aa;
  } elsif (is_string($aa)) {
    $aa = _sort_string( get_string($aa) );
    gspush to_string($aa);
  } elsif (is_block($aa)) {    # SORT BY FUNCTION IN BLOCK
    $aa = get_block($aa);
    my $bb = gspop();
    if (is_array($bb)) {
      gspush _sort_array_by_function($bb, $aa);
    } elsif (is_string($bb)) {
      $bb = get_string($bb);
      gspush to_string( _sort_string_by_function($bb, $aa) );
    } else {
      &___;
    }
  } elsif (is_number($aa)) {
    $aa = -get_number($aa) - 1;
    gspush _copy_element($STACK[$aa]);
  }
}

sub _copy_element {
  my $element = shift;
  if (is_array($element)) {
    my @new_array = map { _copy_element($_) } @$element;
    return \@new_array;
  } else {
    return $element;
  }
}

sub plus_operator {
  my $a = gspop();
  if (is_array($a)) {
    my $b = gspop();
    if (is_array($b)) {
      $a = [ @$b, @$a ];
      gspush $a;
    } elsif (is_string($b)) {
      my @b = _coerce_string_to_array(get_string($b));
      $a =  [ @b, @$a ];
      $a = _coerce_array_to_string($a);
      gspush to_string($a);
    } elsif (is_block($b)) {   # BLOCK ARRAY CONCAT
      $a = _concat_block_array(get_block($b), $a);
      gspush to_block($a);
    } elsif (is_number($b)) {                   # INTEGER ARRAY CONCAT
      $a = [ $b, @$a ];
      gspush $a;
    }
  } elsif (is_block($a)) {
    my $b = gspop();
    if (is_array($b)) {        # ARRAY BLOCK CONCAT
      $a = _concat_array_block($b, get_block($a));
      gspush to_block($a);
    } elsif (is_block($b)) {   # BLOCK BLOCK CONCAT
      $b = get_block($b) . " " . get_block($a);
      gspush to_block($b);
    } elsif (is_string($b)) {  # STRING BLOCK CONCAT
      $b = get_string($b) . " " . get_block($a);
      gspush to_block($b);
    } elsif (is_number($b)) {  # INTEGER BLOCK CONCAT
      $b = get_number($b) . " " . get_block($a);
      gspush to_block($b);
    }
  } elsif (is_string($a)) {
    my $b = gspop();
    if (is_array($b)) {        # STRING CONCAT WITH ARRAY-TO-STRING CONVERSION
      $b = _coerce_array_to_string($b);
      gspush to_string($b . get_string($a));
    } elsif (is_string($b)) {  # STRING STRING CONCAT
      gspush to_string(get_string($b) . get_string($a));
    } elsif (is_block($b)) {   # BLOCK STRING CONCAT
      gspush to_block(get_block($b) . get_string($a));
    } elsif (is_number($b)) {  # INTEGER STRING CONCAT
      gspush to_string(get_number($b) . get_string($a));
    }
  } elsif (is_number($a)) {
    $b = gspop();
    if (is_array($b)) {
      push @$b, $a;
      gspush $b;
    } elsif (is_string($b)) {  # CONCATENATE NUMBER TO STRING
      gspush to_string(get_string($b) . get_number($a));
    } elsif (is_block($b)) {   # CONCATENATE NUMBER TO BLOCK
      gspush to_block(get_block($b) . get_number($a));
    } elsif (is_number($b)) {                   # ADD
      $a = bigi::add(get_number($a),get_number($b));
      gspush to_number($a);
    }
  }
}

sub minus_operator {
  my $a = gspop();
  my $b = gspop();
  
  if (is_array($a)) {
    if (is_array($b)) {
      gspush _array_array_difference($b,$a);
    } elsif (is_string($b)) {
      gspush _string_array_difference($b,$a);
    } elsif (is_block($b)) {
      gspush _block_array_difference($b,$a);
    } elsif (is_number($b)) {
      gspush _array_array_difference( [$b], $a );
    }
  } elsif (is_string($a)) {
    if (is_array($b)) {
      $b = _coerce_array_to_string($b);
      gspush to_string( _string_string_difference($b, get_string($a)) );
    } elsif (is_string($b)) {
      gspush to_string( _string_string_difference(get_string($b), get_string($a)) );
    } elsif (is_block($b)) {
      gspush to_block( _block_string_difference(get_block($b), get_string($a)) );
    } elsif (is_number($b)) {
      gspush to_string( _string_string_difference(get_number($b), get_string($a)) );
    }
  } elsif (is_block($a)) {
    if (is_array($b)) {
      $b = _coerce_array_to_string($b);
      gspush to_block( _string_string_difference($b, get_block($a)));
    } elsif (is_string($b)) {
      gspush to_block( _string_block_difference(get_string($b), get_block($a)) );
    } elsif (is_block($b)) {
      gspush to_block( _block_block_difference(get_block($b), get_block($a)) );
    } elsif (is_number($b)) {
      gspush to_block( _string_string_difference(get_number($b), get_block($a)) );
    }
  } elsif (is_number($a)) {
    if (is_array($b)) {
      gspush _array_array_difference($b, [$a]);
    }  elsif (is_block($b)) {
      gspush to_block( _block_block_difference(get_block($b),get_number($a)) );
    } elsif (is_string($b)) {
      gspush to_string( _string_string_difference(get_string($b),
						  get_number($a)) );
    } elsif (is_number($b)) {                    # SUBTRACT
      $a = bigi::sub(get_number($b), get_number($a));
      gspush to_number($a);
    }
  }
}


sub star_operator {
  my $a = gspop();
  if (is_array($a)) {
    my $b = gspop();
    if (is_array($b)) {
      gspush _array_join($b,$a);
    } elsif (is_string($b)) {
      gspush to_string( _array_string_join($a,get_string($b)) );
    } elsif (is_block($b)) {
      &___;
    } elsif (is_number($b)) {
      gspush _repeat_array($a, get_number($b)); 
    }
  } elsif (is_block($a)) {
    $a = get_block($a);
    my $b = gspop();
    if (is_array($b)) {
      _fold_array($b,$a);
    } elsif (is_string($b)) {
      $b = [ _coerce_string_to_array( get_string($b) ) ];
      _fold_array($b,$a);
    } elsif (is_block($b)) {
      &___;
    } elsif (is_number($b)) {   # REPEAT
      for my $n (1 .. get_number($b)) {
	evaluate($a);
      }
    }
  } elsif (is_string($a)) {    # JOIN
    my $b = gspop();
    if (is_array($b)) {
      gspush to_string( _array_string_join($b, get_string($a)) );
    } elsif (is_block($b)) {
      &___;
    } elsif (is_string($b)) {
      my @chars = split //, get_string($b);
      my $joined_string = join get_string($a), @chars;
      gspush to_string( $joined_string );
    } elsif (is_number($b)) {                  # STRING MULTIPLY
      gspush to_string( get_string($a) x get_number($b) );
    }
  } elsif (is_number($a)) {
    my $b = gspop();
    if (is_array($b)) {       # REPEAT ARRAY
      gspush _repeat_array($b, get_number($a));
    } elsif (is_block($b)) {  # REPEAT BLOCK
      $b = get_block($b);
      for my $n (1 .. get_number($a)) {
	evaluate($b);
      }
    } elsif (is_string($b)) { # REPEAT STRING
      $a = to_string(get_string($b) x get_number($a));
      gspush $a;
    } elsif (is_number($b)) {                  # MULTIPLY
      gspush to_number( bigi::mult(get_number($a),get_number($b)) );
    }
  }
}

sub slash_operator {
  my $a = gspop();
  if (is_array($a)) {         # ARRAY-SPLIT
    my $b = gspop();
    if (is_array($b)) {
      gspush _split_array($b,$a);
    } elsif (is_string($b) || is_block($b) || is_number($b)) {
      &___
    }
  } elsif (is_string($a)) {   # STRING SPLIT
    my $b = gspop();
    if (is_array($b)) {       # ARRAY-TO-STRING CONVERSION + STRING SPLIT
      &___
    } elsif (is_string($b)) { # STRING SPLIT
      $a = get_string($a);
      my @c = map { to_string($_) } split $a, get_string($b);
      push @c, to_string("") if $b =~ /.$a$/;  # difference between perl/ruby split?
      gspush [ @c ];
    } elsif (is_block($b)) {  # BLOCK-STRING SPLIT
      &___
    } elsif (is_number($b)) {                  # COERCE INT TO STRING AND SPLIT
      $b = get_number($b);
      my @c = map { to_string($_) } split $a, $b;
      gspush [ @c ];
    }
  } elsif (is_block($a)) {
    my $b = gspop();
    if (is_array($b)) {       # EACH
      my $block = get_block($a);
      foreach my $elem (@$b) {
	gspush $elem;
	evaluate($block);
      }
    } elsif (is_block($b)) {   # UNFOLD
      my $block = get_block($a);
      my $condition = get_block($b);
      _unfold($condition,$block);
    } elsif (is_string($b)) {  # EACH, STRING COERCED TO ARRAY, back to string

      $b = get_string($b);
      my @b = _coerce_string_to_array($b);
      my $block = get_block($a);
      my @c = ();
      foreach my $elem (@b) {
	my $lb = scalar @STACK;
	gspush $elem;
	evaluate($block);
	push @c, gssplice $lb;
      }
      if (@c) {
	gspush to_string( _coerce_array_to_string(\@c) );
      }
    } elsif (is_number($b)) {
      my $block = get_block($a);
      gspush to_number($b);
      evaluate($block);
    }
  } elsif (is_number($a)) {
    my $b = gspop();
    if (is_array($b)) {  # SUBDIVIDE ARRAY
      my @c = @$b;
      my @d = ();
      while (@c > 0) {
	push @d, [ splice @c, 0, get_number($a) ];
      }
      gspush [ @d ];
    } elsif (is_block($b)) {
      &___;
    } elsif (is_string($b)) {     # ARRAY OF SUBDIVIDED STRINGS
      $b = get_string($b);
      $a = get_number($a);
      if ($a <= 0) { gscroak "negative argument to {string number /}" }
      my @d = ();
      while (length $b > $a) {
	push @d, to_string(substr($b,0,$a));
	$b = substr($b,$a);
      }
      if (length $b > 0) {
	push @d, to_string($b);
      }
      gspush [ @d ];
    } elsif (is_number($b)) {
      my $d = bigi::div(get_number($b),get_number($a));
      gspush to_number($d);
    }
  }
}

sub percent_operator {
  my $a = gspop();
  if (is_array($a)) { # ARRAY-SPLIT, REMOVE EMPTY RESULTS
    &___
  } elsif (is_string($a)) { # SPLIT, REMOVE EMPTY RESULTS
    my $b = gspop();
    if (is_array($b)) {
      &___
    } elsif (is_block($b)) {
      &___
    } elsif (is_string($b)) {
      my @c = map { to_string($_) } grep { length } 
	split get_string($a), get_string($b);
      gspush [ @c ];
    } elsif (is_number($b)) {      # coerce int to string and split ???
      &___
    }
  } elsif (is_block($a)) {  # MAP
    my $block = get_block($a);
    my $b = gspop();
    my $is_string = 0;
    if (is_string($b)) {
      $b = [ _coerce_string_to_array(get_string($b)) ];
      $is_string = 1;
    }
    if (is_array($b)) {
      my @elems = @$b;
      my @c = ();
      foreach my $elem (@elems) {
	my $lb = scalar @STACK;
	gspush $elem;
	evaluate($block);
	# push @c, splice @STACK, $lb;
	push @c, gssplice $lb;
      }
      if ($is_string) {
	gspush to_string( _coerce_array_to_string(\@c) );
      } else {
	gspush [ @c ];
      }
    } elsif (is_block($b)) {
      &___
    } elsif (is_number($b)) {
      gspush to_number($b);
      evaluate($block);
    }
  } elsif (is_number($a)) {
    $a = get_number($a);
    my $b = gspop();
    if (is_array($b)) {  # SELECT
      if ($a < 0) {
	@$b = reverse @$b;
	$a = -$a;
      }
      my @c = @$b[ grep { $_ % $a == 0 } 0..$#{$b} ];
      gspush [ @c ];
    } elsif (is_string($b)) {
      $b = [ _coerce_string_to_array(get_string($b)) ];
      if ($a < 0) {
	@$b = reverse @$b;
	$a = -$a;
      }
      my @c = @$b[ grep { $_ % $a == 0 } 0..$#{$b} ];
      $b = _coerce_array_to_string($b);
      gspush to_string($b);
    } elsif (is_block($b)) {
      &___
    } elsif (is_number($b)) {                  # MODULUS
      my $d = bigi::mod(get_number($b),$a);
      gspush to_number($d);
    }
  }
}

sub pipe_operator {
  my $a = gspop();
  if (is_array($a)) {
    my $b = gspop();
    if (is_array($b)) {
      gspush _setwise_or($b,$a);
    } else {
      &___;
    }
  } elsif (is_block($a)) {  # SET UNION
    &___
  } elsif (is_string($a)) {
    &___
  } elsif (is_number($a)) {
    my $b = gspop();
    if (is_number($b)) {
      gspush to_number( bigi::bitwise_or(get_number($a),get_number($b)) );
    } else {
      &___;
    }
  }
}

sub ampersand_operator {
  my $a = gspop();
  my $b = gspop();

  __coerce($b,$a);

  if (is_number($a)) {
    gspush to_number(bigi::bitwise_and(get_number($a),get_number($b)));
  } elsif (is_array($a)) {
    gspush _setwise_and($b,$a);
  } elsif (is_string($a)) {
    $a = [_coerce_string_to_array(get_string($a))];
    $b = [_coerce_string_to_array(get_string($b))];
    $b = _setwise_and($b,$a);
    gspush to_string(_coerce_array_to_string($b));
  } elsif (is_block($a)) {
    $a = _coerce_string_to_array(get_block($a));
    $b = _coerce_string_to_array(get_block($b));
    $b = _setwise_and($b,$a);
    gspush to_block(_coerce_array_to_string($b));
  }
}

sub ampersand_operator_V1 {
  my $a = gspop();
  if (is_array($a)) {
    my $b = gspop();
    if (is_array($b)) {
      gspush _setwise_and($b,$a);
    } else {
      &___;
    }
  } elsif (is_block($a)) {
    &___;
  } elsif (is_string($a)) {
    &___
  } elsif (is_number($a)) {
    my $b = gspop();
    if (is_array($b) || is_block($b)) {
      &___;
    } elsif (is_string($b)) {
      &___;
    } elsif (is_number($b)) {
      gspush to_number( bigi::bitwise_and(get_number($a),     # BITWISE AND
					  get_number($b)) );
    }
  }
}

sub caret_operator {
  my $a = gspop();
  if (is_array($a)) {
    my $b = gspop();
    if (is_array($b)) {
      gspush _setwise_symmetric_difference($b,$a);
    } elsif (is_string($b)) {
      $a = _coerce_array_to_string($a);
      gspush to_string(_setwise_symmetric_string_difference(get_string($b),
							    $a) );
    } elsif (is_block($b)) {
      $a = _coerce_array_to_block($a);
      gspush to_block( _setwise_symmetric_string_difference(get_block($b),
							    $a) );
    } else {
      &___;
    }
  } elsif (is_block($a)) {
    my $b = gspop();
    if (is_array($b)) {
      $b = _coerce_array_to_block($b);
      gspush to_block(_setwise_symmetric_string_difference($b, get_block($a)));
    } else {
      &___
    }
  } elsif (is_string($a)) {
    my $b = gspop();
    if (is_array($b)) {
      $b = _coerce_array_to_string($b);
      gspush to_string(_setwise_symmetric_string_difference($b,get_string($a)));
    } elsif (is_string($b)) {
      gspush to_string(
          _setwise_symmetric_string_difference(get_string($b), get_string($a)));
    } elsif (is_block($b)) {
      &___;
    } else {
      &___;
    }
  } elsif (is_number($a)) {
    my $b = gspop();
    if (is_array($b) || is_block($b)) {
      &___;
    } elsif (is_string($b)) {
      &___;
    } elsif (is_number($b)) {
      my $z = bigi::bitwise_xor( get_number($a), get_number($b) );
      if ($DEBUG > 1) {
	print STDERR " $a ^ $b => $z\n";
      }
      gspush to_number($z);
    }
  }
}

sub backslash_operator {                   # SWAP
  my $a = gspop();
  my $b = gspop();
  gspush $a;
  gspush $b;
}

sub less_than_operator {
  my $a = gspop();
  my $b = gspop();

  if (is_number($b) && !is_number($a)) {
    ($a,$b) = ($b,$a);
  }

  if (is_string($a)) {
    $a = [ _coerce_string_to_array(get_string($a)) ];
  }
  if (is_array($a)) {                 # COMPARE ARRAY/STRING
    if (is_string($b)) {
      $b = [ _coerce_string_to_array(get_string($b)) ];
    }
    gspush _element_compare($b,$a) < 0 ? to_number(1) : to_number(0);
  } elsif (is_block($a) || is_string($a)) {
    &___
  } elsif (is_array($b)) {            # SELECT LT
    $a = get_number($a);
    $a += scalar @$b if $a < 0;
    if ($a > 0) {
      my @c = @$b[0 .. $a-1];
      gspush [ @c ];
    }
  } elsif (is_string($b)) {
    gspush to_string(  substr( get_string($b), 0,get_number($a) ) );
  } elsif (is_block($b)) {
    gspush to_block( substr( get_block($b), 0,get_number($a)) );
  } elsif (is_number($b)) {
    gspush _element_compare($b,$a) < 0 ? to_number(1) : to_number(0);
  }
}

sub greater_than_operator {
  my $a = gspop();
  my $b = gspop();
  if (is_string($a)) {
    $a = [ _coerce_string_to_array(get_string($a)) ];
  }
  if (is_array($a)) {                 # COMPARE ARRAY/STRING
    if (is_string($b)) {
      $b = [ _coerce_string_to_array(get_string($b)) ];
    }
    gspush _element_compare($b,$a) > 0 ? to_number(1) : to_number(0);
  } elsif (is_block($a)) {
    &___
  } elsif (is_array($b)) {            # SELECT GT
    $a = get_number($a);
    $a += scalar @$b if $a < 0;
    my @c = splice @$b, $a;
    gspush [ @c ];
  } elsif (is_string($b)) {
    $b = get_string($b);
    gspush to_string( substr($b,get_number($a)) );
  } elsif (is_block($b)) {
    $b = get_block($b);
    gspush to_block( substr($b,get_number($a)) );
  } elsif (is_number($b)) {
    gspush _element_compare($b,$a) > 0 ? to_number(1) : to_number(0);
  }
}

sub equal_operator {
  my $a = gspop();
  my $b = gspop();

  if (is_number($b) && !is_number($a)) { ($a,$b)=($b,$a) } # XXX "order"

  if (is_string($a)) {
    $a = [ _coerce_string_to_array( get_string($a) ) ];
  }
  if (is_array($a)) {                 # COMPARE ARRAY/STRING
    if (is_string($b)) {
      $b = [ _coerce_string_to_array(get_string($b)) ];
    }
    gspush _element_compare($a,$b) == 0 ? to_number(1) : to_number(0);
  } elsif (is_block($a)) {
    &___
  } elsif (is_number($a)) {
    $a = get_number($a);
    if (is_array($b)) {            # SELECT GT
      $a += scalar @$b if $a < 0;
      gspush $b->[$a];
    } elsif (is_string($b) || is_block($b)) {
      $b = [ _coerce_string_to_array( get_string($b) ) ];
      gspush $b->[$a];
    } elsif (is_number($b)) {
      gspush _element_compare($a,$b) == 0 ? to_number(1) : to_number(0);
    }
  }
}

sub comma_operator {
  my $a = gspop();
  if (is_array($a)) {      # SIZE OF ARRAY
    gspush to_number(scalar @$a);
  } elsif (is_block($a)) {       # GREP/MAP
    $a = get_block($a);
    my $b = gspop();
    if (!is_array($b)) {
      $b = [ $b ];
    }
    my @c = ();
    foreach my $c (@$b) {
      local @STACK = ($c);
      evaluate($a);
      my $d = gspop();
      if (is_true($d)) {
	push @c, $c;
      }
    }
    #if (@c > 0) {
    #  gspush @c;
    #}
    gspush [ @c ];
  } elsif (is_string($a)) {              # SIZE OF STRING/ARRAY
    gspush to_number( length(get_string($a)) );
  } elsif (is_number($a)) {                       # n-ELEMENT INCREASING ARRAY
    gspush [ map { to_number($_) } 0 .. get_number($a)-1 ];
  }
}

sub dot_operator {
  my $a = gspop();
  gspush $a, _copy_element($a);
}

sub question_operator {
  my $a = gspop();
  if (is_array($a)) {        # FIND IN ARRAY
    my $b = gspop();
    my $found = -1;
    for (my $i=0; $i<@$a; $i++) {
      if ($a->[$i] eq $b) { # XXX - may need to be cuter than this
	$found = $i;
	last;
      }
    }
    gspush to_number($found);
  } elsif (is_string($a)) {         # FIND IN STRING
    my $b = gspop();
    &___
  } elsif (is_block($a)) {         # FIND CONDITION
    $a = get_block($a);
    my $b = gspop();
    if (is_string($b)) {
      $b = _coerce_string_to_array( get_string($b) );
    } elsif (!is_array($b)) {
      $b = [ $b ];
    }

    my $found = undef;
    for (my $i = 0; $i < @$b; $i++) {
      local @STACK = ($b->[$i]);
      evaluate($a);
      my $d = gspop();
      if (is_true($d)) {
	$found = $b->[$i];
	last;
      }
    }
    gspush $found  if defined $found;
    return; # do nothing to stack ...
  } elsif (is_number($a)) {                        # EXPONENT
    my $b = gspop();
    gspush to_number(bigi::pow(get_number($b),
			       get_number($a)));
  }
}

sub open_paren_operator {
  my $a = gspop();
  if (is_array($a)) {        # UNCONS LEFT
    my $elem = shift @$a;
    gspush $a;
    gspush $elem; #  if defined $elem;
  } elsif (is_block($a)) {
    &___;
  } elsif (is_string($a)) {
    $a = get_string($a);
    $b = substr($a,1);
    $a = substr($a,0,1);
    gspush to_string($b || '');
    gspush to_number(ord $a);
  } elsif (is_number($a)) {                        # DECREMENT
    gspush to_number( bigi::add(get_number($a),-1) );
  }
}

sub close_paren_operator {
  my $a = gspop();
  if (is_array($a)) {        # UNCONS RIGHT
    my $elem = pop @$a;
    gspush $a;
    gspush $elem if defined $elem;
  } elsif (is_block($a)) {
    &___;
  } elsif (is_string($a)) {
    $a = get_string($a);
    my $b = chop($a);
    gspush to_string($a);
    gspush to_number(ord $b) if defined $b;
    # gspush to_string($b) if defined $b;
  } elsif (is_number($a)) {
    gspush to_number( bigi::add(get_number($a),1) );        # INCREMENT
  }
}

sub xor_function {

  # GolfScript spec says that  xor  is defined as
  #
  #         {\!!{!}*}
  #
  # I'm not having as much luck with that description.
  #
  # {!!\!!+1=}  does what I think it should do but is
  # sometimes inconsistent with ruby:
  # 0 5 xor -->  1 in Perl, 5 in ruby
  # 5 0 xor -->  1 in Perl, 1 in ruby
  #
  # what about  {.!\if} ?
  #     a b xor
  # ==> a b .!\if
  # ==> a b b !\if
  # ==> a b !b \if
  # ==> a !b b if
  # Yeah, that looks like what ruby does.

  evaluate( ".!\\if");
}

sub if_function {
  my $if_false_element = gspop();
  my $if_true_element = gspop();
  my $condition = gspop();
  if (is_true($condition)) {
    if ($DEBUG) {
      print STDERR "  $condition  is true. Executing $if_true_element\n";
    }
    if (is_block($if_true_element)) {
      evaluate( get_block($if_true_element) );
    } else {
      gspush $if_true_element;
    }
  } else {
    if ($DEBUG) {
      print STDERR "  $condition  is false. Executing $if_false_element\n";
    }
    if (is_block($if_false_element)) {
      evaluate( get_block($if_false_element) );
    } else {
      gspush $if_false_element;
    }
  }
}

sub print_function {
  my $a = gspop();
  if (is_array($a)) {
    local @STACK = @$a;
    &gsoutput;
  } elsif (is_block($a)) {
    print STDOUT "$a}";
  } elsif (is_string($a)) {
    print STDOUT get_string($a);
  } elsif (is_number($a)) {
    print STDOUT get_number($a);
  }
}

sub rand_function {
  my $a = gspop();
  if (!is_number($a)) {
    &___yada;
  } else {
    # once we have enabled big integers in the Perl module,
    # rand() * $n  won't work like we expect. Some other
    # machinations are needed.

    my $n = get_number($a);
    return 0 if bigi::cmp($n,1) <= 0;
    my $r = $n;
    while (bigi::cmp($r,$n) >= 0) {
      my $p = $n;
      $r = 0;
      #print STDERR " rand -------------------------------\n";
      while (bigi::cmp($p,0) > 0) {
	$r = bigi::mult($r,2);

	if (do { no bigint; rand() >= 0.5 }) {
	  $r = bigi::add($r,1);
	}
	$p = bigi::div($p, 2);

	#print STDERR " rand    \$p=$p, \$r=$r, \$n=$n\n";
      }
    }
    gspush to_number($r);
  }
}

sub do_function {
  my $block = gspop();
  return &___ if !is_block($block);

  $block = get_block($block);
  my $a;
  do {
    evaluate($block);
    display_stack() if $DEBUG;
    $a = gspop();
  } while (is_true($a));
}

sub _evaluate_condition {
  my $block = shift;
  evaluate($block);
  my $a = gspop();
  return is_true($a);
}

sub while_function {
  my $condition = gspop();
  my $body = gspop();

  unless (is_block($condition) && is_block($body)) {
    &___;
  }

  $condition = get_block($condition);
  $body = get_block($body);

  while (_evaluate_condition($condition)) {
    evaluate($body);
  }
}

sub until_function {
  my $condition = gspop();
  my $body = gspop();

  unless (is_block($condition) && is_block($body)) {
    &___;
  }

  $condition = get_block($condition);
  $body = get_block($body);

  until (_evaluate_condition($condition)) {
    evaluate($body);
  }
}

sub abs_function {
  my $a = gspop();
  if (is_array($a) || is_string($a) || is_block($a)) {
    &___;
  } else {
    $a = get_number($a);
    if (bigi::cmp($a,0) < 0) {
      gspush to_number(bigi::mult($a,-1));
    } else {
      gspush to_number($a);
    }
  }
}

sub zip_function {
  my $a = gspop();
  if (!is_array($a)) {
    &___
  }

  my $rows = @$a;
  my $b = [];
  my $is_string = 0;
  for (my $i=0; $i < @$a; $i++) {
    my $aa = $a->[$i];
    if (!is_array($aa)) {
      if (is_string($aa)) {
	$is_string = 1;
	$aa = [ _coerce_string_to_array(get_string($aa)) ];
      } else {
	return &___;
      }
    }
    for (my $j = 0; $j < @$aa; $j++) {
      $b->[$j][$i] = $aa->[$j];
    }
  }
  if ($is_string) {
    for (my $j=0; $j < @$b; $j++) {
      $b->[$j] = to_string( _coerce_array_to_string($b->[$j]) );
    }
  }
  gspush $b;
}

sub base_function {
  my $base = gspop();
  if (is_string($base)) {
    my $c = get_string($base);
    if (length $c > 1) {
      &___;
    }
    $base = ord $c;
    if ($DEBUG) { print STDERR "base_function: treating '$c' as base $base\n"; }
  } elsif (is_number($base)) {
    $base = get_number($base);
  } else {
    &___;
  }
  my $operand = gspop();
  if (is_array($operand)) {
    my $value = Math::BigInt->new(0);
    foreach my $elem (@$operand) {
      $value = bigi::mult($value,$base);
      $value = bigi::add($value,get_number($elem));
    }
    gspush to_number($value);
  } elsif (is_string($operand)) {
    my $value = Math::BigInt->new(0);
    foreach my $elem (_coerce_string_to_array(get_string($operand))) {
      $value = bigi::mult($value,$base);
      $value = bigi::add($value, get_number($elem));
    }
    gspush to_number($value);
  } elsif (is_block($operand)) {
    &___;
  } elsif (is_number($operand)) {
    $operand = get_number($operand);
    my @value = ();
    while (bigi::cmp($operand,0) > 0) {
      my $mod = bigi::mod($operand,$base);
      unshift @value, to_number($mod);
      $operand = bigi::div($operand,$base);
    }
    gspush [ @value ];
  }
}

sub is_true {
  my $a = shift;
  if ( (is_array($a) && @$a == 0)
       || (is_string($a) && get_string($a) eq "")
       || (is_block($a) && get_block($a) eq "")
       || (is_number($a) && get_number($a) == 0) ) {
    return 0;
  } else {
    return 1;
  }
}

sub is_false {
  my $a = shift;
  return is_true($a) ? "0" : "1";
}

sub _array_compare {
  my ($a,$b) = @_;
  for (my $i=0; $i<@$a && $i<@$b; $i++) {
    my $d = _element_compare($a->[$i], $b->[$i]);
    if ($d != 0) {
      if ($DEBUG > 1) {
	print STDERR "Array compare ", display_element($a), " <=> ",
	  display_element($b), "; compare \@ $i: $d\n";
      }
      return $d;
    }
  }
  return @$a <=> @$b;
}

sub _array_string_compare {
  my ($array, $string) = @_;
  my $array2 = [ _coerce_string_to_array($string) ];
  if ($DEBUG) {
    print STDERR "_array_string_compare: ";
    print STDERR display_element($array), " / ", display_element($string);
    print STDERR " / ";
    print STDERR display_element($array2), "\n";
  }
  return _array_compare($array, $array2);
}

# XXX - refactor candidate
# compare two elements and indicate which is greater/less
# in "golfscript" order
# return  <0  if $a is "less than" $b
#          0  if $a and $b are equivalent
#         >0  if $a is "greater than" $b
sub _element_compare {
  my ($a,$b) = @_;
  if (is_array($a)) {
    if (is_array($b)) {
      return _array_compare($a,$b);
    } elsif (is_string($b)) {
      return _array_string_compare($a, get_string($b));
    } elsif (is_block($b)) {
      $b = "{" . get_block($b) . "}";
      return _array_string_compare($a, $b);
    } else {
      gscroak "illegal compare array with number";
    }
  } elsif (is_string($a)) {
    if (is_array($b)) {
      return -_array_string_compare($b, get_string($a));
    } elsif (is_block($b)) {
      # compare string with block
      # {block  ==>  "{block} for comparison
      return get_string($a) cmp "{" . get_block($b) . "}";
    } elsif (is_number($b)) {
      gscroak "illegal compare string with number";
    } elsif (is_string($b)) {
      return get_string($a) cmp get_string($b);
    }
  } elsif (is_block($a)) {
    if (is_array($b)) {
      return -_array_string_compare($b, "{".get_block($a)."}");
    } elsif (is_string($b)) {
      return "{".get_block($a)."}" cmp get_string($b);
    } elsif (is_block($b)) {
      return get_block($a) cmp get_block($b);
    } else {
      gscroak "illegal compare block with number";
    }
  } elsif (is_number($a)) {
    if (is_array($b)) {
      gscroak "illegal compare number with array";
    } elsif (is_string($b)) {
      gscroak "illegal compare number with string";
    } elsif (is_block($b)) {
      gscroak "illegal compare number with block";
    } elsif (is_number($b)) {
      return bigi::cmp(get_number($a),get_number($b));
    }
  }
}

##################################################################

sub __coerce {

  my ($type0, $type1) = (is($_[0]), is($_[1]));

  if ($DEBUG) { print STDERR "__coerce($type0,$type1) ==> "; }

  return if $type0 eq $type1;

  # GolfScript hierarchy: block <== string <== array <== int

  if ($type0 eq 'number') {
    if ($type1 eq 'array') {
      print STDERR " $type0==>$type1\n" if $DEBUG;
      return $_[0] = [ $_[0] ];
    } elsif ($type1 eq 'string') {
      print STDERR " $type0==>$type1\n" if $DEBUG;
      
      return $_[0] = to_string(get_number($_[0]));
    } elsif ($type1 eq 'block') {
      print STDERR " $type0==>$type1\n" if $DEBUG;
      return $_[0] = to_block(get_number($_[0]));
    }
  } elsif ($type1 eq 'number') {
    if ($type0 eq 'array') {
      print STDERR " $type1===>$type0\n" if $DEBUG;
      return $_[1] = [ $_[1] ];
    } elsif ($type0 eq 'string') {
      print STDERR " $type1===>$type0\n" if $DEBUG;
      return $_[1] = to_string(get_number($_[1]));
    } elsif ($type0 eq 'block') {
      print STDERR " $type1===>$type0\n" if $DEBUG;
      return $_[1] = to_block(get_number($_[1]));
    }
  } elsif ($type0 eq 'array' && $type1 eq 'string') {
    print STDERR " $type0==>$type1\n" if $DEBUG;
    return $_[0] = to_string(_coerce_array_to_string($_[0]));
  } elsif ($type0 eq 'string' && $type1 eq 'array') {
    print STDERR " $type1===>$type0\n" if $DEBUG;
    return $_[1] = to_string(_coerce_array_to_string($_[1]));
  } elsif ($type0 eq 'array' && $type1 eq 'block') {
    print STDERR " $type0==>$type1\n" if $DEBUG;
    return $_[0] = to_block(_coerce_array_to_block($_[0]));
  } elsif ($type0 eq 'block' && $type1 eq 'array') {
    print STDERR " $type1===>$type0\n" if $DEBUG;
    return $_[1] = to_block(_coerce_array_to_block($_[0]));
  } elsif ($type0 eq 'string' && $type1 eq 'block') {
    print STDERR " $type0==>$type1\n" if $DEBUG;
    return $_[0] = to_block(get_string($_[0]));
  } elsif ($type0 eq 'block' && $type1 eq 'string') {
    print STDERR " $type1===>$type0\n" if $DEBUG;
    return $_[1] = to_block(get_string($_[1]));
  }
}

# _coerce_string_to_array
# input:     string
# output:    array of integers representing character values of the string
sub _coerce_string_to_array {
  my $string = shift;
  my @array = map { to_number(ord $_) } split //, $string;
  if ($DEBUG>1) { print STDERR " coercing string to array: $string => [ @array ]\n"; }
  return @array;
  
}

# _coerce_array_to_string
# input:      array
# output:     string of characters created from values of the string
#
# _coerce_array_to_string treats numbers as character (ASCII) values
# _coerce_array_to_block treats numbers as strings
#
# _coerce_array_to_string([65 66 67]) => "ABC"
# _coerce_array_to_block([65 66 67]) => "65 66 67"
#
sub _coerce_array_to_string {
  my $array = shift;
  my $string = "";
  for (my $i=0; $i<@$array; $i++) {
    my $elem = $array->[$i];
    if (is_array($elem)) {
      $string .= _coerce_array_to_string($elem);
    } elsif (is_string($elem)) {
      $string .= get_string($elem);
    } elsif (is_block($elem)) {
      $string .= "{" . get_block($elem) . "}";
    } elsif (is_number($elem)) {
      $string .= chr(bigi::mod(get_number($elem), 256));
    }
  }
  return $string;
}

# _coerce_array_to_block:
# input:     array
# output:    array as string
# see also: _coerce_array_to_block
sub _coerce_array_to_block {
  my $array = shift;
  my $string = "";
  for (my $i=0; $i<@$array; $i++) {
    if ($i > 0) { $string .= " " };
    my $elem = $array->[$i];
    if (is_array($elem)) {
      $string .= _coerce_array_to_string($elem);
    } elsif (is_block($elem)) {
      $string .= get_block($elem);
    } elsif (is_string($elem)) {
      $string .= get_string($elem);
    } elsif (is_number($elem)) {
      $string .= get_number($elem);
    }
  }
  return $string;
}

# _bitwise_not:
# input:   integer
# output:  -input - 1
sub _bitwise_not {
  my $integer = shift;
  return bigi::add(bigi::neg($integer), -1);
}

# _de_evaluate_array:
# input: array
# output: string that will become that array when evaluated
sub _de_evaluate_array {
  my $array = shift;
  my $output = "";
  for (my $i=0; $i<@$array; $i++) {
    my $elem = $array->[$i];
    if ($i > 0) {
      $output .= " ";
    }
    if (is_array($elem)) {
      $output .= _de_evaluate_array($elem);
    } elsif (is_block($elem)) {
      $output .= _de_evaluate_block( get_block($elem) );
    } elsif (is_string($elem)) {
      $output .= _de_evaluate_string( get_string($elem) );
    } elsif (is_number($elem)) {
      $output .= _de_evaluate_number( get_number($elem) );
    }
  }
  return "[$output]";
}

# _de_evaluate_block:
# input: block
# output: string that will become that block when evaluated
sub _de_evaluate_block {
  my $block = shift;
  return "{$block}";
}

# _de_evaluate_string:
# input: string
# output: string that will become that string when evaluated
sub _de_evaluate_string {
  my $string = shift;
  my @chars = split //, $string;

  $string = "";
  while (@chars) {
    my $char = shift @chars;
    if ($char eq "\n")    { $string .= "\\n" }
    elsif ($char eq "\t") { $string .= "\\t" }
    elsif (ord($char) < 32) { $string .= sprintf "\\%03o", ord($char) }
    elsif ($char =~ /['"]/) { $string .= "\\$char" }                    #']/){}
    else { $string .= $char }
  }
  return "\"$string\"";
}

# _de_evaluate_number:
# input: integer
# output: string that will become that integer when evaluated
sub _de_evaluate_number {
  my $integer = shift;
  return "$integer";
}

# _array_join
# input:     two arrays
# output:    single array
# function:  first array joined together by the elements of the second array
sub _array_join {
  my ($b,$a) = @_;
  my @c;
  push @c, shift @$b;
  foreach (@$b) {
    push @c, @$a;
    push @c, $_;
  }
  return [ @c ];
}

sub _array_string_join {
  my ($array,$string) = @_;
  my $output = '';
  my $numElem = 0;

  foreach my $elem (@$array) {
    $output .= $string if $numElem++;
    if (is_array($elem)) {
      $output .= _coerce_array_to_string($elem);
    } elsif (is_number($elem)) {
      $output .= get_number($elem);
    } elsif (is_string($elem)) {
      $output .= get_string($elem);
    } elsif (is_block($elem)) {
      $output .= "{" . get_block($elem) . "}";
    } else {
      &___;
    }
  }
  return $output;
}

# _sort_string:
# input:     a string
# output:    the string, sorted by character value
sub _sort_string {
  my $string = shift;
  my @chars = split //, $string;
  return join'', sort @chars;
}

# _eval:
# called from _sort_array_by_function to evaluate a value
# with respect to a block
# input:      element, block
# output:     valuation of the element with respect to the block
sub _eval {
  my ($element, $block) = @_;

  gspush $element;
  evaluate($block);
  return gspop();
}

# _sort_array_by_function:
# input:     array, block
# output:    array, sorted by valuation of each element w.r.t. the block
sub _sort_array_by_function {
  my ($array, $block) = @_;

  # good place for a Schwartzian transform
  my @output = map { $_->[0] }
               sort { _element_compare($a->[1], $b->[1]) }
	       map { [ $_ , _eval($_, $block)] } @$array;

  return [ @output ];
}


# _sort_array_by_function:
# input:     array, block
# output:    array, sorted by valuation of each element w.r.t. the block
sub _sort_string_by_function {
  my ($string, $block) = @_;
  my @array = _coerce_string_to_array($string);
  my $output = _sort_array_by_function(\@array, $block);
  my $sorted_string = join '', 
    map{ chr( is_number($_) ? get_number($_) : $_ ) } @$output;
  return $sorted_string;
}

# _concat_block_array
# input:      block, array
# output:     block
sub _concat_block_array {
  my ($block, $array) = @_;
  my $output = $block;
  foreach my $elem (@$array) {
    $output .= " ";
    if (is_array($elem)) {
      my $string = _coerce_array_to_string($elem);
      $output .= $string;
    } elsif (is_string($elem)) {
      $output .= get_string($elem);
    } elsif (is_block($elem)) {
      $output .= "{" . get_block($elem) . "}";
    } elsif (is_number($elem)) {
      $output .= get_number($elem);
    }
  }
  return $output;
}

# _concat_array_block
# input:      array, block
# output:     block
sub _concat_array_block {
  my ($array, $block) = @_;
  my $output = '';
  foreach my $elem (@$array) {
    $output .= " ";
    if (is_array($elem)) {
      my $string = _coerce_array_to_string($elem);
      $output .= $string;
    } elsif (is_string($elem)) {
      $output .= get_string($elem);
    } elsif (is_block($elem)) {
      $output .= "{" . get_block($elem) . "}";
    } elsif (is_number($elem)) {
      $output .= get_number($elem);
    }
  }
  $output .= " $block";
  return substr($output,1);
}

# _array_array_difference:
# input:    array,array
# output:   array
# elements of first array that are NOT in the second array
sub _array_array_difference {
  my ($array1, $array2) = @_;
  my %a2 = map{$_ => 1} @$array2;
  my @a1 = grep { !$a2{$_} } @$array1;
  return \@a1;
}

# _string_array_difference:
# input:    string,array
# output:   string
# characters of the string that are NOT in the values of the 2nd arr
sub _string_array_difference {
  my ($string,$array) = @_;
  my @array2 = _coerce_string_to_array($string);
  my $c = _array_array_difference(\@array2, $array);
  return _coerce_array_to_string($c);
  &___;
}

sub _block_array_difference {
  my ($block, $array) = @_;
  my $block2 = _coerce_array_to_block($array);
  my $string = _string_string_difference($block,$block2);
  return $string;
}

# input:   array,block
# output:  block
sub _array_block_difference {
  my ($array,$block) = @_;
  my $block2 = _coerce_array_to_block($array);
  my $string = _string_string_difference($block2,$block);
  return $string;
}

sub _block_string_difference {
  my ($block,$string) = @_;
  return _string_string_difference($block,$string);
}

sub _string_block_difference {
  my ($string,$block) = @_;
  return _string_string_difference($string,$block);
}

sub _block_block_difference {
  my ($block1,$block2) = @_;
  return _string_string_difference($block1,$block2);
}

sub _string_string_difference {
  my ($string1,$string2) = @_;
  my @array1 = _coerce_string_to_array($string1);
  my @array2 = _coerce_string_to_array($string2);
  my $c = _array_array_difference(\@array1, \@array2);
  my $string = _coerce_array_to_string($c);
  return $string;
}

sub _repeat_array {
  my ($array, $times) = @_;
  my @c = ();
  for my $n (1 .. $times) {
    my $copy = _copy_element($array);
    push @c, @$copy;
  }
  return [ @c ];
}

sub _fold_array {
  my ($array, $block) = @_;
  my @a = @$array;
  my $first = shift @a;
  gspush $first;
  while (@a) {
    my $elem = shift @a;
    gspush $elem;
    evaluate($block);
  }
}

sub __find_array_in_array {
  my ($array1, $array2) = @_;
  for (my $i=0; $i<@$array1; $i++) {
    my $found = $i;
    for (my $j=0; $j<@$array2; $j++) {
      if (_element_compare($array1->[$i+$j], $array2->[$j]) != 0) {
	$found = -1;
	last;
      }
    }
    if ($found >= 0) {
      return $found;
    }
  }
  return -1;
}

sub _split_array {
  my ($array1, $array2) = @_;
  my @array1 = @$array1;
  my @output;
  my $found = __find_array_in_array(\@array1, $array2);
  while ($found >= 0) {
    if ($found > 0) {
      push @output, [ splice @array1, 0, $found ];
    } else {
      push @output, [];
    }
    splice @array1, 0, scalar @$array2;
    $found = __find_array_in_array(\@array1, $array2);
  }
  push @output, [ @array1 ];
  return [ @output ];
}

sub __dup_and_check_condition {
  my $condition = shift;
  evaluate(".");
  evaluate($condition);
  my $a = gspop();
  return is_true($a) ? $a : 0;
}

sub _unfold {
  my ($condition, $block) = @_;
  my ($z, @c);
  while ($z = __dup_and_check_condition($condition)) {
    push @c, _copy_element($STACK[-1]);
    evaluate($block);
  }
  gspop();
  gspush [ @c ];
}

sub _setwise_or {
  my ($set1, $set2) = @_;
  my @c = ();
 ELEM: foreach my $elem (@$set1, @$set2) {
    foreach my $c (@c) {
      if (_element_compare($elem,$c) == 0) {
	next ELEM;
      }
    }
    push @c, $elem;
  }
  return [ @c ];
}

sub _setwise_and {
  my ($set1, $set2) = @_;

  my @c = ();
  SET1: foreach my $elem1 (@$set1) {
      foreach my $c (@c) {
	if (_element_compare($c,$elem1) == 0) {
	  next SET1;
	}
      }
      foreach my $elem2 (@$set2) {
	if (_element_compare($elem1,$elem2) == 0) {
	  push @c, $elem1;
	  next SET1;
	}
      }
  }
  return [ @c ];
}

sub _setwise_symmetric_difference {
  my ($set1, $set2) = @_;
  my @c = ();
 SET1: foreach my $elem1 (@$set1) {
    foreach my $elem2 (@$set2) {
      if (_element_compare($elem1,$elem2) == 0) {
	next SET1;
      }
    }
    foreach my $c (@c) {
      if (_element_compare($elem1,$c) == 0) {
	next SET1;
      }
    }
    push @c, $elem1;
  }
 SET2: foreach my $elem2 (@$set2) {
    foreach my $elem1 (@$set1) {
      next SET2 if _element_compare($elem1,$elem2) == 0;
    }
    foreach my $c (@c) {
      next SET2 if _element_compare($c,$elem2) == 0;
    }
    push @c, $elem2;
  }
  return [ @c ];
}

sub _setwise_symmetric_string_difference {
  my ($string1,$string2) = @_;
  my $array1 = [ _coerce_string_to_array($string1) ];
  my $array2 = [ _coerce_string_to_array($string2) ];
  my $c = _setwise_symmetric_difference($array1,$array2);
  return _coerce_array_to_string($c);
}

1;

__END__

TODO:

In double-quoted string evaluation. Perl supports
	\cN sequences:    \cA is like Ctrl-A
	\uXXXX sequences  \uABCD is unicode character 0xABCD
	\xXX sequences    \xEF is ASCII/latin character 0xEF
What does Ruby support? Where Ruby and Perl have differences,
how hard should we try to emulate Ruby's behavior?

Implementation of builtins that "coerce" (+-|&^) are not as
elegant as they could be. Should make it more explicit when
we are coercing element types. Make better and more consistent
use of "order"-style operations, too.

Better documentation in this file on functions.

Emulate nil type when we retrieve element from an empty stack?
