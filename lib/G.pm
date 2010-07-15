=head1 NAME

G - source filter for Language::GolfScript module

=head1 VERSION

0.02

=head1 SYNOPSIS

    $ perl -MG myscript.gs
    $ perl -MG=debug myscript.gs

=head1 DESCRIPTION

The C<G> module wraps input into the L<Language::GolfScript> module.
This allows you to execute GolfScript code from the command line.

    $ perl -MG myscript.gs
    $ perl -MG -e "5 4+3*2/1+"

See L<Language::GolfScript> for more, including
version, author, and copyright information.

=cut

##################################################################

package G;   # use Language::GolfScript as a source filter
use Language::GolfScript;
use Filter::Simple;
# use Carp::Always;
our $VERSION = $Language::GolfScript::VERSION;
sub import { Language::GolfScript::import(@_) }
FILTER { $DB::single=1; Language::GolfScript::run($_); $_ = "" };
1;
