##Module----------------------------------------------------------------------
# <p>Utils.pm exports several constants, arrays, hash tables and 
#  subroutines used by all of the scripts.</p>
# <p>
# <u>Export</u>:<br>
#   <dd><code><b>sub println($str) </code></b>- prints the string then a '\n' character
#   <dd><code><b>sub uri_escape($uri) </code></b>- uses the URI::Escape::uri_escape() subroutine
#     to escape characters in a URL
#   <dd><code><b>sub html_encode($html) </code></b>- encodes the html tags contained it the string
#     so that the browser ignores them.  Used to fill out the edit forms.
#   <dd><code><b>sub nbsp($x) </code></b>- $x is an integer, returns $x no-break-spaces 
#   <dd><code><b>sub today() </code></b>- returns today's date in the YYYY-MM-DD format
#   <dd><code><b>sub quote($str) </code></b>- returns a string with all single quotes escaped
#   <dd><code><b>$css </code></b>- the tag to import the style sheet 
#   <dd><code><b>$jscript </code></b>- the tag to import the javascript file
#   <dd><code><b>$img_src </code></b>- the location of all the images used by IVEN
#   <dd><code><b>%status_hash </code></b>- hash of all the possible status values and their
#    respective html equivalent (i.e. blue, red etc.)
# </p>
#
# @author Dan Sullivan
##Module----------------------------------------------------------------------

package Utils;


use URI;
use HTML::Entities;
require Exporter;
use vars qw(@ISA @EXPORT $VERSION );
@ISA = qw(Exporter);
@EXPORT = qw( &println $css  $jscript &nbsp &today $img_src &cr &uri_escape &quote %status_hash &html_encode);
 
sub println
{
	print( @_, "\n" );
}

$css = "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"/dpg/iven/iven.css\">";

$jscript = "<script language=javascript src=\"/dpg/iven/iven.js\"></script>";

sub uri_escape
{
	my $str = shift;
	return URI::Escape::uri_escape( $str );
}

sub html_encode
{
	return HTML::Entities::encode( shift );
}

sub nbsp
{
	my $n = shift;
	my $str = "";
	for( my $x = 0; $x < $n; $x++ )
	{ $str = $str . "&nbsp;"; }

	return $str;
}

sub today
{
	my $year = "20" . `/usr/bin/date +%y`; 
	my $month = `/usr/bin/date +%m`;
	my $day = `/usr/bin/date +%d`;

	chop( $year ); chop( $month ); chop( $day );

	return "$year-$month-$day";
}

$img_src = "/dpg/iven/images";

sub cr
{
	return join( "\n", @_ );
}

sub quote
{
	my $str = shift;
	my $qt = "'";

	$str =~ s/$qt/\\$qt/g;

	return "$qt$str$qt";
}

%status_hash = ( "tbd" => "<font color=red><b>TBD</b></font>", 
				 "in_progress" => "<font color=orange><b>InProgress</b></font>",
				 "done" => "<font color=green><b>DONE</b></font>", 
                 "dnd" => "<font color=black><b>NoProc</b></font>",
			     "na" => "<font color=gray><b>N/A</b></font>");
1;
