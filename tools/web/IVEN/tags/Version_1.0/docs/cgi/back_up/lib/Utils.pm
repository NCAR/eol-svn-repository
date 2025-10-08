package Utils;


use URI;
require Exporter;
use vars qw(@ISA @EXPORT $VERSION );
@ISA = qw(Exporter);
@EXPORT = qw( &println $css  $jscript &nbsp &today $img_src &cr $help_src &uri_escape &quote %status_hash);
 
sub println
{
	print( @_, "\n" );
}

$css = "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"http://www.joss.ucar.edu/dpg/iven/iven.css\">";

$jscript = "<script language=javascript src=\"http://www.joss.ucar.edu/dpg/iven/iven.js\"></script>";

sub uri_escape
{
	my $str = shift;
	return URI::Escape::uri_escape( $str );
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

$img_src = "http://www.joss.ucar.edu/dpg/iven/images";
$help_src = "http://www.joss.ucar.edu/mcd/dev/help/";

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

%status_hash = ( "tbd" => "<font color=red>TBD</font>", "done" => "<font color=blue>DONE</font>", "in_progress" => "<font color=red>In Progress</font>" );
1;
