package DLNUtils;


require Exporter;
use vars qw(@ISA @EXPORT $VERSION );
@ISA = qw(Exporter);
@EXPORT = qw( &println $css $body_css $jscript &nbsp &today $img_src &cr $help_src);
 
sub println
{
	print( @_, "\n" );
}

$css = "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"http://www.joss.ucar.edu/mcd/dev/dln/dln.css\">";
$body_css = "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"http://www.joss.ucar.edu/mcd/dev/dln/dln_body.css\">";

$jscript = "<script language=javascript src=\"http://www.joss.ucar.edu/mcd/dev/dln/dln.js\"></script>";

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

$img_src = "http://www.joss.ucar.edu/mcd/dev/dln/images";
$help_src = "http://www.joss.ucar.edu/mcd/dev/help/";

sub cr
{
	return join( "\n", @_ );
}
1;
