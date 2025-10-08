package Utils;


# --------------------------------------------------------------
# Utils.pm:
#   Contains commonly used subroutines and variables, used by
#   both the editor and the public view.
#
# Author: Dan Sullivan
# Date: July, 2003
# --------------------------------------------------------------

require Exporter;
use vars qw(@ISA @EXPORT $VERSION );
@ISA = qw(Exporter);
@EXPORT = qw( &println $css $jscript &nbsp &today &left_css &body_css $img_src $html_src);
 
sub println
{
	print( @_, "\n" );
}

$css = "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"http://www.joss.ucar.edu/mcd/dev/ml/css/main.css\">";

$jscript = "<script language=javascript src=\"http://www.joss.ucar.edu/mcd/dev/ml/ml2.js\"></script>";

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
sub left_css
{
	my $src = shift;
	$src = "left.css" if( !defined( $src ) );
	return "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"http://www.joss.ucar.edu/mcd/dev/ml/css/$src\">";
}
sub body_css
{
	my $src = shift;
	$src = "body.css" if( !defined( $src ) );
	return "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"http://www.joss.ucar.edu/mcd/dev/ml/css/$src\">";
}

$img_src = "http://www.joss.ucar.edu/mcd/dev/ml/images";

$html_src = "http://www.joss.ucar.edu/mcd/dev/ml/edit";
1;
