package Utils;

require Exporter;
use vars qw(@ISA @EXPORT $VERSION );
@ISA = qw(Exporter);
@EXPORT = qw( &println &nbsp &trim &pl &cr $css $table_width $jscript &escape &formatDate $home);

# Add a carriage return to the end of the print 
sub println
{
	print( @_, "\n" );
}

sub pl
{
	print( @_, "\n" );
}

# Print $n nbsp; - for html formatting
sub nbsp
{
	my $n = shift;
	my $str = "";
	for( my $x = 0; $x < $n; $x++ )
	{ $str = $str . "&nbsp;"; }

	return $str;
}

# Trim the spaces at the beginning and end o
#  the string
sub trim
{
	my $str = shift;
	$str =~ s/^\s+//;
	$str =~ s/\s+$//;
	return $str;
}

# Returns a string joining all of the parameters
#  with a \n and ending it with a \n
sub cr
{
	return join( "\n", @_ ) . "\n";
}

$css = "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"http://www.joss.ucar.edu/dpg/TOOLS/qcodiac/main.css\">";

$jscript = "<script language=\"javascript\" src=\"http://www.joss.ucar.edu/dpg/TOOLS/qcodiac/qcodiac.js\"></script>";

$table_width = "95%";

# Escapes spaces, special characters etc. in a URL.  
#  From Tim Bunce URL module
#  Usage:
#     $escaped = escape( "&name=Dan Sullivan" );
# Watch out - you don't want to put the http://... part, 
#  just the paramaters!!
sub escape    
{
	local($_) = @_;
	s/([\x00-\x20"#%;<>?\x7F-\xFF])/sprintf("%%%02x",ord($1))/eg;
	$_;
}

# Simple helper function to parse the YYYYMMDD dates CODIAC returns
#  to the easier to read YYYY-MM-DD
sub formatDate
{
	my $date = shift;

	$date = substr( $date, 0, 4 ) . "-" . substr( $date, 4, 2 ) . "-" . substr( $date, 6, 2 );

	return $date;
}

$home = "http://www.joss.ucar.edu/dpg/TOOLS/qcodiac";

1;
