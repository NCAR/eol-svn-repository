package Utils;

require Exporter;
use Switch;
use vars qw(@ISA @EXPORT $VERSION );
@ISA = qw(Exporter);
@EXPORT = qw( &println &nbsp &trim &pl &cr $css $table_width $jscript &escape &formatDate $home);
use Cwd qw(abs_path);
use EQuery;

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

$css = "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/main.css\">";
$css .= "<link rel=\"stylesheet\" href=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/anytime.css\" />";
$css .= "<link rel=\"stylesheet\" href=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/bluetheme.css\" />";

$jscript = "<script language=\"javascript\" src=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/qcodiac.js\"></script>\n";
$jscript .= "<script type=\"text/javascript\" src=\"//code.jquery.com/jquery-1.10.2.min.js\"></script>\n";
$jscript .= "<script src=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/anytime.js\"></script>\n";
$jscript .= "<script src=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/jquery.tablesorter.js\"></script>";
$jscript .= "<script src=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/jquery.flot.min.js\"></script>";
$jscript .= "<script src=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/jquery.flot.categories.min.js\"></script>";
$jscript .= "<script src=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/jquery.flot.canvas.min.js\"></script>";
$jscript .= "<script src=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/jquery.flot.tickrotor.js\"></script>";
$jscript .= "<script src=\"http://dmg.eol.ucar.edu/software/tools/web/qcodiac/jquery.flot.axislabels.js\"></script>";

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

sub humanReadableFileSize {
	my $size = $_[0];

	if($size > 1073741824) {
		return sprintf("%.1f TB", $size / 1073741824);
	}
	elsif($size > 1048576) {
		return sprintf("%.1f GB", $size / 1048576);
	}
	elsif($size > 1024) {
		return sprintf("%.1f MB", $size / 1024);
	}
	else {
		return sprintf("%.1f KB", $size);
	}
}

sub toMegabytes {
    my $size = $_[0];

    return $size / 1024;
}

# Simple helper to parse YYYY-MM-DD dates to Month Day, Year format
sub humanReadableDate {
	my $date = $_[0];
	my @date = split('-', $date);

	# Parse through month
	switch($date[1]) {
		case '01' {$date[1] = "January";}
		case '02' {$date[1] = "February";}
		case '03' {$date[1] = "March";}
		case '04' {$date[1] = "April";}
		case '05' {$date[1] = "May";}
		case '06' {$date[1] = "June";}
		case '07' {$date[1] = "July";}
		case '08' {$date[1] = "August";}
		case '09' {$date[1] = "September";}
		case '10' {$date[1] = "October";}
		case '11' {$date[1] = "November";}
		case '12' {$date[1] = "December";}
	}

	# Remove the leading zero from single digit days
	$date[2] =~ s/0(?=\d)/$1/;

	# Return the final, formatted date
	return $date[1] . " " . $date[2] . ', ' . $date[0];
}

sub getIgnoredEmailAddresses {
	my $db = EQuery->new();
	$sql = "select GROUP_CONCAT(email) from contact where email like '%ucar.edu%' and (contact_id in (select distinct(contact_id) from user_role) or contact_id in (select distinct(author_id) from note));";
	$db->query($sql, "DBI:mysql:database=dmg_dts;host=riesling.eol.ucar.edu", "dts-full", "l\@micbc");
	my %result = $db->getRow();
	my $list = $result{"GROUP_CONCAT(email)"};

	$list =~ s/([\w\-\.]+)@((?:[\w]+\.)+)([a-zA-Z]{2,4})/\'$1\@$2$3\'/g;
    $list .= ", 'stott\@eol.ucar.edu'";
	return $list;
}

sub getTimeSubset {
    my @firstDate = split('-', $_[0]);
    my @secondDate = split('-', $_[1]);

    my $firstDateDays = getDays(@firstDate);
    my $secondDateDays = getDays(@secondDate);

    my $totalDays = $secondDateDays - $firstDateDays;

    if ($totalDays >= (365 * 2)) {
        return "yearly";
    } elsif ($totalDays >= 60) {
        return "monthly";
    } elsif ($totalDays >= 14) {
        return "weekly";
    } elsif ($totalDays >= 2) {
        return "daily";
    } else {
        return "hourly";
    }
}

sub getDays {
    my @date = @_;
    my $days = 0;

    $days = ($date[0] * 365) + $date[2];

    switch ($date[1]) {
        case '02' {$days += 31}
        case '03' {$days += 59}
        case '04' {$days += 90}
        case '05' {$days += 120}
        case '06' {$days += 151}
        case '07' {$days += 181}
        case '08' {$days += 212}
        case '09' {$days += 243}
        case '10' {$days += 273}
        case '11' {$days += 304}
        case '12' {$days += 334}
    }

    return $days;
}

$home = "http://dmg.eol.ucar.edu/software/tools/web/qcodiac";

1;
