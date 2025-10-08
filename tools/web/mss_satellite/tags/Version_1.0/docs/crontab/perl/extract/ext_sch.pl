#!/bin/perl

use DBI;

my $dbh = getConnection();

my $sat = "G12/RSO";
my $title = "GOES-12 Rapid Scan Schedule";

my $sql = "SELECT Schedule.time, Sector.name, Sector.duration, Sector.abrv FROM Schedule, Sector " .
					"WHERE Schedule.sector=Sector.id and Schedule.satellite=\"$sat\" ORDER BY Schedule.time";

my $sth = $dbh->prepare( $sql );
$sth->execute();

my @row;

printHeader( $title );

my $col1 = "#EBEBF5";
my $col2 = "#FFFFFF";
my $col = $col1;

while( (@row=$sth->fetchrow()) )
{
	if( $col eq $col2 ) { $col = $col1; } else { $col = $col2; } 
	println( "<tr bgcolor=$col>" );
		println( "<td align=center>$row[0]</td>" );
		println( "<td align=left>$row[1]</td>" );
		println( "<td align=center>$row[2]</td>" );
		println( "<td align=center>$row[3]</td>" );
	println( "</tr>" ); 
}

$sth->finish();
$dbh->disconnect();

println( "</table></body></html>" );

sub printHeader
{
	my $title = shift;
	println( "<html><head><title>$title</title></head><body bgcolor=#FFFFFF>" );

	println( "<center><h1>$title</h1></center>" );
	println( "<center><table border=0 cellpadding=2 cellspacing=2>" );

	println( "<tr bgcolor=#6600FF>" );
		println( "<td align=center><font color=white><b>Time(UTC)</td>" );
		println( "<td align=center><font color=white><b>Scan Sector</td>" );
		println( "<td align=center><font color=white><b>Duration</td>" );
		println( "<td align=center><font color=white><b>Abbreviation Used</td>" );
	println( "</tr>" )
}

sub println
{
	print( @_, "\n" );
}
sub getConnection
{
	return DBI->connect( "DBI:mysql:database=suldan;host=thunder",
												"suldan", "hithere", {RaiseError=>1} ) || die( "Unable to Connecto to database" );
}
