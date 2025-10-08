#!/bin/perl -w

use DBI;

my $col1 = "#EBEBF5";
my $col2 = "#FFFFFF";
my $col = $col1;
my $dbh = getConnection();

open( OUT, ">sectors.html" );

my @sectors;
my $count = 0;
my @row;
my $sth;
my $sql;

println( *OUT, "<html><head><title>Scan Sectors</title></head><body>" );

#Routine
println( *OUT, "<center><h2>Routine Scan Sectors</h2></center>" );
println( *OUT, "<center><table border=0 cellpadding=2 cellspacing=2>" );
outSector( "GOES-8", "goes08_rout", "G08" );
outSector( "GOES-10", "goes10_rout", "G10" );
outSector( "GOES-12", "goes12_rout", "G12" );
println( *OUT, "</table>" );

#Rapid scan
println( *OUT, "<center><h2>Rapid Scan Sectors</h2></center>" );
println( *OUT, "<center><table border=0 cellpadding=2 cellspacing=2>" );
outSector( "GOES-8", "goes08_rso", "G08/RSO" );
outSector( "GOES-10", "goes10_rso", "G10/RSO" );
outSector( "GOES-12", "goes12_rso", "G12/RSO" );
println( *OUT, "</table>" );

#Super Rapid scan
println( *OUT, "<center><h2>Super Rapid Scan Sectors</h2></center>" );
println( *OUT, "<center><table border=0 cellpadding=2 cellspacing=2>" );
outSector( "GOES-8", "goes08_srso", "G08/SRSO" );
outSector( "GOES-10", "goes10_srso", "G10/SRSO" );
outSector( "GOES-12", "goes12_srso", "G12/SRSO" );
println( *OUT, "</table>" );


println( *OUT, "</body></html>" );
close( OUT );

$dbh->disconnect();

sub outSectorSch
{
	my $title = shift;
	my $file = shift;
	#my $arr_ref = shift;
	#my @arr = @{$arr_ref};
	my $arr = shift;

	$file = $file . "_" . sprintf( "%2.2d", $arr->[0] ) . ".html";
	open( OUT2, ">$file" );
	$title = "$title: $arr->[1]";

	my $sql = "SELECT time FROM Schedule WHERE sector=$arr->[0] ORDER BY time";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	my @row;
	my $col1 = "#EBEBF5";
	my $col2 = "#FFFFFF";
	my $color = $col2;
	my @times;
	my $count = 0;
	while( (@row = $sth->fetchrow()) )
	{
		$times[$count] = $row[0];
		$count++;
	}

	my $cols = int( $count / 10 );

	if( ($cols * 10) != $count ) { $cols++; }

	printSectorHeader( *OUT2, $title, $arr->[3], $count );
	for( my $row = 0; $row < 10; $row++ )
	{
		if( $color eq $col2 ) { $color = $col1; } else { $color = $col2; }

			if( defined( $times[$row] ) )
			{
				println( *OUT2, "<tr bgcolor=$color>" );
				for( my $col = 0; $col < $cols; $col++ )
				{
					my $index = $col * 10 + $row;
					if( defined( $times[$index] ) )
					{ println( *OUT2, "<td align=center>&nbsp;&nbsp;&nbsp;&nbsp;$times[$index]&nbsp;&nbsp;&nbsp;&nbsp;</td>" );}
					else 
					{ println( *OUT2, "<td align=center>&nbsp;</td>" );}
				}
				println( *OUT2, "</tr>" );
			}
	}
	
	println( *OUT2, "</table></body></html>" );
	close( OUT2 );
}

sub getConnection
{
	return DBI->connect( "DBI:mysql:database=suldan;host=thunder",
												"suldan", "hithere", {RaiseError=>1} ) || die( "Unable to Connecto to database" );
}

sub printSectorHeader
{
	my $OUT = shift;
	my $sect = shift;
	my $duration = shift;
	my $count = shift;
	println( $OUT, "<html><head><title>$sect</title></head><body bgcolor=#FFFFFF>" );

	println( $OUT, "<center><font size=+2><b><u>$sect</u></b></font></center>" );
	println( $OUT, "<center><font size=+2><b>Scan Times</b></font></center><br>" );
	println( $OUT, "<center><font size=+1><b>Duration: $duration&nbsp;&nbsp;&nbsp;Scans Per Day: $count</b></font></center><br>" );

	println( $OUT, "<center><table border=1 cellpadding=5 cellspacing=5>" );
#	println( $OUT, "<tr bgcolor=#6600FF><td colspan=$cols>&nbsp;</td>" );
		
}

sub println
{
	my $OUT3 = shift;
	my $str = shift;
	print( $OUT3 $str, "\n" ); 
}

sub printSectorHeadings
{
	println( *OUT, "<tr bgcolor=#6600FF>" );
		println( *OUT, "<td align=center><font color=white><b>Scan Sector</b></td>" );
		println( *OUT, "<td align=center><font color=white><b>Abbreviation</b></td>" );
		println( *OUT, "<td align=center><font color=white><b>Duration</b></td>" );
	println( *OUT, "</tr>" );
}

sub outSector
{
	my $title = shift;
	my $file = shift;
	my $query = shift;

	my $sql = "SELECT * FROM Sector WHERE satellite=" . $dbh->quote( $query ) . " ORDER BY name";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	println( *OUT, "<tr>" );
		println( *OUT, "<td colspan=4 align=center><font size=+1>$title</td>" );
	println( *OUT, "</tr>" );
	printSectorHeadings();

	my $col = $col1;

	while( (@row = $sth->fetchrow() ) )
	{
		outSectorSch( $title, $file, \@row );

		if( $col eq $col2 ) { $col = $col1; } else { $col = $col2; }
		println( *OUT, "<tr bgcolor=$col>" );
			println( *OUT, "<td align=center><a href=" . $file . "_" . sprintf( "%2.2d", $row[0] ) . ".html>$row[1]</a></td>" );
			println( *OUT, "<td align=center>$row[2]</td>" );
			println( *OUT, "<td align=center>$row[3]</td>" );
		println( *OUT, "</tr>" );
	}
}	
