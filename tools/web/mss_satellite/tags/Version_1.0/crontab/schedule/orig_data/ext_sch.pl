#!/bin/perl -w

# The original script that parsed the .htm files and added the
#  schedules to the database.
use DBI;
#open( INPUT, "<g08_schedule2.htm" );
open( INPUT, "<g10_schedule.htm" );
$satellite="G10";

%g08_sects = ( "NORTHERN HEMISPHERE-EXTENDED" => 1,
								"CONTINENTAL US (CONUS)" => 2,
								"SOUTHERN HEMISPHERE" => 3,
								"FULL DISK" => 4,
								"LIMITED SOUTHERN HEMISPHERE" => 5,
								"NORTHERN HEMISPHERE" => 6
						);

%g10_sects = ( "NORTHERN HEMISPHERE" => 7,
								"PACIFIC U.S. (PACUS)" => 8,
								"SOUTHERN HEMISPHERE" => 9,
								"FULL DISK" => 10 );
my $str;
my @sectors;
my @times;
my $count = 0;
my $dbh = getConnection();
while( !eof(INPUT) )
{
	$str = <INPUT>;
	$str = <INPUT>;
	my @vals = getData( trim($str) );
	printVals( @vals );
	#addSector( $vals[1], $vals[2] );

	my $sql = "INSERT INTO Schedule (id, time, sector, satellite) VALUES ( 0, \"$vals[0]\", \"$g10_sects{$vals[1]}\", \"$satellite\" )";
	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
}
#	printSectors();

sub addSector
{
	my $sect = shift;
	my $time = shift;
	for( my $x = 0; $x < $count; $x++ )	
	{
		if( $sect eq $sectors[$x] && $time eq $times[$x] )
		{
			return;
		}
	}
	$sectors[$count] = $sect;
	$times[$count] = $time;
	$count++;
}
sub printSectors
{
	for( my $x = 0; $x < $count; $x++ )
	{
		print( $sectors[$x], "\t", $times[$x], "\n" );
	}
}
sub getData
{
	my $len = 28;
	my $str = shift;
	my @arr;
	$arr[0] = substr( $str, $len, 8 );
	$start = $len + 8 + 1; 
	$colon = index( $str, ":", $start );
	$end = $colon - 2;
	$arr[1] = trim( substr( $str, $start, $end-$start+1 ) );
	$arr[2] = trim( substr( $str, $end, 6 ) );

	if( length( $arr[2] ) == 4 ) { $arr[2] = "0" . $arr[2]; }
	$arr[2] = "00:" . $arr[2];
	return @arr;
}
sub printVals
{
	my @arr = @_;
	print( $arr[0], "\t", $arr[1], "\t", $arr[2], "\n" ); 
}

sub trim
{
	my $str = $_[0];
	chop( $str );

	$str =~ s/^\s+//;
	$str =~ s/\s+$//;
	return $str;
}
sub getConnection
{
	return DBI->connect( "DBI:mysql:database=suldan;host=thunder",
												"suldan", "hithere", {RaiseError=>1} ) || die( "Unable to connecto to database" );
}
