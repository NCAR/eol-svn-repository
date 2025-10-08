#!/bin/perl 

use DBI;
open( INPUT, "<goes08_srso2.htm" );
#open( INPUT, "<goes10_rso2.htm" );
$satellite="G08/RSO";

%g08_sects = ( "NORTHERN HEMISPHERE" => 12,
								"CONTINENTAL US (CONUS)" => 13,
								"SOUTHERN HEMISPHERE S. S." => 14,
								"FULL DISK" => 15
						);

%g10_sects = ( "NORTHERN HEMISPHERE" => 16,
								"CONTINENTAL US (SUB-CONUS)" => 17,
								"PACIFIC U.S. (PACUS)" => 18,
								"SOUTHERN HEMISPHERE S.S." => 19,
								"FULL DISK" => 20 );
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
	addSector( $vals[1], $vals[2] );

	#my $sql = "INSERT INTO Schedule (id, time, sector, satellite) VALUES ( 0, \"$vals[0]\", \"$g08_sects{$vals[1]}\", \"$satellite\" )";
#print( $sql, "\n" );
	#$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
}
	printSectors();
$dbh->disconnect();

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
