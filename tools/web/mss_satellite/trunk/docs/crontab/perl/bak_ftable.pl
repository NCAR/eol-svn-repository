#!/bin/perl

use DBI;

# This script makes a quick back-up of the FileTable in the
# FileTable_bak table!!

# This works, but using mysqldump and dumping the contents of the 
#  database would probably be better.

my $dbh = connectToDB();

my $sql = "SELECT day_id, date, time, size, path, sector, sector_qc, id FROM FileTable WHERE date >= 2001001 and date <= 2001366";

my $sth = $dbh->prepare( $sql );
$sth->execute();
my @row;
my $x = 0;

while( (@row = $sth->fetchrow()) )
{
	my $sql = "INSERT INTO FileTable_bak ( day_id, date, time, size, path, sector, sector_qc, id ) VALUES (" .
  				"$row[0], $row[1], $row[2], $row[3], " . $dbh->quote( $row[4] ) . ", $row[5], $row[6], $row[7])";
if( !($x%200) )
{
	print( $row[1], "\n" );
}
$x++;
#print( $row[1], "\n" );
	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

}

$dbh->disconnect();


sub connectToDB
{
	return DBI->connect( "DBI:mysql:database=suldan;host=thunder",
												"suldan", "hithere", {RaiseError=>1} ) || die( "Unable to Connect to database" );
}
