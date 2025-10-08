#!/bin/perl -w
# This program print selected items from an ebufr dump of NCDC COOP data.
#

$INFILE = "/rraid1/NEW_COOP_DATA/april.dump";
$QCFILE = "/rraid1/NEW_COOP_DATA/april.out";
$LOGFILE = "/rraid1/NEW_COOP_DATA/apr_cvt.log";

# Open the ebufr dump file (read only) and the log file and output qcf file.
open(INF, $INFILE) || die "Can't open $INFILE: $!\n";
open(LOGF, ">$LOGFILE") || die "Can't open $LOGFILE: $!\n";
open(QCF, ">$QCFILE") || die "Can't open $QCFILE: $!\n";

# Read through the input file, read past the header lines.
$id = "      ";
$year = "    ";
$month = "  ";
$day = "  ";
$hour = "  ";
$lat = "      ";
$lon = "      ";
$iden1 = "     ";
$linenum = 0;

for ($i = 0; $i < 9; $i++)
{
    $line = <INF>;
    $linenum++;
}

while ($line = <INF>)
{
    $linenum++;
    $iden1 = substr($line, 22, 5); # get something to key on
    if ($iden1 =~ /Stati/)
    {
	$id = substr($line, 47, 6);
	if ($id =~ / /)
	{
	    print LOGF "Blank id found at line $linenum";
	}
    }
    if ($iden1 =~ /Year /)
    {
	$year = substr($line, 50, 4);
    }
    if ($iden1 =~ /Month/)
    {
	$month = substr($line, 50, 1);
    }
    if ($iden1 =~ /Heigh/)
    {
	$elev = substr($line, 50, 6);
    }
    if ($iden1 =~ /Latit/)
    {
	$lat = substr($line, 50, 5);
    }
    if ($iden1 =~ /Longi/)
    {
	$lon = substr($line, 49, 6);
    }
    if ($iden1 =~ /Day  /)
    {
	$day0 = substr($line, 50, 1);
	$day1 = substr($line, 51, 1);
	if ($day1 =~ /\./)
	{
	    $day1 = $day0;
	    $day0 = "0";
	}
	$day = $day0.$day1;
    }
    if ($iden1 =~ /Hour /)
    {
	$hour0 = substr($line, 50, 1);
	$hour1 = substr($line, 51, 1);
	if ($hour1 =~ /\./)
	{
	    $hour1 = $hour0;
	    $hour0 = "0";
	}
	$hour = $hour0.$hour1;
    }
    if ($iden1 =~ /Maxim/)
    {
	$maxt = substr($line, 50, 6);
    }
    if ($iden1 =~ /Minim/)
    {
	$mint = substr($line, 50, 6);
	print QCF "$id ";
	print QCF "$year/$month/$day ";
	print QCF "$hour:00:00 ";
	print QCF " $lat  $lon ";
	print QCF " $elev";
	print QCF " $maxt K";
	print QCF " $mint K\n";

	if ($id =~ / /)
	{
	    print LOGF "Blank id found around $linenum\n";
	}
	if ($year =~ / /)
	{
	    print LOGF "Blank year found around $linenum\n";
	}
	if ($hour =~ / /)
	{
	    print LOGF "Blank hour found around $linenum\n";
	}

	if ($day =~ /30/)	# skip some lines
	{
	    $i = 0;
	    while ($line = <INF> && $i < 30)
	    {
		$i++;
		$linenum++;
	    }
	    $id = "      ";
	    $year = "    ";
	    $month = "  ";
	    $day = "  ";
	    $hour = "  ";
	    $lat = "      ";
	    $lon = "      ";
	}
    }
}
close(INF);
close(QCF);
close(LOGF);
