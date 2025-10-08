#! /usr/bin/perl -w

use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use MySqlDatabase;
my $msg = "";

my $database = MySqlDatabase->new("zediupdate","change-456");
#$database->setHost("merlot.eol.ucar.edu");
$database->connect();

if ($#ARGV != 1) {
    print<<EOM;
syntax: diffTaplog TAPLOG Dataset_id

Ex: diffTaplog taplog.233.html 154.002
EOM
}

my $taplog = $ARGV[0];
my $dataset_id = $ARGV[1];

#load files from CODIAC
($msg, my %data) = $database->selectFull("file","*","dataset_id=\"$dataset_id\"");
if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }
unless ($database->getRows() >= 1) { print "No files found in CODIAC!\n"; exit(1); }

#figure out how the data is layed out
my $filenameIndex = 0;
my $begin_dateIndex = 0;
my $end_dateIndex = 0;
my $eventIndex = 0;
my $sizeIndex = 0;
for (my $i = 0; $i <= $#{$data{"name"}}; $i++) {

	if (@{$data{"name"}}[$i] eq "filename") { $filenameIndex = $i; }
	if (@{$data{"name"}}[$i] eq "begin_date") { $begin_dateIndex = $i; }
	if (@{$data{"name"}}[$i] eq "end_date") { $end_dateIndex = $i; }
	if (@{$data{"name"}}[$i] eq "event") { $eventIndex = $i; }
	if (@{$data{"name"}}[$i] eq "size_kb") { $sizeIndex = $i; }
}


#load taplog
unless (-e "/net/www/raf/Catalog/$taplog") {print "Taplog not found!\n"; $database->desconnect(); exit(1); }
open FILE, "/net/www/raf/Catalog/$taplog";

#compare taplog to CODIAC
while (<FILE>)
{
	if ($_ =~ /\<[Aa]\S*\>\s*(\S{3,7})\s+(\S+)?\s+(\S+)?\s+(\d+\/\d+\/\d+)\s+(\d+:\d+:\d+)\??\s+(\d+:\d+:\d+)\??\s+(\d+\.?\d*).*\</)
	{
		chomp(my $fltno = $1);

		my $filename = "ERROR1";
		if (defined($2)) { $filename = $2;}
		else { $filename = $1; }

		my $temp = "ERROR2";
		if (defined($3)) {chomp($temp = $3); }

		chomp(my $datetemp = $4);
		chomp(my $start_time = $5);
		chomp(my $end_time = $6);
		chomp(my $size = $7);
		if ($filename =~ /^TL/)
		{
			$filename = $temp;
		}
		if ($filename eq "-") { $filename = $fltno; }
		$filename =~ s/\*//g;
		$fltno =~ s/\*//g;
		
		my @datesplit = split('/',$datetemp);
		my $date = "$datesplit[2]-$datesplit[0]-$datesplit[1]";
		if (length($datesplit[0]) == 1) { $datesplit[0] = "0" . $datesplit[0]; }

		my @split = split(':', $start_time);
		my $startnum = ($datesplit[2] . $datesplit[0])*100000000 + ($datesplit[1]*86400 + $split[0]*3600 + $split[1]*60 + $split[2]);
		#print "$startnum\n";
		@split = split(':', $end_time);
		my $endnum = ($datesplit[2] . $datesplit[0])*100000000 + ($datesplit[1]*86400 + $split[0]*3600 + $split[1]*60 + $split[2]);
		#print "$endnum\n";

		my $chk = 0;
		for (my $i = 0; $i <= $#{$data{"row"}}; $i++) {
			if ($filename eq @{@{$data{"row"}}[$i]}[$filenameIndex]) {

				print "Matched $filename (" . $fltno . ")\n";
				$chk = 1;

				@split = split(/[-: ]/, @{@{$data{"row"}}[$i]}[$begin_dateIndex]);
				my $startnumCODIAC = ($split[0] . $split[1])*100000000 + ($split[2]*86400 + $split[3]*3600 + $split[4]*60 + $split[5]);
				#print "$startnumCODIAC\n";

				@split = split(/[-: ]/, @{@{$data{"row"}}[$i]}[$end_dateIndex]);
				my $endnumCODIAC = ($split[0] . $split[1])*100000000 + ($split[2]*86400 + $split[3]*3600 + $split[4]*60 + $split[5]);
				#print "$endnumCODIAC\n";

				if ($startnumCODIAC > $endnumCODIAC) {
					print "\tWARNING: StartTime > EndTime!\n";
					print "\t\tCODIAC: " .@{@{$data{"row"}}[$i]}[$begin_dateIndex] . "\n";
                                        print "\t\tCODIAC: " .@{@{$data{"row"}}[$i]}[$end_dateIndex] . "\n";
				}
				if (abs($startnum - $startnumCODIAC) > 600 && ($startnumCODIAC - $startnum > 86400+600 || $startnumCODIAC - $startnum < 86400-600))
				{
					print "\tStart Time Mismatch (". int(abs($startnum - $startnumCODIAC)/60)  . "):\n";
					print "\t\tTaplog: $date $start_time \n";
					print "\t\tCODIAC: " .@{@{$data{"row"}}[$i]}[$begin_dateIndex] . "\n";
				}
				if (($startnumCODIAC - $startnum < 86400+600 && $startnumCODIAC - $startnum > 86400-600) && $fltno !~ /[bBcCdDeEfFgG]$/) {
					print "\tWARNING: Start Date increment on first or non-parted file\n";
					print "\t\tTaplog: $date $start_time \n";
					print "\t\tCODIAC: " .@{@{$data{"row"}}[$i]}[$begin_dateIndex] . "\n";
				}
				if (abs($endnum - $endnumCODIAC) > 600 && ($endnumCODIAC - $endnum > 86400+600 || $endnumCODIAC - $endnum < 86400-600))
                                {
                                        print "\tEnd Time Mismatch (". int(abs($endnum - $endnumCODIAC)/60)  . "):\n";
                                        print "\t\tTaplog: $date $end_time \n";
                                        print "\t\tCODIAC: " .@{@{$data{"row"}}[$i]}[$end_dateIndex] . "\n";
                                }
				if ($fltno ne @{@{$data{"row"}}[$i]}[$eventIndex])
				{
					print "\tFlight Name Mismatch:\n";
					print "\t$fltno != " . @{@{$data{"row"}}[$i]}[$eventIndex] . "\n";
				}
				last;
			}
		}
		if ($chk == 0) {print "Could not find a CODIAC match for $filename ($fltno)\n\n";}
	}
	elsif ($_ =~ /\<[Aa]\>/)
	{
		print "Unrecognized Line:\n";
		print "\t$_\n";
	}
}
$database->disconnect();
exit(0);
