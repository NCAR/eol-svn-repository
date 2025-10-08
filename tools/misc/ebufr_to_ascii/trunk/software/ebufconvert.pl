#! /usr/bin/perl -w
##Module---------------------------------------------------------------------
# <p>This script reads files and conversion information from CODIAC for ebuf
# files and convertes them to an ASCII format and loads the ASCII files into
# CODIAC. Extensive logs are collected and stored in the directory the script
# is run from.</p>
# <p>Checking Issues: Time is always 00:00:00 in ASCII files for datasets 1.58
# and 1.59 causing a check failure on these datasets. Files in dataset 1.84
# are incorrectly loaded, the files are month long but they are loaded as just
# the first day of the month. 1.004: Failes file size check! Output file is 
# smaller than the input file.</p>
# <p>This script is setup for testing, no changes will be made to CODIAC
# and all converted files are placed under /net/work/Projects/ebufconversion.
# When ready to run change $testing to a value of 0.</p>
#
# @author Sean Stroble
# @version 1.0 Original
#
# Usage:    ebufconvert.pl
#
# Example:  ebufconvert.txt
#
##Module---------------------------------------------------------------------


use strict;
use lib "/net/work/lib/perl/mysql";
use File::Path qw(mkpath);
use MySqlDatabase;
use MySqlFile;
my $msg = "";


#####USER SET VARIABLES####
my $startAtDataset = 51;
my $testing = 0; ##CHANGE THIS TO 0 to run for real!



my $src_format = 10; #ebufr
my @dest_format = (18,19,26); #Soundings, precip, surface ascii files..

#connect to MySql Databases
my $tsunami = MySqlDatabase->new("zediupdate","change-456");
$tsunami->connect();

my $merlot = MySqlDatabase->new("zediupdate","change-456");
$merlot->setHost("merlot.eol.ucar.edu");
$merlot->connect();

#Nifty 'die' hook cleans everything up if the script dies.
local $SIG{'__DIE__'} = sub { 
    print "Rolling back database!\n"; 
    close LOG; 
    close LOGI; 
    print $tsunami->rollback(); 
    print $tsunami->disconnect(); 
    print $merlot->rollback(); 
    print $merlot->disconnect(); 
};

#Get a list of dataset_id's from the format_conversions table which convert ebufr data (format_id = 10)
($msg, my @dataset_id_list) = $tsunami->selectAll("format_conversions", "DISTINCT dataset_id", "source_format_id=$src_format ORDER BY dataset_id");
#Check for MySQL errors
dielog("MySql error: $msg\n") unless $msg eq "";
dielog("No Datasets for with source_format_id=$src_format\n") unless $tsunami->getRows() > 0;

#Create logs
open LOG, ">>commands.log" or die "Unable to create commands.log\n";
open LOGI, ">>inserts.log" or die "Unable to create inserts.log\n";

#Loop through all the dataset_ids
my $count = 0;
foreach (@dataset_id_list) {
	$count++;

	if ($count < $startAtDataset) { next; }

	#Create array which will contain all the info needed to insert the converted files later
	my @file = ();

	#$$_[0] is ugly store it in a readable variable
	my $dataset_id = $$_[0];

	#skip test datasets (Datasets which start with 999)
	if ($dataset_id =~ /999\./) { next; }

	#print progress message so that the user knows progress is being made
	print "\rDataset: $dataset_id ($count/" . ($#dataset_id_list+1) . ")                                      ";
	print LOG "Dataset: $dataset_id ($count/" . ($#dataset_id_list+1) . ")\n"; #Print it to the log too

	#Select all ebufr files from current dataset
	($msg, my %file) = $tsunami->selectFull("file", "file_id,directory,filename,begin_date,end_date,event", "dataset_id='$dataset_id' AND format_id=$src_format");

	#Check for MySql Errors
	dielog("MySql error: $msg\n") unless $msg eq "";
	unless ($tsunami->getRows() > 0) {
		printerr( "No files found for dataset $dataset_id where format_id=$src_format\n");
		next;
	}

	#Create WHERE statement for use on the format_conversions table (We want to go from ebufr to one of the three ASCII file types not NetCDF or anything else)
	my $where = "dataset_id=$dataset_id AND source_format_id=$src_format AND (target_format_id=$dest_format[0]";
	for (my $i = 1; $i <= $#dest_format; $i++) {
		$where .= " OR target_format_id=$dest_format[$i]";
	}
	$where .= ")";

	#Select correct ebufr->ASCII conversion command from format_conversion table
	($msg, my @command) = $tsunami->selectAll("format_conversions", "command,target_format_id", $where);

	#Sql error check and we only want ONE matching command
	dielog("MySql error: $msg\n") unless $msg eq "";
	dielog("Dataset $dataset_id has to many matching conversion commands!\n") if $#command > 0;
	if ($#command < 0) { printerr( "Dataset $dataset_id has no matching conversion commands!\n"); next; }

	#Loop through all of the ebufr files for the current dataset
	for (my $i = 0; $i <= $#{$file{"row"}}; $i++) {
		#So the user know we are still going
		print "\rDataset: $dataset_id ($count/" . ($#dataset_id_list+1) . ") File: " . $file{"row"}[$i][2] . " (" . ($i+1) . "/" . ($#{$file{"row"}}+1) . ")                            ";
		print LOG "Dataset: $dataset_id ($count/" . ($#dataset_id_list+1) . ") File: " . $file{"row"}[$i][2] . " (" . ($i+1) . "/" . ($#{$file{"row"}}+1) . ")\n";

		#Full path of ebufr file
		my $inpath = $file{"row"}[$i][1] . "/" . $file{"row"}[$i][2];

		#Generate ASCII filename or die if we couldn't figure one out
		my $outfile = $file{"row"}[$i][2];
		my $n = 0;
		if ($command[0][0] =~ /ebqcf/) {
		    $n=$outfile=~ s/\.ebufr/\.qcf/;
		}
		elsif ($command[0][0] =~ /ep2pq/) {
		    $n=$outfile=~ s/\.ebufr/\.pqcf/;
		}
		elsif ($command[0][0] =~ /ebscf/) {
		    $n=$outfile=~ s/\.ebufr/\.scf/;
		}
		else { dielog("Unknown conversion '$command[0][0]' unable to determine output file name!\n"); }
		unless ($n == 1) { dielog("Unable to generate outfilename from " . $file{"row"}[$i][2] . "\n"); }

		#Path of output ASCII file
		my $outdir; #Just the directory in here (Used for inserting into CODIAC later)
		my $outpath; #Full path of ASCII file in here
		if ($testing) { #SHADOW DIR (/net/work/Projects/ebufconversion)
		    #Make Path for ASCII file (need for shadow directory)
		    mkpath("/net/work/Projects/ebufconversion" . $file{"row"}[$i][1]) unless (-d ("/net/work/Projects/ebufconversion" . $file{"row"}[$i][1]));

		    $outdir = "/net/work/Projects/ebufconversion" . $file{"row"}[$i][1]; ##SHADOW##
		    $outpath = "/net/work/Projects/ebufconversion" . $file{"row"}[$i][1] . "/$outfile"; ##SHADOW##
		}
		else #NO SHADOW
		{
		    $outdir = $file{"row"}[$i][1];
		    $outpath = $file{"row"}[$i][1] . "/$outfile";
		}
	
		#There are some files in CODIAC which do not exist... (71.106 IM LOOKING AT YOU)
		#Warn the user about it
		unless (-e $inpath) {
			printerr("Dataset: $dataset_id\nWARNING: $inpath does not exist!\n");
			next;
		}

		#Conversion commands will fail if the target file exists
		#So skip the conversion if the file exists
		#We are assuming here the the file that is already there was converted correctly
		if (-e $outpath) { printerr("Dataset: $dataset_id\nWARNING: $outpath already exists!\n"); } #Warn the user
		else {
			#Run conversion command (Ebufr->ASCII)
			print LOG "/usr/local/codiac/bin/$command[0][0] $inpath $outpath\n";
			my $output = `/usr/local/codiac/bin/$command[0][0] $inpath $outpath 2>&1`;
			print LOG "$output\n";

			#Exit code of conversion commands is always 0.. Not useful for error checking

			#Fix file permissions on the new ASCII file
			`chmod 664 $outpath`;
			`chgrp ctm-dmg $outpath`;
		}

		#Check and make sure the new ASCII file actually exists, die if it does not
		unless (-e $outpath || (-s $outpath) == 0) { dielog("Output file $outpath was not created or has zero file size!\n"); }

		#Check file size (Should be much larger at least 1.5X)
		#Some ebufer files seem to have internal duplication leading to artificially large file sizes so the ASCII file may be smaller
		#So we dont want to die on this, instad warn the user and log it in a seperate log for manual investigation.
		unless ((-s $outpath) > (1.5*(-s $inpath))) { 
		    printerr("Dataset $dataset_id: Output filesize < 1.5*(input filesize) possible dataloss!\n Inputfile: $inpath\n Outputfile: $outpath\n");
		    open SIZELOG, ">>SizeErrors.log";
		    print SIZELOG "Dataset: $dataset_id:\n Inputfile: $inpath (" . (-s $inpath) . ")\n Outputfile: $outpath (" . (-s $outpath) . ")\n\n";
		    close SIZELOG;
		}

		#Make sure the output files dates are similar to the dates is CODIAC (Will die on major differences, warn on lesser diffs)
		chkDate($outpath, $file{"row"}[$i][3], $file{"row"}[$i][4], $dataset_id);

		#Push information about new ASCII file onto the file array to be inserted later
		push @file, [$dataset_id, $file{"row"}[$i][3], $file{"row"}[$i][4], $command[0][1], $file{"row"}[$i][5], $outdir, $outfile];
		
		if ($testing) { last; } #Only do the first file of each dataset when testing
	}
	print LOG "\n"; #Makes log more readable

	#Insert files for the current dataset
	for (my $i = 0; $i <= $#file; $i++) {

	    	#Create a MySql file and populate with info from file array
		my $mysqlfile = MySqlFile->new();
		$mysqlfile->setDatasetId($file[$i][0]);
		$mysqlfile->setHost("localhost");
		$mysqlfile->setBeginDate(split(/[ :-]/, $file[$i][1]));
		$mysqlfile->setEndDate(split(/[ :-]/, $file[$i][2]));
		$mysqlfile->setFormatId($file[$i][3]);
		$mysqlfile->setPurpose("data");
		$mysqlfile->setFile($file[$i][5], $file[$i][6]);

		#John says hide the ASCII files
		$mysqlfile->{"hide"} = 1;

		#Insert the new ASCII file!
		print LOGI "Insert: $file[$i][5]/$file[$i][6] -> $file[$i][0] ($file[$i][1],$file[$i][2],$file[$i][3])\n";
		$msg = $mysqlfile->insert($merlot);

		#Check for MySQL errors
		dielog("MySql error: $msg\n") unless $msg eq "";
	}

	#Commit the changes (ASCII file inserts) unless we are only testing
	#unless ($testing) { if ($msg eq "") { $merlot->commit(); } }##TESTING##
	if ($msg eq "") { $merlot->commit(); }

	#Clear file array
	@file = ();

	#Stop after the first couple datasets
	#if ($count >= 2) { last; } #FOR TESTING PURPOSES THIS WILL END THE LOOP AFTER A FEW DATASETS

}

#Cleanup and exit
close LOG;
close LOGI;
$tsunami->rollback();
$tsunami->disconnect();
$merlot->rollback();
$merlot->disconnect();
print "\nComplete!\n";
exit(0);

sub chkDate { #Used to compare ASCII file date/time against CODIAC date/time

	my $file = shift; #ASCII filename
	my @begin_date = split(/[ :-]/, shift); #MySQL format: YYYY-MM-DD hh:mm:ss
	my @end_date = split(/[ :-]/, shift); #MySQL format: YYYY-MM-DD hh:mm:ss
	my $dataset_id = shift; #Used for error reporting

	my $chk_depth = 4; #0 = year only, 1 = year,month, 2 = year,month,day etc.

	#Open the ASCII file init date/time arrays [0]=year [1]=month [2]=day [3]=hour [4]=minute [5]=second
	open FILE, $file;
	my @file_begin_date = ();
	my @file_end_date = ();
	my $chk = 0; #set to one when we find a date in the ASCII file
	while (<FILE>) {
		my @date = ($_ =~ /(\d\d\d\d)\/(\d\d)\/?(\d\d)?.?(\d\d)?:?(\d\d)?:?(\d\d)?/); #Usual format YYYY/MM/DD hh:mm:ss (sometimes lacking the Day, and or HMS)

		if ($#date == -1) { @date = ($_ =~ /Nominal \w+ Time \(y,m,d,h,m,s\):(\d\d\d\d), (\d\d), (\d\d), (\d\d):(\d\d):(\d\d)/); } #Format used in some sounding data
		
		#Perl will put empty strings in the array for matches groups which were not used
		#Splice them out so that $#date gives the number of elements sucessfully captured
		for (my $a = 0; $a <= $#date; $a++) {
			unless (defined $date[$a] && $date[$a] ne "") { splice(@date, $a, 1); $a--; }
		}		


		if ($#date == 5) { #if $#date == 5 then YMDHMS were all captured
			$chk = 1; #We found a date

			#If begin date is not set, set it
			if ($#file_begin_date == -1) { @file_begin_date = @date; }

			#Set/update end date
			@file_end_date = @date;
		}
		elsif ($#date == 1) { #If $#date == 1 then we only got the year and month, files are therefor monthy files
			$chk = 1; #Found a date

			#Since monthy files are loaded into CODIAC in a number of ways only check the year and month against codiac
			$chk_depth = 1;

			#Days in each month, used to generate day
			my @daysInMonth = (31,daysInFeb($date[0]),31,30,31,30,31,31,30,31,30,31);
			
			#If begin time is not set, set it
			if ($#file_begin_date == -1) { @file_begin_date = (@date, "01", "00", "00", "00"); }

			#Set/update end time
			@file_end_date = (@date, $daysInMonth[$date[1]-1], "23", "59", "59");
		}
	}
	close FILE;
	unless ($chk) { dielog("Unable to parse Date of $file\n"); } #Didn't find a date in the ASCII file, die

	#Make sure the begin and end dates make some sense
	unless(isValidDate(@file_begin_date)) { 
		dielog("Dataset: $dataset_id\nFile: $file\n" .
			"Invalid Date: $file_begin_date[0]-$file_begin_date[1]-$file_begin_date[2] $file_begin_date[3]:$file_begin_date[4]:$file_begin_date[5]\n\n");
	}
	unless(isValidDate(@file_end_date)) { 
		dielog("Dataset: $dataset_id\nFile: $file\n" .
			"Invalid Date: $file_end_date[0]-$file_end_date[1]-$file_end_date[2] $file_end_date[3]:$file_end_date[4]:$file_end_date[5]\n");
	}

	#WORKAROUNDS

	#If CODIAC has 00:00:00 - 23:59:59 dont both checking the time, the loader made that up
	if ($begin_date[3] ==0 && $begin_date[4] == 0 && $begin_date[5] == 0 &&
	    $end_date[3] == 23 && $end_date[4] == 59 && $end_date[5] == 59)
	{
		$chk_depth = 2;
	}

	
	##if end time in CODIAC is 23:59:59 AND the file end day is 1 greater than the CODIAC end day dont worry about it
	#if ($end_date[3] == 23 && $end_date[4] == 59 && $end_date[5] == 59 && $file_begin_date[2] == $begin_date[2] && $file_end_date[2]-$end_date[2] == 1) {
    #    $chk_depth = 1;
	#}

	#The dates for Month long files are EXTREMELY INCONSISTENT
	#If we run into them ONLY COMPARE YEAR AND MONTH

	#Check if CODIAC says the files are monthy (Begin and end date correspond the the begin and end date of a single month of a single year)
	my @daysInMonth = (31,daysInFeb($end_date[0]),31,30,31,30,31,31,30,31,30,31);
	if ($begin_date[0] == $end_date[0] && $begin_date[1] == $end_date[1] && $begin_date[2] == 01 && $end_date[2] == $daysInMonth[$end_date[1]-1])
	{
		$chk_depth = 1;
	}

	#Check if the file says it is monthy (Begin and end date correspond the the begin and end date of a single month of a single year)
	@daysInMonth = (31,daysInFeb($file_end_date[0]),31,30,31,30,31,31,30,31,30,31);
	if ($file_begin_date[0] == $file_end_date[0] && $file_begin_date[1] == $file_end_date[1] && $file_begin_date[2] == 01 && $file_end_date[2] == $daysInMonth[$file_end_date[1]-1])
	{
		$chk_depth = 1;
	}

	#END WORKAROUNDS

	#Check File Begin/End datetimes against CODIAC Begin/End datetimes
	#loop through date/time elements from 0->$chk_depth (0=year,1=month,2=day,3=hour,4=minute,5=second)
	for (my $i = 0; $i <= $chk_depth; $i++) { 

		#Compare Begin datetimes (CODIAC vs ASCII file)
		if ($file_begin_date[$i] ne $begin_date[$i]) {
			if ($i <= -1) { #MAJOR MISMATCH (year,month,day) differs, die
				dielog("Dataset: $dataset_id\nFile: $file\n" .
					"Begin Date Mismatch: \n" . 
				       	" File: $file_begin_date[0]-$file_begin_date[1]-$file_begin_date[2] $file_begin_date[3]:$file_begin_date[4]:$file_begin_date[5] \n" .
					" CODIAC: $begin_date[0]-$begin_date[1]-$begin_date[2] $begin_date[3]:$begin_date[4]:$begin_date[5]\n\n", "DateErrors.log");
			}
			else { #Minor mismatch (hour,minute,second) differs, warn
				printerr("Dataset: $dataset_id\nFile: $file\n" .
					"Begin Date Mismatch: \n" . 
					" File: $file_begin_date[0]-$file_begin_date[1]-$file_begin_date[2] $file_begin_date[3]:$file_begin_date[4]:$file_begin_date[5] \n" .
					" CODIAC: $begin_date[0]-$begin_date[1]-$begin_date[2] $begin_date[3]:$begin_date[4]:$begin_date[5]\n\n", "DateErrors.log");
				last;
			}
		}

		#Compare End datetimes (CODIAC vs ASCII file)
		if ($file_end_date[$i] ne $end_date[$i]) {
			if ($i <= -1) { #MAJOR MISMATCH (year,month,day) differs, die
				dielog("Dataset: $dataset_id\nFile: $file\n" .
					"End Date Mismatch: \n" . 
					" File: $file_end_date[0]-$file_end_date[1]-$file_end_date[2] $file_end_date[3]:$file_end_date[4]:$file_end_date[5] \n" .
					" CODIAC: $end_date[0]-$end_date[1]-$end_date[2] $end_date[3]:$end_date[4]:$end_date[5]\n\n", "DateErrors.log");
			}
			else { #Minor mismatch (hour,minute,second) differs, warn
				printerr("Dataset: $dataset_id\nFile: $file\n" .
					"End Date Mismatch: \n" . 
					" File: $file_end_date[0]-$file_end_date[1]-$file_end_date[2] $file_end_date[3]:$file_end_date[4]:$file_end_date[5] \n" .
					" CODIAC: $end_date[0]-$end_date[1]-$end_date[2] $end_date[3]:$end_date[4]:$end_date[5]\n\n", "DateErrors.log");
				last;
			}
		}
	}
}

sub isValidDate {
	if ($_[0] > 2012 || $_[0] < 1950) { return 0; } #Year should not be in the future nor before 1950
	if ($_[1] > 12 || $_[1] < 0) { return 0; } #Months go from 0 to 12
	if ($_[2] > 31 || $_[2] < 0) { return 0; } #Days go from 0 to 31 (not worrying about 30, 29 and 28 day months)
	if ($_[3] > 23 || $_[3] < 0) { return 0; } #Hours go from 0 to 23
	if ($_[4] > 60 || $_[4] < 0) { return 0; } #minutes go from 0 to 60
	if ($_[5] > 60 || $_[5] < 0) { return 0; } #So do seconds
	return 1;
}

sub daysInFeb {
    if (scalar(@_) != 1) {
	die("Invalid parameters to daysInFeb\n");
    }

    if ((($_[0] % 4 == 0) && ($_[0] % 100 != 0)) || ($_[0] % 400 == 0)) { #leap years are divisible by 4 and not 100 unless they are divisible 400
	return 29;
    } else {
	return 28;
    }
}

sub dielog {
	my $line = shift; #aparently open writes to $_ so we need to store this now
	my $err_file = shift; #Custom error file may be specified

	#Open Errors.log or $err_file if defined
	if (defined $err_file) { open ERRLOG, ">>$err_file" or die "Unable to create $err_file\n"; }
	else { open ERRLOG, ">>Errors.log" or die "Unable to create Errors.log\n"; }

	#print to error log (die will print to console)
	print ERRLOG "$line\n";

	#Cleanup
	close ERRLOG;

	die "\n$line";
}

sub printerr {
	my $line = shift; #aparently open writes to $_
	my $err_file = shift; #Custom error file may be specified

	#Open Errors.log or $err_file is defined
	if (defined $err_file) { open ERRLOG, ">>$err_file" or die "Unable to create $err_file\n"; }
	else { open ERRLOG, ">>Errors.log" or die "Unable to create Errors.log\n"; }

	#Print message to console and log
	print STDERR "\n$line\n";
	print ERRLOG "$line\n";

	#Cleanup
	close ERRLOG;
}
