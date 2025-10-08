#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>This script updates the database for GOES satellite data that is 
# automatically put on the mass store system.  It updates the following 
# datasets<ul>
#   <li>100.003- GOES-12 1KM Multi-spectral TDF Data</li>
#   <li>100.004- GOES-12 4KM Multi-spectral TDF Data</li>
#   <li>100.005- GOES-12 Sounder</li>
#   <li>100.006- GOES-10 Sounder</li>
# </ul></p>
#
# @warning This script needs to be run on tsunami as the JOSS user so it
# can update zedi.
#
# @author Janine Goldstein
# @version 2.3 Change mysql lib path to /net/work/lib/perl/mysql pursuant
# to upgrade of codiac db to zedi8.
#
# @author Joel Clawson
# @version 2.2 Updated the script to use the ingest user on the zedi database
# on tsunami.eol.  Changed the email to come from the joss@eol user and reply-to
# to be me (Joel).
#
# @author Joel Clawson
# @version 2.1 Update to Vertion5 of the modules and migrated from hurricane.joss
# to tsunaim.eol.
#
# @author Joel Clawson
# @version 2.0 Updated to MySQL and Version3 of the conversion modules.
# 
# @author Joel Clawson
# @version 1.0 Original Creation
##Module--------------------------------------------------------------------
use strict;
use lib "/net/work/software/conversion_modules/Version5";
use lib "/net/work/lib/perl/mysql";
use DpgDate qw(:DEFAULT);
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;

&main();

##--------------------------------------------------------------------------
# @signature void main()
# <p>This controls the flow of the script.</p>
##--------------------------------------------------------------------------
sub main {
    my %storms;
    loadStorm(\%storms, "100.003", "G13", "1KM");
    loadStorm(\%storms, "100.004", "G13", "4KM");
#    loadStorm(\%storms, "100.005", "G12", "SOUNDER");
#    loadStorm(\%storms, "100.006", "G10", "SOUNDER");

    # Load all of the dates between last week and today into a hash
    my %dates;
    my $cur_date = loadLastWeek();
    my $today = loadToday();
    while (compareDates($cur_date,"YYYY/MM/DD",$today,"YYYY/MM/DD") > -1) {
	my @date = split('\/', $cur_date);
	$dates{$cur_date}->{"Year"} = $date[0];
	$dates{$cur_date}->{"Date"} = $cur_date;
	$dates{$cur_date}->{"Julian"} = 
	    sprintf("%04d%03d",$date[0],convertJulian($date[0], $date[2], $date[1]));
	$cur_date = (adjustDateTime($cur_date,"YYYY/MM/DD","000000","HHMMSS",1,0,0,0))[0];
    }

    my $report = "";
    #my $database = MySqlDatabase->new("joss","ofps-345");
    my $database = MySqlDatabase->new("ingest","gob-ble");
    $database->connect();

    # Loop through all of the storms in the hash
    foreach my $key (keys %storms) {
	foreach my $date (sort keys %dates) {
	
	    my $mss_dir = sprintf("%s/%s/%s", $storms{$key}->{"MSS"}, 
				  $dates{$date}->{"Year"}, 
				  $dates{$date}->{"Julian"});
	    my @files = `/net/local_lnx/dcs/bin/msls -l $mss_dir`;

	    # Loop through all of the files in the directory
	    foreach my $file (@files) {

		if ($file !~ /^total/) {
		    chomp($file);
		    my @file_info = split(' ', $file);

		    # Determine if the file is already in the database.
		    my $in_db = $database->select("file","*",sprintf("directory='%s' and filename='%s' and dataset_id=(select id from dataset where archive_ident='%s')",$mss_dir,$file_info[8],$key));

		    # Only insert if it isn't in the database
		    if (!$in_db) {
			
			# Create the file.
			my $hour = substr($file_info[8], 12, 2);
			my $min = substr($file_info[8], 14, 2);
			my $mysql = MySqlMSSFile->new();
			$mysql->setDatasetArchiveIdent($key);
			$mysql->setFile($mss_dir,$file_info[8]);
			$mysql->setFormatId(55);
			$mysql->setBeginDate(split(/\//,$dates{$date}->{"Date"}),$hour,$min,0);
			$mysql->setEndDate(split(/\//,$dates{$date}->{"Date"}),$hour,$min,59);

			# Insert the File
			my $msg = $mysql->insert($database);
			
#			printf("\t%s %s %s %02d %05d %s/%s\n",$mysql->getDatasetId(),$mysql->getBeginDate(),$mysql->getEndDate(),$mysql->getFormatId(),$mysql->getSize(),$mysql->getDirectory(),$mysql->getFilename()); 

			# Only update the dataset if the file was entered successfully.
			if ($msg eq "") {

			    # Create and load the dataset.
			    my $dataset = MySqlDataset->new($key);
			    $msg = $dataset->selectDataset($database);

			    # Only continue if the dataset was retreived successfully.
			    if ($msg eq "") {
				
				# Load and format dates
				my $file_begin = $mysql->getBeginDate(); $file_begin =~ s/[\s:-]//g;
				my $file_end = $mysql->getEndDate(); $file_end =~ s/[\s:-]//g;
				my $begin = $dataset->getBeginDate(); $begin =~ s/[\s:-]//g;
				my $end = $dataset->getEndDate(); $end =~ s/[\s:-]//g;
				
				# Compare the dates and change as necessary.
				if ($file_begin < $begin) {
				    $dataset->setBeginDate(split(/[\s:-]/,$mysql->getBeginDate())); 
				}
				if ($file_end > $end) {
				    $dataset->setEndDate(split(/[\s:-]/,$mysql->getEndDate())); 
				}

				# Update the dataset with the new dates.
				$msg = $dataset->updateDataset($database);
			    }
			}
			
			# Only commit if an error has not occured, otherwise roll back.
			if ($msg eq "") { $msg = $database->commit(); }
			else { $report .= $msg."  Database rolled back.\n".$database->rollback(); }
		    }
		}
	    }
	}
    }

    # Always cleanly disconnect.
    $database->disconnect();

    # Send the message report.
    if ($report ne "") { sendMail($report); }
    #else { sendMail("There were no errors to report.\n"); }
}

##----------------------------------------------------------------------------
# @signature String loadLastWeek()
# <p>Get the date that was one week ago from today.</p>
#
# @output $date The date in "YYYY/MM/DD" format that was one week ago.
##--------------------------------------------------------------------------- 
sub loadLastWeek {
    my ($day, $month, $year) = (localtime())[3..5];

    return (adjustDateTime(sprintf("%04d/%02d/%02d",$year+1900,$month+1,$day),"YYYY/MM/DD",
			   "000000","HHMMSS",-7,0,0,0))[0];
}


##--------------------------------------------------------------------------
# @signature void loadStorm(Hash storms, String id, String sat, String dist)
# <p>Add a storm to process to the Hash of storms.</p>
#
# @param storms This is a reference to the Hash of storms to be processed.
# @param id The storm id of the storm to be added.
# @param sat The number of the GOES satellite to process.
# @param dist The distance of the satellite.
##--------------------------------------------------------------------------
sub loadStorm {
    my $storms = shift;
    my $id = shift;
    my $sat = shift;
    my $dist = shift;
    $$storms{$id}->{"MSS"} = sprintf("/JOSS/DATA/RAW/SATELLITE/GOES/%s/%s",$sat,$dist);
}

##---------------------------------------------------------------------------
# @signature (String year, String date, String julian) loadToday()
# <p>Get today's date from the system and parse them into useable forms.</p>
#
# @return $date Today's date in YYYY/MM/DD format
##---------------------------------------------------------------------------
sub loadToday {
    my ($day, $month, $year) = (localtime())[3..5];
    $month += 1;
    $year += 1900;
    return sprintf("%04d/%02d/%02d", $year, $month, $day);
}

##---------------------------------------------------------------------------
# @signature void sendMail(String msg)
# <p>This sends an email message to the recipients.</p>
#
# @input msg The message to send in the email.
##---------------------------------------------------------------------------
sub sendMail {
    my $body = $_[0];

    open( SENDMAIL, "|/usr/lib/sendmail -t" ) ||
        die "Unable to open sendmail";

    print SENDMAIL "Reply-To: janine\@eol.ucar.edu\n";
    print SENDMAIL "Subject: GOES Satellite CODIAC MySQL Script Error\n";
    print SENDMAIL "To: pmartin\@eol.ucar.edu janine\@eol.ucar.edu\n\n\n";
    print SENDMAIL "Automated Script:\n$body\n\n\n";
    close(SENDMAIL);
}






