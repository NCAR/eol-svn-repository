#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
## <p>The download_ncep-emc_data script is a daily cron script that does the following
## tasks:</p>
## <ol>
##   <li>Download precipitation data from the National Center for Environmental 
##   Prediction (NCEP) Environment Modeling Center (EMC) NOMADS ftp site.  This data
##   is stored on our server until the monthly process is run to archive the data.
## </ol>
##
## @author Janet Scannell December 5, 2018
##
##  Added check to script for the number of files downloaded.  JNS 2/13/19
##  Revised check to account for changes in number of files. JNS Aug 2020
##  Added check for last date processed.  If a date has been missed, because
##     of computer downtime, will send email.  This change needs the file
##     DO_NOT_DELETE_last_process_date to be moved to the directory:
##     /net/work/operational/precip/NCEP_EMC/ingest-NOMADS - JNS Nov 2020
##
##  Added wait time between requests in wget; Changed FTP_SITE to https - JNS - Apr 2021
##
###Module-----------------------------------------------------------------------
#
use strict;
use lib "/net/work/lib/perl/mail";
use DateTime;
use MAIL;

my $FTP_SITE = "https://nomads.ncep.noaa.gov/";
my $FTP_DIR = "pub/data/nccf/com/pcpanl/prod/";
my $DOWNLOAD_DIR = "/net/work/operational/precip/NCEP_EMC/ingest-NOMADS/";
my $last_process_date_file = "$DOWNLOAD_DIR/DO_NOT_DELETE_last_process_date";
my $running_file = "$DOWNLOAD_DIR/DO_NOT_DELETE_running";
my @monitors = ("eol-cds-ingest\@ucar.edu");
# my @monitors = ("echohawk\@ucar.edu,anstett\@ucar.edu");

&main();

sub main {
    my $dt = DateTime->now;
    my $day_to_download = $dt - DateTime::Duration->new( days => 9 );
    my $year = $day_to_download->year;
    my $month = sprintf("%02d",$day_to_download->month);
    my $day = sprintf("%02d",$day_to_download->day);
    my $last_process_date = $dt - DateTime::Duration->new( days => 10 );
    my $last_process_year = $last_process_date->year;
    my $last_process_month = sprintf("%02d",$last_process_date->month);
    my $last_process_day = sprintf("%02d",$last_process_date->day);
    my $check_process_day = $last_process_year . $last_process_month . $last_process_day;
# Save current date to $running_file, then at the end move $running_file to $last_process_date
    if (! open (RUNFILE, ">$running_file")) {
       sendMailAndDie(sprintf("Cannot create $running_file.\n"));
    }
    print RUNFILE $year . $month . $day;
    close(RUNFILE);
# Check if the last date processed is the previous day to the day that is being processed
    if (! open (PROCESSFILE, "<$last_process_date_file")) {
       sendMailAndDie(sprintf("Cannot open $last_process_date_file.\n"));
    }
    my @process_file = <PROCESSFILE>;
    close(PROCESSFILE);
    if ($process_file[0] ne $check_process_day) {
       sendMail(sprintf("Ingest possibly missed a day of data. - Date last processed: %s\n", $process_file[0]));
    }

    if (! (-e "$DOWNLOAD_DIR/$year")) {
       mkdir("$DOWNLOAD_DIR/$year");
    }
    if (! (-e "$DOWNLOAD_DIR/$year/$month")) {
       mkdir("$DOWNLOAD_DIR/$year/$month");
    }
    chdir("$DOWNLOAD_DIR/$year/$month");
    my $wgetsite = $FTP_SITE . $FTP_DIR . "pcpanl." . $year . $month . $day . "/";
    my $logfile = $DOWNLOAD_DIR . "log." . $year . $month . $day;
    my $result = system("wget -w 1 -nd -np -r -l 1 -R 'index.html*','pcpanl*' -o $logfile $wgetsite"); 

    if ($result != 0) {
	sendMailAndDie(sprintf("There was an error downloading the NCEP-EMC precip data for %02d %02d %04d.\n  Return code from wget: %d\n",
		          $month,$day,$year,$result/256));
#    } else {
#	sendMailAndDie(sprintf("The NCEP-EMC precip data for %02d %02d %04d was successfully downloaded.\n Return code from wget: %d\n",
#		          $month,$day,$year,$result));
    }
    my ($text, $filesdownloaded) = split(' ', qx(grep Downloaded $logfile));
# Move $running_file to $last_process_date.
    system("mv $running_file $last_process_date_file");
#
# As of 2/13/19 there should be 387 data files saved each day, with 1 file being discarded by wget (index.html)
#
#
# As of 7/29/20 there should be 126 data files saved each day, with 1 file being discarded by wget (index.html)
#
    if ($filesdownloaded < 127) {
	sendMailAndDie(sprintf("There was an error downloading the NCEP-EMC precip data for %02d %02d %04d.\n  Number of files downloaded was: %d Should be: 127\n",
                          $month,$day,$year,$filesdownloaded));
    }
}

sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("NCEP-EMC precip- ERROR",$0."\n\n".$body, @monitors);
    exit(1);
}

sub sendMail {
    my ($body) = @_;
    MAIL::send_mail("NCEP-EMC precip- MISSING DATA",$0."\n\n".$body, @monitors);
}
