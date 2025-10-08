#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
## <p>The download_mrms_data.pl script is a twice daily cron script that does the following
## tasks:</p>
## <ol>
##   <li>Download radar data from the National Center for Environmental 
##   Prediction (NCEP) Multi-Radar/Multi-Sensor System (MRMS) ftp site.  This script
##   was created for the WINTRE-MIX project, so the data can be archived after
##   the project is over. The script downloads data from multiple directories
##   on the ftp site and saves a log file of the downloads.  If the data file
##   has already been downloaded, then this file is skipped.  There is a wait 
##   time of 1 second between each wget request, so the ftp server does not
##   get overloaded.
## </ol>
##
## @author Janet Scannell January 2022
##
##
##  Change the cron script so it runs every 8 hours (3 times/day).
##  Change the reporting to only send the errors after all the downloads have 
##  completed.  If one of the 6 sets of downloads gets an error, it won't affect
##  the other downloads.
##  Change the download so every product, except for MergedBaseReflectivity, is
##  only downloaded for files on the hour and on the half hour.
##
##  @author J. Scannell February 2022
##
###Module-----------------------------------------------------------------------
#
use strict;
use lib "/net/work/lib/perl/mail";
use DateTime;
use MAIL;

my $FTP_SITE = "https://mrms.ncep.noaa.gov/";
my @FTP_DIR;
$FTP_DIR[0] = "data/2D/MergedBaseReflectivity/";
$FTP_DIR[1] = "data/2D/LowLevelCompositeReflectivity/";
$FTP_DIR[2] = "data/3DRhoHV/MergedRhoHV_01.00/";
$FTP_DIR[3] = "data/3DRhoHV/MergedRhoHV_02.00/";
$FTP_DIR[4] = "data/3DZdr/MergedZdr_01.00/";
$FTP_DIR[5] = "data/3DZdr/MergedZdr_02.00/";
my $DOWNLOAD_DIR = "/scr/dmgtmp/cully/projects/wintre-mix/mrms";
my @monitors = ("eol-cds-ingest\@ucar.edu");
my $report = "";

&main();

sub main {
    my $dt = DateTime->now;
    my $ymd = $dt->ymd;
    my $hour = $dt->hour;
    chdir("$DOWNLOAD_DIR");
    my $logfile = sprintf("%s/logs/log-%s-%02d", $DOWNLOAD_DIR, $ymd, $hour);
# Get all the data for the Merged product.
    my $wgetsite = $FTP_SITE . $FTP_DIR[0];
    my $result = system("wget -N -w 1 -nH -np -r -l 1 -R 'index.html*,*latest*' -a $logfile $wgetsite");
    if ($result != 0) {
       $report .= sprintf("There was an error downloading the WINTRE-MIX MRMS data for date: %s hour: %02d.\n  Dataset being downloaded: %s.\n Return code from wget: %d\n\n\n",
          $ymd,$hour,$FTP_DIR[0],$result/256);
    } 
# Only get data on the hour and half hour for the other products.
    for (my $i = 1; $i <= $#FTP_DIR; $i++) { 
       $wgetsite = $FTP_SITE . $FTP_DIR[$i];
       $result = system("wget -N -w 1 -nH -np -r -l 1 -A '*-[012][0-9][03][0]*' -a $logfile $wgetsite"); 
       if ($result != 0) {
          $report .= sprintf("There was an error downloading the WINTRE-MIX MRMS data for date: %s hour: %02d.\n  Dataset being downloaded: %s.\n Return code from wget: %d\n\n\n",
		          $ymd,$hour,$FTP_DIR[$i],$result/256);
       } 
    }
    if ($report ne "") {
       sendMailAndDie($report);
    }
#    sendMailAndDie(sprintf("The WINTRE-MIX MRMS data for date: %s hour: %02d was successfully downloaded.\n Return code from wget: %d\n",
#		          $ymd,$hour,$result));
}

sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("WINTRE_MIX MRMS - ERROR",$0."\n\n".$body, @monitors);
    exit(1);
}
