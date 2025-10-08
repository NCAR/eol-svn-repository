#!/usr/bin/perl

$| = 1;

use Net::FTP;

# This script uses wget to retrieve the NWS RRS BUFR radiosonde data from NCEI for archival.
 
#$ftp_server = "catalog.eol.ucar.edu";
#$ftp_dir = "pub/incoming/catalog/we-can";

#$ftp_server = "ftp.eol.ucar.edu";
#$ftp_dir = "pub/data/incoming/itop";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime(time());
$mon++; # zero-based
$year = $year + 1900;

$wget = "/usr/bin/wget";
# $ENV{'WGETRC'}="/h/eol/loehrer/cron/wgetrc.robots";
# use -e robots=off instead

$date=sprintf("%04d%02d%02d",$year,$mon,$mday);
$datehour=sprintf("%04d%02d%02d%02d",$year,$mon,$mday,$hour);

$FILE_DIR[0] = "https://www1.ncdc.noaa.gov/pub/data/igra/v1/related/DataRequest/NCAR/";
 
# $TEMPDIR[0] = "/h/eol/echohawk/crontest";
$TEMPDIR[0] = "/net/ftp/pub/data/incoming/ncdc_soundings";

if ($ftp_server) {
   $ftp = Net::FTP->new($ftp_server) || die "Unable to connect to $ftp_server, $@\n";
   $ftp->login() || die "Unable to anonymously login to $ftp_server, $ftp->message\n";
   $ftp->binary();
   $ftp->cwd($ftp_dir) || die "Unable to cd to appropriate directory $ftp_dir, $ftp->message\n";

}

$num = 0;

for($i = 0; $i <= $num; $i++) {

chdir($TEMPDIR[$i]) || die "Couldn't reach $TEMPDIR[$i]: $!";

  open(WGET, "$wget -q -N -r -nH --cut-dirs=8 --no-parent -e robots=off --reject \"index.html*\"  $FILE_DIR[$i] 2>&1 |") || die "unable to open wget";
  while(<WGET>) {
     print $_;
     if(/saved/) {
        print "New update found at $hour:$min\n";

     } elsif(/not retrieving/) {
        print "File has not been updated - exiting\n";
     }
  }
}
