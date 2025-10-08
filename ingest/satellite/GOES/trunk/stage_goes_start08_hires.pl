#!/usr/bin/perl

use Fcntl;
use POSIX qw(tmpnam strftime);

$TAR="/bin/tar";
$RM="/bin/rm";
$MSRCP="/net/local_lnx/bin/msrcp -wpwd jossdata -pe 32767 -overwrite never";
$MSLS="/net/local_lnx/bin/msls";

$log="/h/eol/joss/goes/g12_start08_hi_log";
$srcdir="/satellite/live/G12/start08/HighRes";
#$tmpdir="/codiac_tmp/joss";
#$tmpdir="/work/joss/GOES_rainex_hi/tmp";
$destdir="/JOSS/DATA/RAW/BY_PROJECT/START08/G12/1KM";

do { $name = tmpnam() } until sysopen(FH, $name, O_RDWR|O_CREAT|O_EXCL);

#open STDOUT, ">&FH" or die "Couldn't redirect stdout, $!";
#open STDERR, ">&STDOUT" or die "Couldn't dup stdout, $!";
select STDERR; $|=1;
select STDOUT; $|=1;


chdir($srcdir) or die "Couldn't reach $srcdir: $!";
opendir(DIR,'.') or die "Couldn't open $srcdir: $!";
@files=sort(readdir(DIR));
closedir(DIR);

foreach $file (@files) {
   next unless -f $file;
   next unless ($file =~ /^g12.+gz$/ );
   $age = ( -M $file ) * 86400;
   $time=substr($file,3,5);

       $command = sprintf("%s %s/%s mss:%s/%s", $MSRCP,$srcdir,$file,$destdir,$file);
       print "Command is:$command\n";
       system($command);
}
