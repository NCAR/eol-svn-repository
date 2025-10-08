#!/bin/perl

use Fcntl;
use POSIX qw(tmpnam strftime);

$TAR="/bin/tar";
$RM="/bin/rm";
$MSRCP="/usr/local/dcs/bin/msrcp -wpwd jossdata -pe 32767";
$MSLS="/usr/local/dcs/bin/msls";

$log="/home/joss/g12_rainex_hi_log";
$srcdir="/ingest/live/G12/rainex/HighRes";
#$tmpdir="/codiac_tmp/joss";
$tmpdir="/work/joss/GOES_rainex_hi/tmp";
$destdir="/JOSS/DATA/RAW/BY_PROJECT/RAINEX/GOES12/1KM";

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
   next unless ($file =~ /^g12.+nc$/ );
   $age = ( -M $file ) * 86400;
   $time=substr($file,3,5);
#   print "$file ($time) is $age seconds old\n";
   if($file =~ /^g2/) {
   $oldest_g2{$time}=$age if (!defined($oldest_g2{$time}) or $age > $oldest_g2{$time});
   $newest_g2{$time}=$age if (!defined($newest_g2{$time}) or $age < $newest_g2{$time});
   $count_g2{$time}++;
   } else {
   $oldest{$time}=$age if (!defined($oldest{$time}) or $age > $oldest{$time});
   $newest{$time}=$age if (!defined($newest{$time}) or $age < $newest{$time});
   $count{$time}++;
   }
   next unless $age > 172800; # skip files younger than 2 days
#   if ($file =~ /^TEMP/) {
#      # Nuke TEMP files
##      print "Deleting $file\n";
#      unlink($full) or die "Couldn't unlink $full: $!";
#      $deleted++;
#      next;
#   }

#   system("/bin/mkdir -p $destdir/$datetime") unless -e "$destdir/$datetime";
#   system("/bin/mv $srcdir/$file $destdir/$datetime/$file.$datetime");
#   $moved{"$destdir/$datetime"}++;
}

foreach $key (sort keys %oldest) {
    if ($newest{$key} > 7200) {  #most recent file at least two hours old
        print "Satellite G10 Sounder $key has $count{$key} files, ages $newest{$key} ",
              "to $oldest{$key} seconds\n";
        ($year,$jday) = $key =~ /^(\d\d)(\d\d\d)/;
        $year = $year + 2000;
	$tarfile="$tmpdir/g0.$key.tar";
	$destfile="$destdir/$year/g10.$key.tar";
	print "Sending data for $key to $destfile\n";
        system("$TAR cf $tarfile g0*${key}*");
#        print "MSRCP Command is: $MSRCP $tarfile mss:$destfile\n"; 
        system("$MSRCP $tarfile mss:$destfile");
	system("$MSLS -l $destfile");
	system("$RM g0*${key}*");
	system("$RM $tarfile");
    }
}

foreach $key_g2 (sort keys %oldest_g2) {
    if ($newest_g2{$key_g2} > 7200) {  #most recent file at least two hours old
        print "Satellite G12 Sounder $key_g2 has $count_g2{$key_g2} files, ages $newest_g2{$key_g2} ",
              "to $oldest_g2{$key_g2} seconds\n";
        ($year,$jday) = $key_g2 =~ /^(\d\d)(\d\d\d)/;
        $year = $year + 2000;
	$tarfile="$tmpdir/g2.$key_g2.tar";
	$destfile="$destdir/$year/g12.$key_g2.tar";
	print "Sending data for $key_g2 to $destfile\n";
        system("$TAR cf $tarfile g2*${key_g2}*");
#        print "MSRCP Command is: $MSRCP $tarfile mss:$destfile\n"; 
        system("$MSRCP $tarfile mss:$destfile");
	system("$MSLS -l $destfile");
	system("$RM g2*${key_g2}*");
	system("$RM $tarfile");
    }
}

close(FH);
$monitor="stoss";
$subject="GOES Soundings stage to Mass Store";
if (-s $name > 0) {
    system("/bin/mailx -s '$subject' $monitor < $name");
}
unlink($name);

#open STDOUT, "|/bin/mailx -s '$subject' $monitor" or die
#    "Couldn't redirect stdout, $!";

exit 0 unless $deleted or %moved;

open(LOG,">>$log") or die "Couldn't open $log, $!";
print LOG asctime(time),": Deleted $deleted files\n" if $deleted;
foreach $key (sort(keys %moved)) {
   print LOG asctime(time),": Moved $moved{$key} files to $key\n";
}
close(LOG);

sub asctime {
    my($time)=@_;
    return "the epoch" if $time==0;
    return POSIX::strftime("%D %T %Z",localtime($time));
}
