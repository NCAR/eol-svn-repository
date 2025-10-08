#!/usr/bin/perl
#
# This script will run on a periodic basis and create tar directories for 
# received ECMWF files for the 00 and 12 UTC model runs for the 
# duration of the OTREC field campaign

$|=1;

use Archive::Tar;
use Time::Local;

$SIG{'ALRM'} = 'handler';

($prog = $0) =~ s%.*/%%;
#print `/bin/ps -ef`;
$num = grep /perl .*$prog/,`/bin/ps -ef`;
if($num > 1) {
 print "Instance already running. exiting\n";
 exit;
}

$tmp = "/scr/ctm/tmp";
$email_carol = "costanza@ucar.edu";
$email_linda = "echohawk@ucar.edu";

# Get previous day time variables 
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime(time-(86400));
$mon++;
$yday++;
$year += 1900;
$monday = sprintf("%02d%02d",$mon,$mday);
$yearmonday = sprintf("%04d%02d%02d",$year,$mon,$mday);
print "Month Day is: $monday\n";

$hr[0] = '00';
$hr[1] = '12';

$ecmwfdir = "/scr/ctm/catalog/ecmwf";
$dest = "/FS/EOL/2019/otrec/model";

for ($h = 0; $h <= 1; $h++) {
	# Search over the date for 00 UTC
	$searchhour = sprintf("D1D%s%s00*",$monday,$hr[$h]);
	print "What to search: $searchhour\n";
	chdir($ecmwfdir) || die "Couldn't reach $ecmwfdir: $!";
	# Copy all of the files in the search to the $tmp dir
	$copy_output = `cp $searchhour $tmp`;
	# Create the temporary tar file valid for one forecast hour of ECMWF
	$tfile = sprintf("ECMWF_%s_%s.tar",$yearmonday,$hr[$h]);
	$tarfile = sprintf("$tmp/$tfile");
	print "Temp Tar File: $tarfile\n";
	chdir($tmp) || die "Couldn't reach $tmp: $!";
	# Get a list of all the files together in the $tmp directory
	@files = glob("$searchhour");
	# Tar all files that have just been copied to $tmp that match $searchhour
	print "Creating tar file\n";
	$tar_output = Archive::Tar->create_archive($tarfile,@files);
	# Remove all of the files from the $tmp directory that have been tared
	unlink(@files);
	print "Removed all files from tmp dir\n";
	print "Destination directory on HPSS: $dest\n";
	print "Running the HPSS push...\n";
	$res = &hpss_push($tarfile,$tfile,$dest);   
	unless($res){
		# Remove the tarfile from the $tmp
		unlink($tarfile);
		print "Removed tar file from tmp dir\n\n";
	} else {
		print "Error in HPSS push\n\n";
		system("echo 'The OTREC ECMWF archive FAILED!' | mailx -s 'ECMWF Archive FAILED $yearmonday' $email_carol");
		system("echo 'The OTREC ECMWF archive FAILED!' | mailx -s 'ECMWF Archive FAILED $yearmonday' $email_linda");
	    exit 0;
	}
}

########################### HPSS function #################################
sub hpss_push {
    my($local,$file,$dest)=@_;
    $HSI='/opt/local/bin/hsi';

    $dir_exist = system("$HSI 'ls $dest'");
    if($dir_exist == 0 ) {
       $command = "$HSI 'cd $dest; put $local : $file; chmod 764 $file'";
       print "Directory exists, Command is $command\n";
    } else {
       $command = "$HSI 'mkdir -p $dest; cd $dest; put $local : $file; chmod 764 $file; chmod 3775 $dest'";
       print "No Directory, Command is $command\n";
    }
    $result=system($command);

    return($result);
}


