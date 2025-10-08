#! /usr/bin/perl -w

#
# Note: These data are not loaded into codiac operationally. They are
# loaded as needed for specific projects. Search for ASOS in codiac
# to see what projects and time periods they have been loaded for.
#
use strict;
#use lib "/h/eol/dmg/HPSS_cronjobs/lib";
use lib "/net/work/lib/perl/hpss";
use lib "/net/work/lib/perl/mail";
use Net::FTP;
use HPSS;
use MAIL;

$ENV{FTP_PASSIVE} = 1;

my $FTP_SITE = "ftp.ncdc.noaa.gov";
my $FTP_DIR = "pub/download/hidden/onemin";
my $LOGIN_EMAIL = "eol-cds-ingest\@ucar.edu";
my $DOWNLOAD_DIR = "/scr/tmp/joss/asos";
my $HPSS_DIR = "/FS/EOL/operational/surface/asos/ncdc";
# Campaign Storage user, host, archive path
my $CS_HOST='eoldata@data-access.ucar.edu';
my $CS_DIR="/glade/campaign/eol/archive/operational/surface/asos/ncdc";
my @monitors = ("eol-cds-ingest\@ucar.edu");

&main();

sub main {
    my ($year,$month) = find_download_month();
    create_directory($DOWNLOAD_DIR);
    
    my $ftp = open_ftp();
    my ($download_error_flag,$dir_listing,$download_info,@files) = download($ftp,$year,$month);
    my ($HPSS_error_flag,$hpss) = put_on_hpss($year,@files);
    my ($CS_error_flag,$cs) = put_on_campaign_store($year,@files);

    if ($download_error_flag) {
	sendMailAndDie(sprintf("There was an error downloading the 1min ASOS data from NCDC for %02d %04d.  Details follow.\n\n%s\n\n%s\n\n%s\n",
		          $month,$year,$dir_listing,$download_info,$hpss));
    } elsif ($HPSS_error_flag) {
       	sendMailAndDie(sprintf("There was an error putting the 1min ASOS data from NCDC on the HPSS for %02d %04d.  Details follow.\n\n%s\n\n%s\n\n%s\n",
		          $month,$year,$dir_listing,$download_info,$hpss));
    } else {
	#sendMailAndDie(sprintf("The 1min ASOS data from NCDC for %02d %04d was successfully downloaded and put on the hpss.\n\n%s\n\n%s\n\n%s\n",
	#	          $month,$year,$dir_listing,$download_info,$hpss));
    }
}

sub create_directory {
    my ($path) = @_;
    my @dirs = split(/\//,$path);
    my $count = 1;
    my $accum_dir = $dirs[0];
    while ($count < scalar(@dirs)) {
        $accum_dir = sprintf("%s/%s",$accum_dir,$dirs[$count]);
        if (!(-e $accum_dir)) {
            mkdir($accum_dir) || die("Cannot create $accum_dir\n");
        }
        $count++;
    }
}

sub download {
    my ($ftp,$year,$month) = @_;
    my $listing = "Listing of Files on $FTP_SITE/$FTP_DIR\n\n";
    my $download = "List of Downloaded Files:\n\n";
    my @downloaded_files;
    my $error_flag = 0;
    
    my @files = $ftp->dir() or sendMailAndDie("Cannot list the files in $FTP_SITE/$FTP_DIR\n");

    foreach my $file (@files) {
       $listing .= sprintf("\tFile: %s\n",$file);

       my $filename = (split(' ',$file))[8];
       my $filesize = (split(' ',$file))[4];
       if ($filename =~ /$year$month/) {
	   my $tries = 0;
	   my $success = 0;
	   while (!$success && $tries < 5) {
	       #print "$filename\n"; exit(10);
	       if (-e "$DOWNLOAD_DIR/$filename" && -s "$DOWNLOAD_DIR/$filename")
	       {
		   $download .= sprintf("\tFile Already Downloaded: %s (%d)\n",$filename,$filesize);
		   push(@downloaded_files,$filename);
		   $success = 1;
	       }
	       elsif ($ftp->get($filename,sprintf("%s/%s",$DOWNLOAD_DIR,$filename))) {
		   $download .= sprintf("\tDownloaded File: %s (%d)\n",$filename,$filesize);
		   push(@downloaded_files,$filename);
		   $success = 1;
	       } else {
		   $tries++;
	       }
	   }

	   if (!$success) {
	       $download .= sprintf("FAILED DOWNLOAD: %s (%s) -> Tried %d times!\n",$filename,$filesize,$tries);
	       $error_flag = 1;
	   }
       }
    }

    $ftp->quit();
    return ($error_flag,$listing,$download,@downloaded_files);
}

sub find_download_month {
    my @today = localtime();
    my $year = $today[5] + 1900;
    my $month = $today[4];

    if ($month == 0) {
       $year--;
       $month = 12;
    }

    return (sprintf("%04d",$year),sprintf("%02d",$month));    
}

sub open_ftp {
    my $ftp = Net::FTP->new($FTP_SITE,Timeout => 480) or sendMailAndDie("Could not connect to $FTP_SITE\n");
    $ftp->login("anonymous",$LOGIN_EMAIL) or sendMailAndDie("Could not login into $FTP_SITE with anonymous:$LOGIN_EMAIL\n");
    $ftp->binary;
    $ftp->cwd($FTP_DIR) or sendMailAndDie("Could not change to working directory $FTP_DIR\n");
    return $ftp;
}

sub put_on_hpss {
    my ($year,@files) = @_;
    my $hpss = "HPSS Copy:\n\n";
    my $error_flag = 0;
    
    my $HPSS_DIR = sprintf("%s/%04d",$HPSS_DIR,$year);

    foreach my $file (@files) {

	my $result = HPSS::put(\"$DOWNLOAD_DIR/$file",\"$HPSS_DIR/$file"); #, "-d");
	if ($result ne "") {
		$hpss .= sprintf("File: %s/%s could not be copied to %s\n%s\n",$DOWNLOAD_DIR,$file,$HPSS_DIR,$result);
		$error_flag = 1;
        } else {
	    $hpss .= sprintf("\tFile: %s/%s was successfully put on the HPSS at %s\n",$DOWNLOAD_DIR,$file,$HPSS_DIR);
	}

    }

    return ($error_flag,$hpss);
}

sub put_on_campaign_store {
    my ($year,@files) = @_;
    my $cs = "Campaign Store Copy:\n\n";
    my $error_flag = 0;

    ## check if year directory exists, if not create it ##
    my $exists = 0;
    my @date_dirs = `ssh $CS_HOST ls $CS_DIR 2>&1`;
    my $mkdir;
    chomp @date_dirs;
    foreach my $dir (sort(@date_dirs)) {
        if ($dir eq $year) { 
	    $exists = 1;
	}
    }
    if (!$exists) {
       $cs .= "Creating YYYY directory on Campaign Storage\n";
       $mkdir = `ssh $CS_HOST  mkdir $CS_DIR/$year 2>&1`;
   }
   ##### Copy files to archive location #####
    foreach my $file (@files) {
	my $result = `scp $DOWNLOAD_DIR/$file $CS_HOST:$CS_DIR/$year/$file 2>&1`;
        if ($result ne "") {
	    $cs .= sprintf("File: %s/%s could not be copied to %s\n%s\n",$DOWNLOAD_DIR,$file,$HPSS_DIR,$result);
	    $error_flag = 1;
	} else {
	    $cs .= sprintf("\tFile: %s/%s was successfully put on the Campaign Store at %s/%s\n",$DOWNLOAD_DIR,$file,$CS_DIR,$year);
            if (!unlink(sprintf("%s/%s",$DOWNLOAD_DIR,$file))) {
	        $cs .= sprintf("File: %s/%s/%s could not be deleted.\n",$CS_DIR,$year,$file);
                $error_flag = 1;
	    }
        }
    }
    ## Chmod files in Campaign Storage directory ##
    my $my_cs_chmod = `ssh $CS_HOST chmod 440 $CS_DIR/$year/* 2>&1`;
    $cs .= $my_cs_chmod;
    return ($error_flag,$cs);
}

sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("NCDC 1min ASOS ERROR",$0."\n\n".$body, @monitors);
    exit(1);
}
