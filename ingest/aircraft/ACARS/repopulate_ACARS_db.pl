#! /usr/bin/perl -w

#########
#Written by: Sean Stroble 
#Date: 7/9/2010
########

use strict;
use Net::FTP;
use Net::FTP::Throttle;
$ENV{FTP_PASSIVE} = 1;

my $FTP_SITE = "pftp.madis-data.noaa.gov";
my $FTP_DIR;
my $DOWNLOAD_DIR;
my $LOGIN_NAME = "eol_madis_research";
my $LOGIN_PASS = "Gimm3Dat4eol";
my $MSS_DIR      = "NULL";
my @MONITORS     = ("eol-cds-ingest\@ucar.edu");
my $REPLY_TO     = "eol-cds-ingest\@ucar.edu";
my ( $year, $month, $day, $daystart, $daystop );

&main(@ARGV);

sub main {

	# Usage
	if ( $#ARGV+1 < 4 ) {
		print "ERROR: Invalid arguments inputted\n";
		print "Usage: YYYY MM DD DD\nThe first DD is start day, the second is end day.\nDays between and including the specified day will be downloaded\n";
		exit(1);
	}

	$year  = $ARGV[0];
	$month = $ARGV[1];
	$daystart = $ARGV[2];
	$daystop = $ARGV[3];
	
	print "Opening FTP connection\n";
	my $ftp = open_ftp( $year );
	
	for ($day = $daystart; $day <= $daystop; $day++) {
		
		if ($day < 10) {$day = "0" . int($day);} #quick fix for numbers lacking initial 0
		$DOWNLOAD_DIR = "/net/work/operational/aircraft";
	    	print("*****$year/$month/$day (YYYY/MM/DD) is now being downloaded*****\n");
		
		$DOWNLOAD_DIR .= "/$year/$year$month$day";
		print "Creating dir tree: $DOWNLOAD_DIR\n";
		create_directory($DOWNLOAD_DIR);

		$FTP_DIR = "/archive/$year/$month/$day/point/acars/netcdf"; 
		#print "Opening FTP connection\n";
		#my $ftp = open_ftp( $year );
		print "Downloading...\n";
		my (
			$download_error_flag, $dir_listing,
			$download_info,       @files
		) = download( $ftp, $year, $month);


		my $tries = 1;
		while ( $download_error_flag != 0 && $tries <= 10 ) {
			print("\nREOPENING DOWNLOAD CONNECTION!\n");
			my $ftp = open_ftp( $year );
			print "Attempting to download\n";

			( $download_error_flag, $dir_listing, $download_info, @files ) =
			  download( $ftp, $year, $month);
			
			$tries++;
		}
		if ( $tries >= 10 ) {
			print "DOWNLOAD ERROR: Failed after 10 tries\n";
		}

		if ($download_error_flag) {
			print "DOWNLOAD ERROR: $download_error_flag\n";
		}
		else {
			print $year;
			print $dir_listing;
			print $download_info . "\n";
		}
	}
	$ftp->quit() or die("Cannot terminate the ftp connection");
}

#recursive create directory
sub create_directory {
	my ($path) = @_;
	my @dirs      = split( /\//, $path );
	my $count     = 1;
	my $accum_dir = $dirs[0];
	while ( $count < scalar(@dirs) ) {
		$accum_dir = sprintf( "%s/%s", $accum_dir, $dirs[$count] );
		if ( !( -e $accum_dir ) ) {
			mkdir($accum_dir) || die("Cannot create $accum_dir\n");
		}
		$count++;
	}
}

sub download {
	my ( $ftp, $year, $month) = @_;
	
	$ftp->cwd("$FTP_DIR")
	  or die(
		"Could not change to working directory $FTP_DIR\n");
	
	my $listing    = "Listing of Files on $FTP_SITE/$FTP_DIR\n\n";
	my $download   = "List of Downloaded Files:\n\n";
	my $error_flag = 0;
	my @downloaded_files;
	my @files      = $ftp->dir() or die("Cannot list the files in $FTP_SITE/$FTP_DIR\n");
	my $index = 0;

	foreach my $file (@files) {
		#add file to listing
		$listing .= sprintf( "\tFile: %s\n", $file );

		my $filename = ( split( ' ', $file ) )[8];
		my $filesize = ( split( ' ', $file ) )[4];
		if ( $filename =~ /$year$month/ ) {
			$index++;
			my $tries   = 0;
			my $success = 0;
			if ( -e sprintf( "%s/%s", $DOWNLOAD_DIR, $filename ) ) {
				$download .=
				  sprintf( "\tFile: %s (%d) has already been downloaded\n",
					$filename, $filesize );
				printf( "\tFile: %s (%d) has already been downloaded\n",
					$filename, $filesize );
				push( @downloaded_files, $filename );
			}
			else {
				while ( !$success && $tries < 5 ) {

					if (
						$ftp->get(
							$filename,
							sprintf( "%s/%s", $DOWNLOAD_DIR, $filename )
						)
					  )
					{
						$download .= sprintf( "\tDownloaded File: %s (%d)\n",
							$filename, $filesize );
						push( @downloaded_files, $filename );
						printf( "file number %4d: %s was downloaded\n",
							$index, $filename );
						$success = 1;
					}
					else {
						$tries++;
					}
				}

				if ( !$success ) {
					print("\nDOWNLOAD FAILED!\n");
					return ( 1, $listing, $download, @downloaded_files );

				}
			}
		}
	}

	return ( $error_flag, $listing, $download, @downloaded_files );
}

sub open_ftp {
	my ( $year ) = @_;
	my $ftp = Net::FTP::Throttle->new( $FTP_SITE, Timeout => 960, Passive => 1, MegabitsPerSecond => 0.5 )
	  or die("Could not connect to $FTP_SITE\n");
	$ftp->login( $LOGIN_NAME, $LOGIN_PASS )
	  or die(
		"Could not login into $FTP_SITE with $LOGIN_NAME:$LOGIN_PASS\n");
	$ftp->binary;
	return $ftp;
}
