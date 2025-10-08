#! /usr/bin/perl -w

use strict;
use Email::MIME;
use Email::MIME::Creator;
use Net::FTP;

$ENV{FTP_PASSIVE} = 1;

my $FTP_SITE = "ftp.ncdc.noaa.gov";
my $FTP_DIR;
my $LOGIN_EMAIL  = "eol-cds-ingest\@ucar.edu";
my $DOWNLOAD_DIR = "/ingest/tmp/asos";
my $MSS_DIR      = "/JOSS/DATA/RAW/SURFACE/ASOS/NCDC/1min";
my @MONITORS     = ("eol-cds-ingest\@ucar.edu");
my $REPLY_TO     = "eol-cds-ingest\@ucar.edu";
my ( $year, @types, $month );
my %abbrev = (
	6405 => "om1",
	6406 => "om2",
	6401 => "fmd",
);

&main(@ARGV);

sub main {

	# Usage
	if ( !defined @ARGV ) {
		sendMailAndDie( "Script did not run. Usage was incorrect.\n"
			  . "Usage: repopulate_ASOS_db.pl year month\n");
	}

	$year  = $ARGV[0];
	$month = $ARGV[1];
	@types = ( "6401", "6405", "6406" );
	foreach my $type (@types) {
		printf(
"*****Type %d for year %d and month %d is now being downloaded*****\n",
			$type, $year, $month );

		create_directory($DOWNLOAD_DIR);

		if ( $type == "6401" ) { $FTP_DIR = "pub/data/asos-fivemin"; }
		elsif ( $type == "6405" || $type == "6406" ) {
			$FTP_DIR = "pub/data/asos-onemin";
		}
		else { sendMailAndDie("Unknown type $type given as arg to script\n"); }

		my $ftp = open_ftp( $year, $type );
		my ( $download_error_flag, $dir_listing, $download_info, @files ) =
		  download( $ftp, $year, $month );
		my (@tar_files) = create_tar_files( $year, $type, $month, @files );
		foreach my $file (@tar_files) { print "Compressed " . $file . "\n"; }
		my ( $mass_store_error_flag, $mass_store ) =
		  put_on_mass_store( $year, @tar_files );

		if ($download_error_flag) {
			send_mail(
				sprintf( "NCDC 1min ASOS Download Error %s %s", $year, $type ),
				sprintf(
"There was an error downloading the 1min ASOS data from NCDC for %02d %04d.  Details follow.\n\n%s\n\n%s\n\n%s\n",
					$type, $year, $dir_listing, $download_info, $mass_store
				)
			);
		}
		elsif ($mass_store_error_flag) {
			send_mail(
				sprintf(
					"NCDC 1min ASOS Mass Store Error %s %s", $type, $year
				),
				sprintf(
"There was an error putting the 1min ASOS data from NCDC on the mass store for %02d %04d.  Details follow.\n\n%s\n\n%s\n\n%s\n",
					$type, $year, $dir_listing, $download_info, $mass_store
				)
			);
		}
		else {
			print $type;
			print $year;
			print $dir_listing;
			print $download_info;
			print $mass_store;
			send_mail(
				sprintf(
					"NCDC 1min ASOS Notification %s %s %s",
					$type, $year, $month
				),
				sprintf(
"The 1min ASOS data from NCDC for %02d %04d %d was successfully downloaded and put on the mass store.\n\n%s\n\n%s\n\n%s\n",
					$type,        $year,          $month,
					$dir_listing, $download_info, $mass_store
				)
			);
		}
	}
}

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
	my ( $ftp, $year, $month ) = @_;
	my $listing  = "Listing of Files on $FTP_SITE/$FTP_DIR\n\n";
	my $download = "List of Downloaded Files:\n\n";
	my @downloaded_files;
	my $error_flag = 0;

	my @files = $ftp->dir()
	  or sendMailAndDie("Cannot list the files in $FTP_SITE/$FTP_DIR\n");
	foreach my $file (@files) {
		$listing .= sprintf( "\tFile: %s\n", $file );

		my $filename = ( split( ' ', $file ) )[8];
		my $filesize = ( split( ' ', $file ) )[4];
		if ( $filename =~ /$year$month/ ) {
			my $tries   = 0;
			my $success = 0;
			while ( !$success && $tries < 5 ) {
				if (
					$ftp->get(
						$filename, sprintf( "%s/%s", $DOWNLOAD_DIR, $filename )
					)
				  )
				{
					$download .= sprintf( "\tDownloaded File: %s (%d)\n",
						$filename, $filesize );
					push( @downloaded_files, $filename );
					$success = 1;
				}
				else {
					$tries++;
				}
			}

			if ( !$success ) {
				$download .=
				  sprintf( "FAILED DOWNLOAD: %s (%s) -> Tried %d times!\n",
					$filename, $filesize, $tries );
				$error_flag = 1;
			}
		}
	}

	$ftp->quit();
	return ( $error_flag, $listing, $download, @downloaded_files );
}

sub open_ftp {
	my ( $year, $type ) = @_;
	my $ftp = Net::FTP->new( $FTP_SITE, Timeout => 960,Passive => 1)
	  or sendMailAndDie("Could not connect to $FTP_SITE\n");
	$ftp->login( "anonymous", $LOGIN_EMAIL )
	  or sendMailAndDie(
		"Could not login into $FTP_SITE with anonymous:$LOGIN_EMAIL\n");
	$ftp->binary;
	$ftp->cwd("$FTP_DIR/$type-$year")
	  or sendMailAndDie(
		"Could not change to working directory $FTP_DIR/$type-$year\n");
	return $ftp;
}

sub put_on_mass_store {
	my ( $year, @files ) = @_;
	my $mss        = "Mass Store Copy:\n\n";
	my $error_flag = 0;

    die "MSS is dead. $0 must be fixed.";

	my $MSS_DIR = sprintf( "%s/%04d", $MSS_DIR, $year );

	foreach my $file (@files) {
		printf( "/net/local_lnx/dcs/bin/msrcp -pe 32767 -wpwd"
			  . " jossdata %s/%s mss:%s/%s\n",
			$DOWNLOAD_DIR, $file, $MSS_DIR, $file );
		if (
			system(
				sprintf(
"/net/local_lnx/dcs/bin/msrcp -pe 32767 -wpwd jossdata %s/%s mss:%s/%s",
					$DOWNLOAD_DIR, $file, $MSS_DIR, $file
				)
			) != 0
		  )
		{
			$mss .= sprintf( "File: %s/%s could not be copied to %s\n",
				$DOWNLOAD_DIR, $file, $MSS_DIR );
			$error_flag = 1;
		}
		else {
			$mss .= sprintf(
				"\tFile: %s/%s was successfully put on the mass store at %s\n",
				$DOWNLOAD_DIR, $file, $MSS_DIR );
			if ( !unlink( sprintf( "%s/%s", $DOWNLOAD_DIR, $file ) ) ) {
				$mss .= sprintf( "File: %s/%s could not be deleted.\n",
					$DOWNLOAD_DIR, $file );
				$error_flag = 1;
			}
		}
	}

	return ( $error_flag, $mss );
}

sub send_mail {
	my ( $subject, $body ) = @_;
	my $sender = "eol-cds-ingest\@ucar.edu";
	my @parts  = (
		Email::MIME->create(
			attributes => { content_type => "text/plain" },
			body =>
			  sprintf(
"This message was generated by /home/joss/asos/repopulate_ASOS_db.pl on tsunami.\n\n%s",
				$body )
		)
	);
	my $email = Email::MIME->create( parts => [@parts] );
	$email->header_set( "From"     => $sender );
	$email->header_set( "Reply-To" => $REPLY_TO );
	$email->header_set( "To"       => @MONITORS );
	$email->header_set( "Subject"  => $subject );

	open( my $SENDMAIL, "|/usr/lib/sendmail -t" )
	  || die("Unable to open sendmail\n");
	printf( $SENDMAIL $email->as_string() );
	close($SENDMAIL);
}

sub sendMailAndDie {
	my ($body) = @_;
	send_mail( "NCDC 1min ASOS ERROR", $body );
	exit(1);
}

sub create_tar_files {
	my ( $year, $type, $month, @files ) = @_;
	if ( @files == 0 ) {
		sendMailAndDie("No files exist in the ingest location.\n");
	}

	my $tarfile = sprintf( "%s_%04d%02d.tar", $abbrev{$type}, $year, $month );
	print "Creating " . $tarfile . "\n";

	my @tar_files = ();

	# Only create the tar file if it doesn't already exist, else warn user.
	if ( -e $DOWNLOAD_DIR . "/" . $tarfile ) {
		sendMailAndDie("$tarfile already exists. New tar file not created.\n");
	}
	else {

	#printf("/bin/tar -cf %s/%s %s\n",$DOWNLOAD_DIR,$tarfile, join(" ",@files));
		if (
			system(
				sprintf(
					"cd %s; /bin/tar -cf %s %s",
					$DOWNLOAD_DIR, $tarfile, join( " ", @files )
				)
			) != 0
		  )
		{
			sendMailAndDie(
				"$tarfile was not able to be created.\n"
				  . sprintf(
					"/bin/tar -cf %s/%s %s",
					$DOWNLOAD_DIR, $tarfile, join( " ", @files )
				  )
			);
		}
		else {

			# remove original file
			foreach my $file (@files) {
				if ( !unlink( sprintf( "%s/%s", $DOWNLOAD_DIR, $file ) ) ) {
					send_mail(
						sprintf( "%s was not able to be removed.\n", $file ) );
				}
			}

			# Compress file
			if (
				system( sprintf( "compress %s/%s", $DOWNLOAD_DIR, $tarfile ) )
				!= 0 )
			{
				sendMailAndDie(
					"$DOWNLOAD_DIR/$tarfile was not able to be compressed\n");
			}

			push( @tar_files, sprintf( "%s", $tarfile . ".Z" ) );
		}
	}

	return (@tar_files);
}
