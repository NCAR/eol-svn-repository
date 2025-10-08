#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------------------
# <p>The NcepFTP module is used to connect to the FTP server to download
# monthly precip data from Sid Katz.  It establishes a connection to the
# server and downloads the different sets of data on the server.  The
# steps taken by the module are placed into a log file in the logs 
# directory.</p>
#
# @author Joel Clawson
##Module-------------------------------------------------------------------------------------
package NcepFTP;
use strict;
use NcepUtil;
use Net::FTP;

$ENV{FTP_PASSIVE} = 1;

# Class variables
my ($LOG,$FTP);

##-------------------------------------------------------------------------------------------
# @signature void close()
# <p>Close down the connection to the FTP server and cleanly close the log file.</p>
##-------------------------------------------------------------------------------------------
sub close {
    my $self = shift;

    # Can only close an open connection.
    if (defined($FTP)) {
	$FTP->quit();
	printf($LOG "Closed the connection to the server.\n");
    }

    close($LOG);
}

##-------------------------------------------------------------------------------------------
# @signature String download_ceopavn()
# <p>Download the <code>ceopavn</code> files from the FTP server and put them in the ingest
# directory.</p>
#
# @output $report An error message if any of the expected files could not be downloaded, or
# the empty string if there were no errors to report.
##-------------------------------------------------------------------------------------------
sub download_ceopavn {
    my $self = shift;
    my $alive = defined($FTP);
    my $dir = sprintf("%s/ceopavn",$self->{"ingest_dir"});
    my $file_data = sprintf("ceopavn.%04d%02d",$self->{"year"},$self->{"month"});

    # Setup the ingest directory
    unless (-e $dir) {
	if (NcepUtil::create_directory($dir)) {
	    printf($LOG "Created directory: %s\n",$dir);
	} else {
	    return sprintf("Could not create ceopavn ingest directory: %s\n",$dir);
	}
    }

    # Create a connection if one does not exist
    if (!$alive) {
	my $msg = $self->open();
	return $msg if ($msg ne "");
    }

    my $report = "";
    my $days = NcepUtil::get_days_in_month($self->{"month"},$self->{"year"});

    # Loop through the days
    for (my $i = 1; $i <= $days; $i++) {
	my $file = sprintf("%s%02d00",$file_data,$i);

	# Download the 0z file.
	if ($FTP->get($file,sprintf("%s/%s",$dir,$file))) {
	    printf($LOG "Downloaded file: %s\n",$file);
	} else {
	    $file =~ s/ceopavn/ceopgfs/;
	    if ($FTP->get($file,sprintf("%s/%s",$dir,$file))) {
		printf($LOG "Downloaded file: %s\n",$file);
	    } else {
		$report .= $report eq "" ? 
		    "Could not download file: $file" : "\nCould not download file: $file";
	    }
	}

	# Download the 12z file.
	$file = sprintf("%s%02d12",$file_data,$i);
	if ($FTP->get($file,sprintf("%s/%s",$dir,$file))) {
	    printf($LOG "Downloaded file: %s\n",$file);
	    chmod(0775,$file);
	} else {
	    $file =~ s/ceopavn/ceopgfs/;
	    if ($FTP->get($file,sprintf("%s/%s",$dir,$file))) {
		printf($LOG "Downloaded file: %s\n",$file);
	    } else {
		$report .= $report eq "" ? 
		    "Could not download file: $file" : "\nCould not download file: $file";
	    }
	}
    }

    # Close the connection if this function created it.
    $self->close() if (!$alive);

    return $report;
}

##-------------------------------------------------------------------------------------------
# @signature String download_daily_precip();
# <p>Download the daily precip data from the FTP server and put them into the ingest
# directory.</p>
#
# @output $report An error message if any of the expected files could not be downloaded, or
# the empty string if there were no errors to report.
##-------------------------------------------------------------------------------------------
sub download_daily_precip {
    my $self = shift;
    my $alive = defined($FTP);
    my $dir = sprintf("%s/%s%04d",$self->{"ingest_dir"},$self->{"abbrev"},$self->{"year"});
    my $file = sprintf("prcp.dly.%s%04d",$self->{"abbrev"},$self->{"year"});

    # Setup the ingest directory
    unless (-e $dir) {
	if (NcepUtil::create_directory($dir)) {
	    printf($LOG "Created directory: %s\n",$dir);
	} else {
	    return sprintf("Could not create daily precip ingest directory: %s\n",$dir);
	}
    }

    # Create a connection if one does not exist
    if (!$alive) {
	my $msg = $self->open();
	return $msg if ($msg ne "");
    }

    # Download the daily precip file.
    my $report = "";
    if ($FTP->get($file,sprintf("%s/%s",$dir,$file))) {
	printf($LOG "Downloaded file: %s\n",$file);
	chmod(0775,$file);
    } else {
	$report = "Could not download file: $file";
    }

    # Close the connection if this function created it.
    $self->close() if (!$alive);

    return $report;
}

##-------------------------------------------------------------------------------------------
# @signature String download_data()
# <p>Download all of the data from the FTP server and put them into the ingest directory.</p>
#
# @output $report An error message if any of the expected files could not be downloaded, or
# the empty string if there were no errors to report.
##-------------------------------------------------------------------------------------------
sub download_data {
    my $self = shift;
    my $alive = defined($FTP);
    my $report = "";
    my $msg;

    # Create a connection if one does not exist
    $report .= $self->open() if (!$alive);
    return $report if ($report ne "");

    # Download the hourly precip data
    $msg = $self->download_hourly_precip();
    $report .= $msg eq "" ? $msg : sprintf("\n%s",$msg);

    # Download the daily precip data
    $msg = $self->download_daily_precip();
    $report .= $msg eq "" ? $msg : sprintf("\n%s",$msg);

    # Download the snapshot data
    $msg = $self->download_snapshot();
    $report .= $msg eq "" ? $msg : sprintf("\n%s",$msg);

    # Download the stage 4 data
    $msg = $self->download_stage4();
    $report .= $msg eq "" ? $msg : sprintf("\n%s",$msg);

    # Download the ST2_4km data
    $msg = $self->download_st2_4km();
    $report .= $msg eq "" ? $msg : sprintf("\n%s",$msg);

    # Download the ceopavn
    $msg = $self->download_ceopavn();
    $report .= $msg eq "" ? $msg : sprintf("\n%s",$msg);

    # Close the connection if this function created it.
    $self->close() if (!$alive);

    return $report;
}

##-------------------------------------------------------------------------------------------
# @signature String download_hourly_precip()
# <p>Download the hourly precip data from the FTP server and put them into the ingest
# directory.</p>
#
# @output $report An error message if any of the expected files could not be downloaded, or
# the empty string if there were no errors to report.
##-------------------------------------------------------------------------------------------
sub download_hourly_precip {
    my $self = shift;
    my $alive = defined($FTP);
    my $dir = sprintf("%s/%s%04d",$self->{"ingest_dir"},$self->{"abbrev"},$self->{"year"});
    my $file = sprintf("prcp.hrly.%s%04d",$self->{"abbrev"},$self->{"year"});

    # Setup the ingest directory
    unless (-e $dir) {
	if (NcepUtil::create_directory($dir)) {
	    printf($LOG "Created directory: %s\n",$dir);
	} else {
	    return sprintf("Could not create hourly precip ingest directory: %s\n",$dir);
	}
    }

    # Create a connection if one does not exist
    if (!$alive) {
	my $msg = $self->open();
	return $msg if ($msg ne "");
    }

    # Download the hourly precip file.
    my $report = "";
    if ($FTP->get($file,sprintf("%s/%s",$dir,$file))) {
	printf($LOG "Downloaded file: %s\n",$file);
	chmod(0775,$file);
    } else {
	$report = "Could not download file: $file";
    }

    # Close the connection if this function created it.
    $self->close() if (!$alive);

    return $report;
}

##-------------------------------------------------------------------------------------------
# @signature String download_snapshot()
# <p>Download the snapshot data from the FTP server and put them into the ingest
# directory.</p>
#
# @output $report An error message if any of the expected files could not be downloaded, or
# the empty string if there were no errors to report.
##-------------------------------------------------------------------------------------------
sub download_snapshot {
    my $self = shift;
    my $alive = defined($FTP);
    my $dir = sprintf("%s/%s%04d",$self->{"ingest_dir"},$self->{"abbrev"},$self->{"year"});
    my $file = sprintf("snapshot.%s%04d",$self->{"abbrev"},$self->{"year"});

    # Setup the ingest directory
    unless (-e $dir) {
	if (NcepUtil::create_directory($dir)) {
	    printf($LOG "Created directory: %s\n",$dir);
	} else {
	    return sprintf("Could not create ceopavn ingest directory: %s\n",$dir);
	}
    }

    # Create a connection if one does not exist
    if (!$alive) {
	my $msg = $self->open();
	return $msg if ($msg ne "");
    }

    # Download the snapshot file.
    my $report = "";
    if ($FTP->get($file,sprintf("%s/%s",$dir,$file))) {
	printf($LOG "Downloaded file: %s\n",$file);
	chmod(0775,$file);
    } else {
	$report = "Could not download file: $file";
    }

    # Close the connection if this function created it.
    $self->close() if (!$alive);

    return $report;
}

##-------------------------------------------------------------------------------------------
# @signature String download_st2_4km()
# <p>Download the st2_4km data from the FTP server and put them into the ingest
# directory.</p>
#
# @output $report An error message if any of the expected files could not be downloaded, or
# the empty string if there were no errors to report.
##-------------------------------------------------------------------------------------------
sub download_st2_4km {
    my $self = shift;
    my $alive = defined($FTP);
    my $dir = sprintf("%s/%s%04d",$self->{"ingest_dir"},$self->{"abbrev"},$self->{"year"});
    my $file_data = sprintf("ST2_4km.%04d%02d",$self->{"year"},$self->{"month"});

    # Setup the ingest directory
    unless (-e $dir) {
	if (NcepUtil::create_directory($dir)) {
	    printf($LOG "Created directory: %s\n",$dir);
	} else {
	    return sprintf("Could not create st2_4km ingest directory: %s\n",$dir);
	}
    }

    # Create a connection if one does not exist
    if (!$alive) {
	my $msg = $self->open();
	return $msg if ($msg ne "");
    }

    my $report = "";
    my $days = NcepUtil::get_days_in_month($self->{"month"},$self->{"year"});

    # Loop through the days for the month
    for (my $i = 1; $i <= $days; $i++) {
	my $file = sprintf("%s%02d",$file_data,$i);
	
	# Download the st2_4km file for the day
	if ($FTP->get($file,sprintf("%s/%s",$dir,$file))) {
	    printf($LOG "Downloaded file: %s\n",$file);
	    chmod(0775,$file);
	} else {
	    $report .= $report eq "" ? "Could not download file: $file" : 
		"\nCould not download file: $file";
	}
    }

    # Close the connection if this function created it.
    $self->close() if (!$alive);

    return $report;
}

##-------------------------------------------------------------------------------------------
# @signature String download_stage4()
# <p>Download the stage4 data from the FTP server and put them into the ingest
# directory.</p>
#
# @output $report An error message if any of the expected files could not be downloaded, or
# the empty string if there were no errors to report.
##-------------------------------------------------------------------------------------------
sub download_stage4 {
    my $self = shift;
    my $alive = defined($FTP);
    my $dir = sprintf("%s/%s%04d",$self->{"ingest_dir"},$self->{"abbrev"},$self->{"year"});
    my $file = sprintf("stage4.%s%04d",$self->{"abbrev"},$self->{"year"});

    # Setup the ingest directory
    unless (-e $dir) {
	if (NcepUtil::create_directory($dir)) {
	    printf($LOG "Created directory: %s\n",$dir);
	} else {
	    return sprintf("Could not create stage4 ingest directory: %s\n",$dir);
	}
    }

    # Create a connection if one does not exist
    if (!$alive) {
	my $msg = $self->open();
	return $msg if ($msg ne "");
    }

    my $report = "";
    # Download the stage4 file
    if ($FTP->get($file,sprintf("%s/%s",$dir,$file))) {
	printf($LOG "Downloaded file: %s\n",$file) if ($report eq "");
	chmod(0775,$file);
    } else {
	$report = "Could not download file: $file";
    }

    # Close the connection if this function created it.
    $self->close() if (!$alive);

    return $report;
}

##-------------------------------------------------------------------------------------------
# @signature NcepFTP new(int month, int year)
# <p>Create a new module that downloads the data from the FTP server.
#
# @input $month The month of the archiving.
# @input $year The year of the month.
##-------------------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    # Die if invalid parameters
    die("Invalid arguements to NcepArchive->new.  Must have month and year.\n")
	if (scalar(@_) != 2);

    $self->{"month"} = $_[0];
    $self->{"year"} = $_[1];

    # Create the log directory
    my $log_dir = sprintf("%s/logs/%04d",NcepUtil::ncep_home(),$self->{"year"});
  NcepUtil::create_directory($log_dir) unless(-e $log_dir);
    
    # Open the log file
    open($LOG, sprintf(">%s/logs/%04d/FTP%04d%02d.log",NcepUtil::ncep_home(),$self->{"year"},
		       $self->{"year"},$self->{"month"})) or die(sprintf("%s/logs/%04d/FTP%04d%02d.log",
				NcepUtil::ncep_home(),$self->{"year"},$self->{"year"},$self->{"month"}));

    # Define some variable to ease use.
    $self->{"ingest_dir"} = sprintf("%s/ingest/%04d/%02d",NcepUtil::ncep_home(),
				    $self->{"year"},$self->{"month"});
    $self->{"abbrev"} = NcepUtil::monthAbbrev($self->{"month"});

    return $self;
}

##-------------------------------------------------------------------------------------------
# @signature String open()
# <p>Open the connection to the FTP server and get to the data directory.</p>
#
# @output $report An error message if any of the expected files could not be downloaded, or
# the empty string if there were no errors to report.
##-------------------------------------------------------------------------------------------
sub open {
    my $self = shift;
    my $email = "joss\@joss.ucar.edu";

    # Establish the connection
    $FTP = Net::FTP->new("ftp.emc.ncep.noaa.gov",Timeout => 480) or 
	return "Could not establish a connection with the server.";
    printf($LOG "Connected to the server.\n");

    # Login to the server
    $FTP->login("anonymous",$email) or
	return "Could not login to the server.";
    printf($LOG "Logged into the server as anonymous.\n");

    # Change to the data directory
    $FTP->cwd("/mmb/gcp/precip/JOSS") or
	return "Could not change to the data directory on the server.";
    printf($LOG "Changed to the data directory on the server.\n");

    return "";
}

1;

