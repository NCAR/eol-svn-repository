#! /usr/bin/perl -w

##Module---------------------------------------------------------------------------
# The NcepMySql module is used for connecting to CODIAC using MySQL.  It puts all
# of the data into the archive area and inserts the files into CODIAC.  It also
# updates the dates on the associated datasets for the newly inserted files.
#
# @author Joel Clawson
##Module---------------------------------------------------------------------------
package NcepMySql;
use strict;
use lib "/work/software/MySQL/lib";
use File::Copy;
use NcepUtil;
use MySqlDatabase;
use MySqlDataset;
use MySqlFile;

my $LOG;

##--------------------------------------------------------------------------------
# @signature void close()
# <p>Properly shutdown the NcepMySql module removing all of the empty temporary 
# directories and closing the log file.</p>
##--------------------------------------------------------------------------------
sub close {
    my $self = shift;

    my ($MONTH,$YEAR,$ARCHIVE);
    my $dir = sprintf("%s/ingest/%04d/%02d",NcepUtil::ncep_home(),$self->{"year"},$self->{"month"});
    opendir($MONTH,$dir) or printf($LOG "Could not read %s directory\n",$dir);
    if (scalar(grep(!/^\./,readdir($MONTH))) == 0) {
	printf($LOG "Removed directory: %s\n",$dir) if (rmdir($dir));

	my $dir = sprintf("%s/ingest/%04d",NcepUtil::ncep_home(),$self->{"year"});
	opendir($YEAR,$dir) or printf($LOG "Could not read %s directory\n",$dir);
	if (scalar(grep(!/^\./,readdir($YEAR))) == 0) {
	    printf($LOG "Removed directory: %s\n",$dir) if (rmdir($dir));
	    
	    my $dir = sprintf("%s/ingest",NcepUtil::ncep_home());
	    opendir($ARCHIVE,$dir) or printf($LOG "Could not read %s directory\n",$dir);
	    if (scalar(grep(!/^\./,readdir($ARCHIVE))) == 0) {
		printf($LOG "Removed directory: %s\n",$dir) if (rmdir($dir));
	    
	    }
	    closedir($ARCHIVE);
	}
	closedir($YEAR);
    }
    closedir($MONTH);

    close($LOG);
}

##--------------------------------------------------------------------------------
# @signature void disconnect()
# <p>Close the connection to the database.</p>
##--------------------------------------------------------------------------------
sub disconnect {
    my $self = shift;
    $self->{"database"}->disconnect();
}

##--------------------------------------------------------------------------------
# @signature int get_file_size(String path, String file)
# <p>Get the size of the specified file in kilobytes.</p>
#
# @input $path The directory containing the file.
# @input $file The name of the file.
# @output $size The size of the file in kilobytes.
##--------------------------------------------------------------------------------
sub get_file_size {
    return int(NcepUtil::file_size(sprintf("%s/%s",$_[0],$_[1]))/1024) + 1;
}

##--------------------------------------------------------------------------------
# @signature String insert_compress_file(String storm, String path, String file)
# <p>Insert a compressed GRIB file into the database.  It assumes that the GRIB 
# file is an hourly file if it does not specify a length in the file name.</p>
#
# @input $storm The storm id the file is associated with.
# @input $path The archive directory where the file stored.
# @input $file The name of the file being inserted.
# @output $err An error message if there is a problem, or the empty string if 
# everything went okay.
##--------------------------------------------------------------------------------
sub insert_compress_file {
    my $self = shift;
    my $file = MySqlFile->new();
    $file->setFile($_[1],$_[2]);
    $file->setDatasetId($_[0]);
    $file->setFormatId(66);
    $file->setPurpose("data");
    $file->setHost("localhost");

    # Date in the file is end date, not begin date
    $_[2] =~ /\D+(\d{4})(\d{2})(\d{2})(\d{2})\.(.{3})\.Z/;
    $file->setEndDate($1,$2,$3,$4,0,0);

    my $date = sprintf("%04d/%02d/%02d",$1,$2,$3);
    my $time = sprintf("%02d:00",$4);
    if (defined($5) && $5 !~ /Grb/) {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,-1*substr($5,0,2),1);
    } else {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,0,-59);
    }
    $file->setBeginDate(split(/\//,$date),split(/:/,$time),1);

    return $self->insert_into_online_database($file);
}

##--------------------------------------------------------------------------------
# @signature String insert_daily_precip()
# <p>Insert the daily precipitation gage files into the database and update the
# corresponding dataset dates.</p>
#
# @output $err An error report for commands that did not occur or an empty string
# if all of the commands executed correctly.
##--------------------------------------------------------------------------------
sub insert_daily_precip {
    my $self = shift;

    # Full paths of directories
    my $archive = sprintf("%s/dly_gage/%04d%02d",NcepUtil::ncep_archive(),$self->{"year"},$self->{"month"});
    my $ingest = sprintf("%s/ingest/%04d/%02d/dly_prcp",NcepUtil::ncep_home(),$self->{"year"},
			 $self->{"month"});

    # A reference to the function that defines the data to be put into the database.
    my $function = \&insert_daily_precip_file;

    # Insert the files into the database that match the pattern
    my $report = $self->insert_data($ingest,$archive,$function,"gage\.dly");

    # Make sure that the temporary directory holding the data has been removed.
    if (-e $ingest) {
	$report .= sprintf("Unable to remove directory: %s\n",$ingest);
    }
    return $report;
}

##--------------------------------------------------------------------------------
# @signature String insert_daily_precip_file(String path, String file)
# <p>Insert a daily precipitation gage file into the database.</p>
#
# @input $path The directory where the file is stored.
# @input $file The name of the file being inserted.
# @output $err An error message if one occured or the empty string.
##--------------------------------------------------------------------------------
sub insert_daily_precip_file {
    my $self = shift;
    return $self->insert_precip_file("21.005",$_[0],$_[1]);
}

##--------------------------------------------------------------------------------
# @signature String insert_data(String ingest, String archive, Function* function, String pattern)
# <p>Move the data that matches the pattern from the ingest directory to the 
# archive directory and insert it into the database using the specified function.</p>
#
# @input $ingest The directory where the data is currently located.
# @input $archive The directory where the data is to be archived.
# @input $function The function that correctly defines the data to put into the
# database.
# @input $pattern The pattern to use to obtain the data in the ingest directory.
# @output $msg An error message if the data could not be inserted into the database or
# the empty string if everything went okay.
##--------------------------------------------------------------------------------
sub insert_data {
    my $self = shift;
    my $ingest = shift;
    my $archive = shift;
    my $function = shift;
    my $pattern = shift;

    my $report = "";
    my $INGEST;

    # Create the archive directory.
    unless (-e $archive) {
      NcepUtil::create_directory($archive);
	printf($LOG "Created directory: %s\n",$archive);
    }

    # Get the files in the ingest directory
    opendir($INGEST,$ingest) or return sprintf("Could not open directory: %s\n",$ingest);
    my @files = grep(/$pattern/,readdir($INGEST));
    closedir($INGEST);

    foreach my $file (sort(@files)) {
	if (copy(sprintf("%s/%s",$ingest,$file),sprintf("%s/%s",$archive,$file))) {
	    chmod(0775,sprintf("%s/%s",$archive,$file));
	    printf($LOG "Copied %s from %s to %s\n",$file,$ingest,$archive);

	    # Insert the file into the database and remove it from the ingest directory.
	    my $ret_val = $self->$function($archive,$file);

	    if ($ret_val eq "") {
		if (unlink(sprintf("%s/%s",$ingest,$file))) {
		    printf($LOG "Removed file: %s/%s\n",$ingest,$file);
		} else {
		    $report .= sprintf("Unable to remove: %s/%s\n",$ingest,$file);
		}
	    } else {
		$report .= $ret_val;
	    }
	} else {
	    $report .= sprintf("Unable to copy %s/%s to %s/%s.  Did not put into database.\n",
			       $ingest,$file,$archive,$file);
	}
    }

    # Only try to remove the directory if an error has not occured.
    if ($report eq "") {
	if (rmdir($ingest)) {
	    printf($LOG "Removed directory: %s\n",$ingest);
	}
    }

    return $report;
}

##--------------------------------------------------------------------------------
# @signature String insert_gag4_file(String path, String file)
# <p>Insert a 4 KM Gage-Only GRIB file into the database.</p>
#
# @input $path The archive directory for the file.
# @input $file The name of the file to insert.
# @output $err An error message or the empty string if there is no error.
##--------------------------------------------------------------------------------
sub insert_gag4_file {
    my $self = shift;
    return $self->insert_compress_file("21.088",$_[0],$_[1]);
}

##--------------------------------------------------------------------------------
# @signature String insert_gif_file(String path, String file)
# <p>Insert a GIF imagery file into the database.</p>
#
# @input $path The archive directory for the file.
# @input $file The name of the file to insert.
# @output $err An error message or the empty string if there is no error.
##--------------------------------------------------------------------------------
sub insert_gif_file {
    my $self = shift;
    my $file = MySqlFile->new();
    $file->setFile($_[0],$_[1]);
    $file->setDatasetId("21.087");
    $file->setFormatId(34);
    $file->setPurpose("data");
    $file->setHost("localhost");

    # Date in the file is end date, not begin date
    $_[1] =~ /\D+(\d{4})(\d{2})(\d{2})(\d{2})\.(.{3}\.)?gif/;
    my $year = $1;
    my $month = $2;
    my $day = $3;
    my $hour = defined($4) ? $4 : 0;
    my $length = $5;
    $file->setEndDate($year,$month,$day,$hour,0,0);

    my $date = sprintf("%04d/%02d/%02d",$year,$month,$day);
    my $time = sprintf("%02d:00",$hour);
    if (defined($length) && $length !~ /Grb/) {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,-1*substr($length,0,2),1);
    } else {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,0,-59);
    }
    $file->setBeginDate(split(/\//,$date),split(/:/,$time),1);

    return $self->insert_into_online_database($file);
}

##-------------------------------------------------------------------------------------
# @signature String insert_hourly_precip()
# <p>Insert the hourly precipitation gage files into the database and update the
# corresponding dataset dates.</p>
#
# @output $err An error report for commands that did not occur or an empty string
# if all of the commands executed correctly.
##-------------------------------------------------------------------------------------
sub insert_hourly_precip {
    my $self = shift;

    # Full paths of directories
    my $archive = sprintf("%s/hrly_gage/%04d%02d",NcepUtil::ncep_archive(),$self->{"year"},$self->{"month"});
    my $ingest = sprintf("%s/ingest/%04d/%02d/hrly_prcp",NcepUtil::ncep_home(),$self->{"year"},
			 $self->{"month"});

    my $function = \&insert_hourly_precip_file;

    # Insert the data into the database.
    my $report = $self->insert_data($ingest,$archive,$function,"gage\.hrly");

    # Generate an error message if the ingest directory to hold data has not been removed.
    if (-e $ingest) {
	$report .= sprintf("Unable to remove directory: %s\n",$ingest);
    }
    return $report;
}

##-------------------------------------------------------------------------------------
# @signature String insert_hourly_precip_file(String path, String file)
# <p>Insert an hourly precipitation gage file into the database.</p>
#
# @input $path The directory where the file is archived.
# @input $file The name of the file being inserted.
# @output $err An error message if one occured or the empty string.
##-------------------------------------------------------------------------------------
sub insert_hourly_precip_file {
    my $self = shift;
    return $self->insert_precip_file("21.004",$_[0],$_[1]);
}

##-------------------------------------------------------------------------------------
# @signature String insert_into_online_database(MySqlFile file)
# <p>Insert the data in the file into the on_line_phys_dir database table.</p>
#
# @input $file The file that contains the information to be entered
# @output $err An error message if one occured or the empty string.
##-------------------------------------------------------------------------------------
sub insert_into_online_database {
    my $self = shift;
    my $file = shift;

    # Would be 1 because the final size is increased by 1 to make sure the size put into
    # the database is not smaller than the size of the actual file.
    if ($file->getSize() == 1) {
	return sprintf("File %s/%s has 0 size.  Not inserting into the database.\n",
		       $file->getDirectory,$file->getFilename());
    }

    if ($file->getDirectory() =~ /^\/archive/) {
        $file->setDirectory("/export".$file->getDirectory());
    }

    my $report = $file->insert($self->{"database"});

    # Execute the SQL statement.
    if ($report eq "") {
	printf($LOG "Inserted file (%s) into the database.\n",$file->getFilename());
	if (($report .= $self->update_dataset($file)) eq "") {
	    printf($LOG "Update dataset: %s\n",$file->getDatasetId());
	    $report .= $self->{"database"}->commit();
	    return $report;
	} else {
	    $self->{"database"}->rollback();
	    return sprintf("Unable to update the dataset dates for storm: %s (%s)\n",
			   $file->getDatasetId(),$report);
	}
    } else {
	return sprintf("Unable to insert the file: %s/%s\n",$file->getDirectory(),
		       $file->getFilename());
    }
}

##--------------------------------------------------------------------------------
# @signature String insert_ncep()
# <p>Insert all of the Ncep precipitation files into the database.</p>
#
# @output $err An error message if there was a problem inserting any of the files
# or the empty string if everything was inserted okay.
##--------------------------------------------------------------------------------
sub insert_ncep {
    my $self = shift;

    # Insert the Hourly Gage files
    my $report = $self->insert_hourly_precip();

    # Insert the Daily Gage files
    $report .= $self->insert_daily_precip();

    # Insert the Snapshot Imagery files
    $report .= $self->insert_snapshot();

    # Insert the Stage 4 files
    $report .= $self->insert_stage4();

    # Insert the ST2 4 KM GRIB files.
    $report .= $self->insert_st2_4km();

    return $report;
}

##--------------------------------------------------------------------------------
# @signature String insert_mul4_file(String path, String file)
# <p>Insert a 4 KM Multi-Sensor GRIB file into the database.</p>
#
# @input $path The archive directory for the file.
# @input $file The name of the file to insert.
# @output $err An error message or the empty string if there is no error.
##--------------------------------------------------------------------------------
sub insert_mul4_file {
    my $self = shift;
    return $self->insert_compress_file("21.089",$_[0],$_[1]);
}

##-------------------------------------------------------------------------------------
# @signature String insert_precip_file(String storm, String path, String file, String desc)
# <p>Insert a precip data file into the database.  It is for the hourly and daily gage
# files.</p>
#
# @input $storm The storm id for the file.
# @input $path The location where the file is archived.
# @input $file The name of the file being inserted.
# @input $desc The data description for the file.
# @output $err An error message if one occured or the empty string.
##-------------------------------------------------------------------------------------
sub insert_precip_file {
    my $self = shift;
    my $file = MySqlFile->new();
    $file->setFile($_[1],$_[2]);
    $file->setDatasetId($_[0]);
    $file->setFormatId(36);
    $file->setPurpose("data");
    $file->setHost("localhost");

    my $filedate = (split(/\./,$file->getFilename()))[3];
    my @date = (substr($filedate,0,4),substr($filedate,4,2),substr($filedate,6,2));
    $file->setBeginDate(@date,0,0,0);
    $file->setEndDate(@date,23,59,59);

    return $self->insert_into_online_database($file);
}

##--------------------------------------------------------------------------------
# @signature String insert_rad4_file(String path, String file)
# <p>Insert a 4 KM GRIB Radar Estimate file into the database.</p>
#
# @input $path The archive directory for the file.
# @input $file The name of the file to insert.
# @output $err An error message or the empty string if there is no error.
##--------------------------------------------------------------------------------
sub insert_rad4_file {
    my $self = shift;
    return $self->insert_compress_file("21.090",$_[0],$_[1]);
}

##--------------------------------------------------------------------------------
# @signature String insert_rfc4_file(String path, String file)
# <p>Insert a 4 KM GRIB Gage-Only 24 hour RFC file into the database.</p>
#
# @input $path The archive directory for the file.
# @input $file The name of the file to insert.
# @output $err An error message or the empty string if there is no error.
##--------------------------------------------------------------------------------
sub insert_rfc4_file {
    my $self = shift;
    my $file = MySqlFile->new();
    $file->setFile($_[0],$_[1]);
    $file->setDatasetId("21.091");
    $file->setFormatId(66);
    $file->setPurpose("data");
    $file->setHost("localhost");

    # Date in the file is end date, not begin date
    $_[1] =~ /rfc4\.(\d{4})(\d{2})(\d{2})(\d{2})\.(.{3})\.Z/;
    $file->setEndDate($1,$2,$3,$4,0,0);

    my $date = sprintf("%04d/%02d/%02d",$1,$2,$3);
    my $time = sprintf("%02d:00",$4);
    if (defined($5) && $5 !~ /Grb/) {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,-1*substr($5,0,2),1);
    } else {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,0,-59);
    }
    $file->setBeginDate(split(/\//,$date),split(/:/,$time),1);

    return $self->insert_into_online_database($file);
}

##--------------------------------------------------------------------------------
# @signature String insert_snapshot()
# <p>Insert the snapshot gif files into the database.</p>
#
# @output $err A report on the errors on the inserting of the files or the empty
# string if there are no errors to report.
##--------------------------------------------------------------------------------
sub insert_snapshot {
    my $self = shift;

    # Full paths of directories
    my $archive = sprintf("%s/preview_gifs/%04d%02d",NcepUtil::ncep_image_archive(),
			  $self->{"year"},$self->{"month"});
    my $ingest = sprintf("%s/ingest/%04d/%02d/snapshot",NcepUtil::ncep_home(),$self->{"year"},
			 $self->{"month"});
    my $function = \&insert_gif_file;

    my $report = $self->insert_data($ingest,$archive,$function,"\.gif");

    if (-e $ingest) {
	$report .= sprintf("Unable to remove directory: %s\n",$ingest);
    }
    return $report;
}

##--------------------------------------------------------------------------------
# @signature String insert_st2_4km()
# <p>Insert the 4 KM GRIB files into the database.</p>
#
# @output $err A report on the errors on the inserting of the files or the empty
# string if there are no errors to report.
##--------------------------------------------------------------------------------
sub insert_st2_4km {
    my $self = shift;
    my $ingest = sprintf("%s/ingest/%04d/%02d/st2_4km",NcepUtil::ncep_home(),$self->{"year"},$self->{"month"});
    my $arch_pattern = sprintf("%s/grib4km/%s/%04d%02d",NcepUtil::ncep_archive(),"%s",$self->{"year"},$self->{"month"});
    
    my $report = $self->insert_data($ingest,sprintf($arch_pattern,"gag4"),
				    \&insert_gag4_file,"ST2gg");

    $report .= $self->insert_data($ingest,sprintf($arch_pattern,"mul4"),
				  \&insert_mul4_file,"ST2ml");
    
    $report .= $self->insert_data($ingest,sprintf($arch_pattern,"rad4"),
				  \&insert_rad4_file,"ST2rd");
    
    $report .= $self->insert_data($ingest,sprintf($arch_pattern,"rfc4"),
				  \&insert_rfc4_file,"rfc4");
    
    $report .= $self->insert_data($ingest,sprintf($arch_pattern,"ubr4"),
				  \&insert_ubr4_file,"ST2un");
    
    if (-e $ingest) {
	$report .= sprintf("Unable to remove directory: %s\n",$ingest);
    }
    return $report;
}

##--------------------------------------------------------------------------------
# @signature String insert_stage4()
# <p>Insert the stage4 data files and GIF imagery into the database.</p>
#
# @output $err An error message on the inserting of the files or the empty string
# if there are no errors to report.
##--------------------------------------------------------------------------------
sub insert_stage4 {
    my $self = shift;

    # Full paths of directories
    my $archive = sprintf("%s/preview_gifs/%04d%02d",NcepUtil::ncep_image_archive(),$self->{"year"},
			  $self->{"month"});
    my $ingest = sprintf("%s/ingest/%04d/%02d/stage4",NcepUtil::ncep_home(),$self->{"year"},$self->{"month"});
    my $function = \&insert_gif_file;
    
    my $report = $self->insert_data($ingest,$archive,$function,"\.gif");
    
    $archive = sprintf("%s/stage4/%04d%02d",NcepUtil::ncep_archive(),$self->{"year"},$self->{"month"});
    $function =\&insert_stage4_file;

    $report .= $self->insert_data($ingest,$archive,$function,"^ST4");

    if (-e $ingest) {
	$report .= sprintf("Unable to remove directory: %s\n",$ingest);
    }

    return $report;
}

##--------------------------------------------------------------------------------
# @signature String insert_stage4_file(String path, String file)
# <p>Insert a stage4 data file into the database.</p>
#
# @input $path The archive directory for the file.
# @input $file The name of the file to insert.
# @output $err An error message or the empty string if there is no error.
##--------------------------------------------------------------------------------
sub insert_stage4_file {
    my $self = shift;
    return $self->insert_compress_file("21.093",$_[0],$_[1]);
}

##--------------------------------------------------------------------------------
# @signature String insert_ubr4_file(String path, String file)
# <p>Insert a 4 KM GRIB UnBiased Radar Estimation file into the database.</p>
#
# @input $path The archive directory for the file.
# @input $file The name of the file to insert.
# @output $err An error message or the empty string if there is no error.
##--------------------------------------------------------------------------------
sub insert_ubr4_file {
    my $self = shift;
    return $self->insert_compress_file("21.092",$_[0],$_[1]);
}

##-------------------------------------------------------------------------------------------
# @signature NcepArchive new(int month, int year)
# <p>Create a new module that can archive tar balls to the mass store and uncompress the
# downloaded data so it can be put into /archive/codiac.
#
# @input $month The month of the archiving.
# @input $year The year of the month.
# @output $archive The archiving module.
##-------------------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    # Die if invalid parameters
    die("Invalid arguements to NcepMySql->new.  Must have month and year.\n")
	if (scalar(@_) != 2);

    $self->{"month"} = $_[0];
    $self->{"year"} = $_[1];

    $self->{"database"} = MySqlDatabase->new("zediupdate","change-456");

    open($LOG, sprintf(">%s/logs/%04d/MySql%04d%02d.log",NcepUtil::ncep_home(),$self->{"year"},
		       $self->{"year"},$self->{"month"}));

    return $self;
}

##--------------------------------------------------------------------------------
# @signature String open()
# <p>Establish the connection to the database.</p>
#
# @output $err An error message or the empty string if no error occured.
##--------------------------------------------------------------------------------
sub open {
    my $self = shift;

    $self->{"database"}->connect() or return "Unable to connect to the database.\n";
    return "";
}

##--------------------------------------------------------------------------------
# @signature int update_dataset(MySqlFile file)
# <p>Update the specified dataset with potential new begin and end dates.</p>
#
# @input $file The file that caused the data update.
# @output $err 1 if the update was successful, 0 otherwise.
##--------------------------------------------------------------------------------
sub update_dataset {
    my $self = shift;
    my $file = shift;
    my $dataset = MySqlDataset->new($file->getDatasetId());
    
    # Get the current begin and end dates in the database.
    my $report = $dataset->selectDataset($self->{"database"});

    if ($report eq "") {
	my $file_begin = $file->getBeginDate(); $file_begin =~ s/[\s:-]//g;
	my $file_end = $file->getEndDate(); $file_end =~ s/[\s:-]//g;
	my $begin = $dataset->getBeginDate(); $begin =~ s/[\s:-]//g;
	my $end = $dataset->getEndDate(); $end =~ s/[\s:-]//g;

	if ($file_begin < $begin) { $dataset->setBeginDate(split(/[\s:-]/,$file->getBeginDate())); }
	if ($file_end > $end) { $dataset->setEndDate(split(/[\s:-]/,$file->getEndDate())); }

	return $dataset->updateDataset($self->{"database"});
    } else {
	return $report;
    }
}

1;




