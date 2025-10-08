#! /usr/bin/perl -w

##Module---------------------------------------------------------------------------
# The NcepEmpsql module is used for connecting to CODIAC using empsql.  It puts all
# of the data into the archive area and inserts the files into CODIAC.  It also
# updates the dates on the associated datasets for the newly inserted files.
#
# @author Joel Clawson
##Module---------------------------------------------------------------------------
package NcepEmpSql;
use strict;
use File::Copy;
use NcepUtil;

# Set the environment variable so empress will work correctly.
$ENV{EMPRESSPATH} = '/usr/empress8.62';

my $LOG;

##--------------------------------------------------------------------------------
# @signature void close()
# <p>Properly shutdown the NcepEmpsql module removing all of the empty temporary 
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
# @signature String insert_compress_file(String storm, String path, String file, String desc)
# <p>Insert a compressed GRIB file into the database.  It assumes that the GRIB 
# file is an hourly file if it does not specify a length in the file name.</p>
#
# @input $storm The storm id the file is associated with.
# @input $path The archive directory where the file stored.
# @input $file The name of the file being inserted.
# @input $desc The data description for the file.
# @output $err An error message if there is a problem, or the empty string if 
# everything went okay.
##--------------------------------------------------------------------------------
sub insert_compress_file {
    my $self = shift;
    my %values;

    $values{"l_dds_id"} = $_[0];
    $values{"dir_path"} = $_[1];
    $values{"file_name"} = $_[2];
    $values{"data_desc"} = $_[3];

    # Date in the file is end date, not begin date
    $_[2] =~ /\D+(\d{8})(\d{2})\.(.{3})\.Z/;
    $values{"end_date"} = $1;
    $values{"e_hour"} = $2; $values{"e_min"} = 0;

    my $date = sprintf("%04d/%02d/%02d",substr($1,0,4),substr($1,4,2),substr($1,6,2));
    my $time = sprintf("%02d:00",$2);
    if (defined($3) && $3 !~ /Grb/) {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,-1*substr($3,0,2),1);
    } else {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,0,-59);
    }
    $date =~ s/\///g;
    $values{"begin_date"} = $date;
    ($values{"b_hour"},$values{"b_min"}) = split(/:/,$time);

    $values{"logical_fmt"} = 66;

    $values{"data_amt"} = get_file_size($_[1],$_[2]);
    $values{"media_code"} = 10;
    $values{"phys_fmt"} = 1;
    $values{"hw_address"} = "localhost";
    $values{"archive_date"} = "today";

    return $self->insert_into_online_database(\%values);
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
    return $self->insert_precip_file("21.005",$_[0],$_[1],"NCEP Daily PCP");
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
    return $self->insert_compress_file("21.088",$_[0],$_[1],"NCEP 4 KM GRIB Gage-Only Analysis");
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
    my %values;

    $values{"dir_path"} = $_[0];
    $values{"file_name"} = $_[1];

    $values{"l_dds_id"} = "21.087";
    $values{"data_desc"} = "Preview GIFs";

    $_[1] =~ /\D+(\d{8})(\d{2})\.(\d{2}h\.)?gif/;
    $values{"end_date"} = $1;
    $values{"e_hour"} = $2; $values{"e_min"} = 0;

    my $date = sprintf("%04d/%02d/%02d",substr($1,0,4),substr($1,4,2),substr($1,6,2));
    my $time = sprintf("%02d:00",$2);
    if (defined($3)) {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,-1*substr($3,0,2),1);
    } else {
	($date,$time) = NcepUtil::adjustDateTime($date,$time,0,0,-59);
    }
    $date =~ s/\///g;
    $values{"begin_date"} = $date;
    ($values{"b_hour"},$values{"b_min"}) = split(/:/,$time);

    $values{"logical_fmt"} = 34;

    $values{"data_amt"} = get_file_size($_[0],$_[1]);
    $values{"media_code"} = 10;
    $values{"phys_fmt"} = 1;
    $values{"hw_address"} = "localhost";
    $values{"archive_date"} = "today";

    return $self->insert_into_online_database(\%values);
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
    return $self->insert_precip_file("21.004",$_[0],$_[1],"NCEP Hrly PCP");
}

##-------------------------------------------------------------------------------------
# @signature String insert_into_online_database(Hash* data)
# <p>Insert the data in the hash into the on_line_phys_dir database table.</p>
#
# @input $data A reference to a hash containing the data.
# @output $err An error message if one occured or the empty string.
##-------------------------------------------------------------------------------------
sub insert_into_online_database {
    my $self = shift;
    my $data = shift;

    # Would be 1 because the final size is increased by 1 to make sure the size put into
    # the database is not smaller than the size of the actual file.
    if ($$data{"data_amt"} == 1) {
	return sprintf("File %s/%s has 0 size.  Not inserting into the database.\n",
		       $$data{"dir_path"},$$data{"file_name"});
    }


    my $params = "";
    my $values = "";
    
    # Match the parameter and value in lists so they get inserted in the correct place in the table.
    foreach my $key (keys(%{$data})) {
	if ($params eq "") {
	    $params = $key;
	    $values = sprintf("'%s'",$$data{$key});
	} else {
	    $params .= sprintf(",%s",$key);
	    $values .= sprintf(",'%s'",$$data{$key});
	}
    }
    
    # Create the SQL statement to insert the data.
    my $insert = sprintf("\"INSERT INTO on_line_phys_dir(%s) VALUES(%s);\"",$params,$values);

    # Execute the SQL statement.
    if (system(sprintf("/usr/empress8.62/bin/empcmd /storm/codiac/codiac_db/phys_dir_db %s",
		       $insert)) == 0) {
	printf($LOG "Inserted file (%s) into the database.\n",$$data{"file_name"});
	if ($self->update_dataset($$data{"l_dds_id"},$$data{"begin_date"},$$data{"end_date"})) {
	    printf($LOG "Update dataset: %s\n",$$data{"l_dds_id"});
	    return "";
	} else {
	    return sprintf("Unable to update the dataset dates for storm: %s\n",$$data{"l_dds_id"});
	}
    } else {
	return sprintf("Unable to insert the file: %s/%s\n",$$data{"dir_path"},$$data{"file_name"});
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
    return $self->insert_compress_file("21.089",$_[0],$_[1],"NCEP 4 KM GRIB Multi-Sensor Analysis");
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
    my %values;

    $values{"l_dds_id"} = $_[0];
    $values{"dir_path"} = $_[1];
    $values{"file_name"} = $_[2];
    $values{"data_desc"} = $_[3];

    $values{"begin_date"} = (split(/\./,$values{"file_name"}))[3];
    $values{"b_hour"} = 0; $values{"b_min"} = 0;
    $values{"end_date"} = $values{"begin_date"};
    $values{"e_hour"} = 23; $values{"e_min"} = 59;

    $values{"logical_fmt"} = 36;

    $values{"data_amt"} = get_file_size($_[1],$_[2]);
    $values{"media_code"} = 10;
    $values{"phys_fmt"} = 2;
    $values{"hw_address"} = "localhost";
    $values{"archive_date"} = "today";

    return $self->insert_into_online_database(\%values);
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
    return $self->insert_compress_file("21.090",$_[0],$_[1],"NCEP 4 KM GRIB Radar Estimate");
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
    my %values;

    $values{"dir_path"} = $_[0];
    $values{"file_name"} = $_[1];

    $values{"l_dds_id"} = "21.091";
    $values{"data_desc"} = "NCEP 4 KM GRIB Gage-Only 24h RFC";

    $_[1] =~ /rfc4\.(\d{8})(\d{2})\.(.{3})\.Z/;
    $values{"end_date"} = $1;
    $values{"e_hour"} = $2; $values{"e_min"} = 0;

    my $date = sprintf("%04d/%02d/%02d",substr($1,0,4),substr($1,4,2),substr($1,6,2));
    my $time = sprintf("%02d:00",$2);
    ($date,$time) = NcepUtil::adjustDateTime($date,$time,-1,0,1);
    $date =~ s/\///g;
    $values{"begin_date"} = $date;
    ($values{"b_hour"},$values{"b_min"}) = split(/:/,$time);

    $values{"logical_fmt"} = 66;

    $values{"data_amt"} = get_file_size($_[0],$_[1]);
    $values{"media_code"} = 10;
    $values{"phys_fmt"} = 1;
    $values{"hw_address"} = "localhost";
    $values{"archive_date"} = "today";

    return $self->insert_into_online_database(\%values);
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
    return $self->insert_compress_file("21.093",$_[0],$_[1],"NCEP 4 KM GRIB Stage IV Analysis");
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
    return $self->insert_compress_file("21.092",$_[0],$_[1],"NCEP 4 KM GRIB UnBiased Radar (UBR) Est.");
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
    die("Invalid arguements to NcepArchive->new.  Must have month and year.\n")
	if (scalar(@_) != 2);

    $self->{"month"} = $_[0];
    $self->{"year"} = $_[1];


    open($LOG, sprintf(">%s/logs/%04d/Database%04d%02d.log",NcepUtil::ncep_home(),$self->{"year"},
		       $self->{"year"},$self->{"month"}));

    return $self;
}

##--------------------------------------------------------------------------------
# @signature int update_dataset(String storm, String begin_date, String end_date)
# <p>Update the specified dataset with potential new begin and end dates.</p>
#
# @input $storm The storm id for the dataset.
# @input $begin_date The begin date to check.
# @input $end_date The end date to check.
# @output $err 1 if the update was successful, 0 otherwise.
##--------------------------------------------------------------------------------
sub update_dataset {
    my $self = shift;
    my $storm = shift;
    my $begin_date = shift;
    my $end_date = shift;
    my $command_path = "/usr/empress8.62/bin/empcmd /storm/codiac/codiac_db/catalog_db";
    
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# TEMPORARY TO PREVENT ACCESS TO DATABASE
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#    return 1;

    # Get the current begin and end dates in the database.
    my @results = `$command_path \"SELECT begin_date,end_date FROM dataset WHERE storm_id='$storm';\"`;
    my ($begin,$end) = split(' ',$results[2]);

    # Keep the oldest begin date and the latest end date
    $begin_date = $begin if ($begin < $begin_date);
    $end_date = $end if ($end > $end_date);

    # Update with the newest values.
    return (system(sprintf("%s \"UPDATE dataset SET begin_date='%s',end_date='%s',date_last_update='today' WHERE storm_id='%s'\"",$command_path,$begin_date,$end_date,$storm)) == 0);
}

1;




