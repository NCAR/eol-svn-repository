#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The HIAPER_Converter script is used for converting NSF/NCAR HIAPER (GV) 
# dropsonde data in the ISF EOL sounding format into the EOL Sounding Composite 
# (ESC) format.</p>
#     NOTE:  From www.hiaper.ucar.edu: 2006 heralded the debut of the 
#            nation's most advanced research aircraft, the NSF/NCAR 
#            Gulfstream V (or GV, formerly referred to as HIAPER).  
#
#
# @author Linda Echo-Hawk 6 February 2015
# @version 1.3 Updated for DEEPWAVE
#          - BEWARE: Do not use this version for future projects. See below.
#          - BEWARE: For DEEPWAVE, one file had a problem with the time values. 
#            The first one was OK (-1.00), but the rest were 459647995.50, etc. 
#            The file is D20140624_064129_P.1.PresCorrQC.eol. This file also 
#            has a bad UTC Launch Time: 1999, 11, 30, 06:41:29. This required
#            several fixes that are specific for DEEPWAVE ONLY. Do not use 
#            this version of the HIAPER_Converter.pl script for future
#            projects. It would be better to use the MPEX or previous 
#            versions.
#          - Because of the code added for the file with problem time values,
#            the original method of this converter to read the raw data into
#            and array and then reverse it (for dropsondes) had to be changed. 
#            For this version, the data is read in and the output (toString)
#            is read into an output array. When all the records have been 
#            read into the array, it is reversed, and then printed to the
#            output file. 
#
#
# @author Linda Echo-Hawk 5 March 2014
# @version 1.2 Updated for MPEX
#          - Added code for the radiosonde header line so that
#            the sonde type would be included (missing from raw data)
#          
#
# @author Linda Echo-Hawk 21 March 2011
# @version 1.1 Updated for PREDICT
#          - Updated the function to use /net/work libs
#          - Added informational output messages
#          - Added build_latlon_format function
#          - Search for HARD-CODED and change the project name to 
#            re-use this script
#
# @author Joel Clawson
# @version 1.0 This conversion was originally created for T-REX.  It was adapted
# from the ISS EOL to ESC conversion script.
##Module------------------------------------------------------------------------
package HIAPER_Converter;
use strict;

 if (-e "/net/work/") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/Station";
    use lib "/net/work/lib/perl/UpperAir";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/Station";
    use lib "/work/lib/perl/UpperAir";
}

use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

use SimpleStationMap;
use Station;

my ($WARN);

printf "\nHIAPER_Converter began on ";print scalar localtime;printf "\n";  
&main();
printf "\nHIAPER_Converter ended on ";print scalar localtime;printf "\n";
##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the HIAPER dropsonde data by converting it from the native ASCII
# format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = HIAPER_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature HIAPER_Converter new()
# <p>Create a new instance of a HIAPER_Converter.</p>
#
# @output $self A new HIAPER_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
	# HARD-CODED PROJECT NAME
    $self->{"PROJECT"} = "DEEPWAVE";
    $self->{"NETWORK"} = "NCAR_GV";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->clean_for_file_name($self->{"NETWORK"}),
				      $self->clean_for_file_name($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";
    
    $self->{"stations"} = SimpleStationMap->new();
    
    return $self;
}

##------------------------------------------------------------------------------
# @signature Station build_default_station(String station_id, String network)
# <p>Create a default station for the HIAPER network using the specified 
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub build_default_station {
    my ($self,$station_id,$network) = @_;
    
    my $station = Station->new($station_id,$network);
    $station->setStationName($network);

    # $station->setStateCode("CA");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99);
    # Radiosonde, Vaisala RD94 (#956)
    $station->setPlatformIdNumber(956);
    $station->setMobilityFlag("m");

    return $station;
}

##-------------------------------------------------------------------------
# @signature String clean_for_file_name(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub clean_for_file_name {
    my ($self,$text) = @_;

    # Convert spaces to underscores.
    $text =~ s/\s+/_/g;

    # Remove all hyphens
    $text =~ s/\-//g;

    return $text;
}

##------------------------------------------------------------------------------
# @signature String build_latlon_format(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds the the value.
##------------------------------------------------------------------------------
sub build_latlon_format {
    my ($self,$value) = @_;

    my $fmt = $value < 0 ? "-" : "";
    while (length($fmt) < length($value)) { $fmt .= "D"; }
    return $fmt;
}
 
##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub convert {
    my ($self) = @_;
    
    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    mkdir($self->{"FINAL_DIR"}) unless (-e $self->{"FINAL_DIR"});
    
    $self->read_data_files();
    $self->print_station_files();
}

##------------------------------------------------------------------------------
# @signature ClassHeader parse_header(String file, String[] lines)
# <p>Parse the header lines from the specified file into the ESC header format.</p>
#
# @input $file The name of the file being parsed.
# @input $lines[] The header lines to be parsed.
# @output $header The header data in ESC format.
##------------------------------------------------------------------------------
sub parse_header {
    my ($self,$file,@lines) = @_;
    my $header = ClassHeader->new();

    # Parse off the release direction of the sounding.
    my ($datatype,$direction) = split(/\//,(split(/:/,$lines[0]))[1]);
    $header->setReleaseDirection(trim($direction));

    # ----------------------------------------
	# NOTE: Confirm that each of these values 
	# appears in the raw data. For MPEX, 
	# there was no $flight.
	# ----------------------------------------
    # Parse the general flight information
	# Project Name/Platform:   GV System - DEEPWAVE, RF01/none, none
	# ----------------------------------------
    my ($project,$flight,$platform,$id) = split(/[\/,]/,(split(/:/,$lines[2]))[1]);
    $flight = trim($flight);
    $id = trim($id);
	$project = trim($project);

	# for DEEPWAVE, $flight is RF01
	# print "Project: $project Flight: $flight Platform: $platform ID: $id\n";
    # Project: GV System - DEEPWAVE Flight: RF01 Platform: none ID: none
    
	# this step not required for MPEX or DEEPWAVE
	# $id =~ s/677F/N677F/g;
    
	
	# $id was "none"
	$id = "N677F";
    # $platform = trim($platform);
	# $platform was "none"
	# NCAR_GV_N677F_
    $platform = "NCAR GV";
    $header->setType(trim($datatype));
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    $header->setId($id);
	# For DEEPWAVE: Only flights RF01-RF05 include flight 
	# number in the raw data header
	if ($flight)
	{
		print "FLIGHT: $flight\n";
		$header->setSite(sprintf("%s, %s %s",$platform,$id,$flight));
	}
	else
	{
		print "No Flight Info (RF01, etc.)\n";
		$header->setSite(sprintf("%s, %s",$platform,$id));
	}

    # Parse the location of the dropsonde.
    (split(/:/,$lines[4]))[1] =~ /\d+\s+[\d\.]+\'[WE],*\s+(\-?[\d\.]+)(\s+deg)?,\s+\d+\s+[\d\.]+\'[NS],*\s+(\-?[\d\.]+),?\s*(deg,*)?\s+([\d\.]+)/i;
    my ($lon,$lat,$alt) = ($1,$3,$5);

    $header->setLatitude($lat,$self->build_latlon_format($lat));
    $header->setLongitude($lon,$self->build_latlon_format($lon));
    
    $header->setAltitude($alt,"m");
    
    # Parse the date of the drop.
    $lines[5] =~ /(\d{4}, \d{2}, \d{2}), (\d{2}:\d{2}:\d{2})/;
    my ($date,$time) = ($1,$2);
	print "DATE $date  and TIME $time\n";
	# For DEEPWAVE: One file has a launch time of 30 Nov 1999
	if ($date =~ /1999/)
	{
		print "Fix date on this file $file\n";
		$date = "2014, 06, 24";
	}
    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    # Add the radiosonde information header line
	if ($lines[6] =~ /^Sonde/)
	{
		chomp $lines[6];
		my ($sonde_label, $id) = split(/:/,$lines[6]);
		my $sonde_type = "Vaisala RD94";
		$header->setLine(5, trim($sonde_label).":",trim(join("",$id,$sonde_type)));

	}

    # Add all other non-predefined header lines to the header.
    # for (my $i = 6; $i < 11; $i++) 
    for (my $i = 7; $i < 11; $i++) 
	{
		if ($lines[$i] !~ /^\s*\/\s*$/) 
		{
			my ($label,@data) = split(/:/,$lines[$i]);
            my $dataline = trim(join(":",@data));
	        unless ($dataline eq "" || $dataline eq ",") 
			{
				$header->setLine($i-1, trim($label).":",$dataline);
			}
		}
    }
    return $header;
}

##------------------------------------------------------------------------------
# @signature void parse_raw_files(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parse_raw_file {
    my ($self,$file) = @_;
    
    printf("Processing file: %s\n",$file);
    
    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);

    # Generated the header for the file.
    my $header = $self->parse_header($file,@lines[0..13]);
    
    # Only process the file if a header was generated.
    if (defined($header)) {

	# Determine the station that generated the sounding.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"});
	if (!defined($station)) {
	    $station = $self->build_default_station($header->getId(),$self->{"NETWORK"});
	    $self->{"stations"}->addStation($station);
	}
	$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

    # ----------------------------------------------------
	# Create the output file name and open the output file
	# ----------------------------------------------------
	my $outfile = sprintf("%s_%s_%04d%02d%02d%02d%02d%02d.cls",
	            	$self->{"NETWORK"},
                	$header->getId(),
                	split(/,/,$header->getActualDate()),
                	split(/:/,$header->getActualTime()));
	printf ("\tOutput file name is %s\n\n", $outfile);

	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");
	
	print($OUT $header->toString());
	
    # for DEEPWAVE file without correct times
    my $insert_time_zero = 1;
  	my $insert_time_one = 0;
    my $insert_time = 0.0;

	# Read in the records
	# my $index = @lines;
	my $index = 0;
	my @record_list = ();

	# foreach my $line (reverse(@lines)) {
	foreach my $line (@lines) {
        # $index--;
	    # Ignore the header lines.
	    # if ($index < 14) { last; }
	    if ($index < 14) { $index++; next; }


	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);
        
		# FOR DEEPWAVE ONLY
		if ($file =~ /^D20140624_064129/)
		{
		
			if ($insert_time_zero)
			{
				my $first_insert_time = -1.0;
				$record->setTime($first_insert_time);
				$insert_time_one = 1;
				$insert_time_zero = 0;
			}
			elsif ($insert_time_one)
			{
				$record->setTime($insert_time);
				$insert_time += 0.25;
    	
			}
        }
        else
		{
			$record->setTime($data[0]);
		}
	    $record->setPressure($data[4],"mb") if ($data[4] != -999);
	    $record->setTemperature($data[5],"C") if ($data[5] != -999);
	    $record->setDewPoint($data[6],"C") if ($data[6] != -999);
	    $record->setRelativeHumidity($data[7]) if ($data[7] != -999);
	    $record->setUWindComponent($data[8],"m/s") if ($data[8] != -999);
	    $record->setVWindComponent($data[9],"m/s") if ($data[9] != -999);
	    $record->setWindSpeed($data[10],"m/s") if ($data[10] != -999);
	    $record->setWindDirection($data[11]) if ($data[11] != -999);
	    $record->setAscensionRate($data[12],"m/s") if ($data[12] != -999);
	    
	    if ($data[14] != -999) 
		{
            $record->setLongitude($data[14],$self->build_latlon_format($data[14]));
	    }
	    if ($data[15] != -999) 
		{
			$record->setLatitude($data[15],$self->build_latlon_format($data[15]));
	    }
	   
	   $record->setAltitude($data[13],"m") if ($data[13] != -999);
	    
		push(@record_list, $record);
	    # printf($OUT $record->toString());
	}

    # Print the records to the file.
	foreach my $record (reverse(@record_list)) {
		print ($OUT $record->toString()) if (defined($record));
	}	
    }
}

##------------------------------------------------------------------------------
# @signature void print_station_files()
# <p>Generate the output files containing station information.</p>
##------------------------------------------------------------------------------
sub print_station_files {
    my ($self) = @_;

    open(my $STN, ">".$self->{"STATION_FILE"}) || 
	die("Cannot create the ".$self->{"STATION_FILE"}." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);
}

##------------------------------------------------------------------------------
# @signature void read_data_files()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub read_data_files {
    my ($self) = @_;
    
    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
    my @files = grep(/^D\d{8}_\d{6}.+\.eol/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
    foreach my $file (@files) {
	$self->parse_raw_file($file);
    }
    
    close($WARN);
}

##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove all leading and trailing whitespace from the specified String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed String.
##------------------------------------------------------------------------------
sub trim {
  my ($line) = @_;
  return $line if (!defined($line));
  $line =~ s/^\s+//;
  $line =~ s/\s+$//;
  return $line;
}
