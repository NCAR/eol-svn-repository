#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>This script converts the NOAA P3 aircraft Dropsonde *.frd files into ESC format.</p>
#
# @author Joel Clawson
# @version 1.0 This was originally created for the RAINEX project.
#
# @author L. Cully
# @version Updated May 2008 by L. Cully to use latest sounding libraries.
#  Now placed output *.cls files in the /output dir not the final dir.
#  Note that the *stationCD.out file is still being placed in the ./final dir.
#
# BEWARE: This s/w assumes the raw *.frd data in /raw_data/ directories.
##Module------------------------------------------------------------------------
package NOAA_P3_Converter;
use strict;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
    use lib "/net/work/lib/perl/Station";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
    use lib "/work/lib/perl/Station";
}
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

use SimpleStationMap;
use Station;

my ($WARN);

printf "\nNOAA_P3_Converter.pl began on ";print scalar localtime;
&main();
printf "\nNOAA_P3_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the NOAA dropsonde data by converting it from the native ASCII
# format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = NOAA_P3_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature NOAA_P3_Converter new()
# <p>Create a new instance of a NOAA_P3_Converter.</p>
#
# @output $self A new NOAA_P3_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"PROJECT"} = "RAINEX";
    $self->{"NETWORK"} = "NOAA_P3";
    
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
# <p>Create a default station for the network using the specified 
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub build_default_station {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);

    $station->setStationName(sprintf("%s - %s",$network,$station_id));
    $station->setStateCode("FL");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(37);
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
# @signature void convert()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub convert {
    my ($self) = @_;
    
    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    mkdir($self->{"FINAL_DIR"}) unless (-e $self->{"FINAL_DIR"});
    
    $self->read_data_files($self->{"RAW_DIR"});

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
    my ($self,$file,$storm,@lines) = @_;
    my $header = ClassHeader->new();
    $header->setProject($self->{"PROJECT"});
    $header->setType("NOAA P3 Dropsonde");
    $header->setReleaseDirection("Descending");

    # Pull out the tail number of the aircraft.
    $lines[3] =~ /Aircraft:\s+(\S+)/;
    $header->setId($1);
    $header->setSite(sprintf("%s %s/%s",$self->{"NETWORK"},$1,$storm));

    # Pull out the release location
    $lines[15] =~ /Lat:\s+(\d+\.\d+)\s+([NS])/i;
    my $lat = lc($2) eq "s" ? -1 * $1 : $1;
    my $lat_fmt = $lat < 0 ? "-" : "";
    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
    $header->setLatitude($lat,$lat_fmt);

    $lines[16] =~ /Lon:\s+(\d+\.\d+)\s+([EW])/i;
    my $lon = lc($2) eq "w" ? -1 * $1 : $1;
    my $lon_fmt = $lon < 0 ? "-" : "";
    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
    $header->setLongitude($lon,$lon_fmt);

    # Pull out the sounding id
    $lines[17] =~ /SID:\s+(\d+)/;
    $header->setLine(5,"Sonde Id:",$1);

    # Pull out the comments
    $lines[11] =~ /COMMENTS:\s+(.+)$/i;
    $header->setLine(10,"Comments:",trim($1));

    # Pull out the splash pressure
    $lines[7] =~ /Splash PR\s+=\s+(\-?\d+\.\d+\s+[A-Za-z\/]+)/;
    my $splash_pressure = $1;
    $splash_pressure =~ s/\s+/ /g;
    $header->setLine(8,"Splash Pressure:",$splash_pressure);

    # Pull out the hydrostatic surface pressure
    $lines[8] =~ /HYD\s+SFCP\s+=\s+(\-?\d+\.\d+\s+[A-Za-z\/]+)/;
    my $hs_sfc_press = $1;
    $hs_sfc_press =~ s/\s+/ /g;
    $header->setLine(9,"Hydrostatic Surface Pressure:",$hs_sfc_press);

    # Pull out the bias corrections
    $lines[5] =~ /Bias\s+corrections:\s+(.+)$/;
    my $bias = $1;
    $bias =~ s/\s+/ /g;
    $bias =~ s/(\.\d+\s+[A-Za-z\/\%]+)\s+/$1, /g;
    $header->setLine(6,"Bias Corrections:",trim($bias));

    # Pull out the release flight level data.
    $lines[15] =~ /PS:\s*(\-?\d*(\.\d+)?\s+[A-Za-z\/]+)/;
    my $pressure = $1;
    $pressure =~ s/\s+/ /g;
    $lines[15] =~ /TA:\s*(\-?\d*(\.\d+)?\s+[A-Za-z\/]+)/;
    my $temp = $1;
    $temp =~ s/\s+/ /g;
    $lines[17] =~ /RH:\s*(\-?\d*(\.\d+)?\s+[A-Za-z\/\%]+)/;
    my $rh = $1;
    $rh =~ s/\s+/ /g;
    $lines[15] =~ /WD:\s*(\-?\d*(\.\d+)?\s+[A-Za-z\/]+)/;
    my $wind_dir = $1;
    $wind_dir =~ s/\s+/ /g;
    $lines[16] =~ /WS:\s*(\-?\d*(\.\d+)?\s+[A-Za-z\/]+)/;
    my $wind_spd = $1;
    $wind_spd =~ s/\s+/ /g;

    $header->setLine(7,"Release Flight Level Data:",
		     sprintf("PR = %s, TA = %s, RH = %s, WD = %s, WS = %s",
			     $pressure,$temp,$rh,$wind_dir,$wind_spd));

    #------------------------------------------------------------------
    # BEWARE: the output date for the Nominal Release may need a space
    #         added at the beginning of the year. 
    #   See /net/work/lib/perl/UpperAir/ClassHeader.pm
    #------------------------------------------------------------------

    # Pull out the date and time
    $lines[15] =~ /Date:\s+(\d{6})\s+/;
    my $date = sprintf("20%s",$1);
    $lines[16] =~ /Time:\s+(\d{6})\s+/;
    my $time = $1;
    $header->setActualRelease($date,"YYYYMMDD",$time,"HHMMSS",0);
    $header->setNominalRelease($date,"YYYYMMDD",$time,"HHMMSS",0);

    return $header;
}

##------------------------------------------------------------------------------
# @signature int parse_flag(int flag)
# <p>Parse the source flag into a ESC flag.</p>
#
# @input $flag The source flag to be parsed.</p>
# @output $flag The ESC flag parsed from the source flag.</p>
##------------------------------------------------------------------------------
sub parse_flag {
  my ($self,$flag) = @_;

  if ($flag == 0) { return $GOOD_FLAG; }
  elsif ($flag == 3) { return $ESTIMATE_FLAG; }
  elsif ($flag == 4) { return $QUESTIONABLE_FLAG; }
  elsif ($flag == 5) { return $UNCHECKED_FLAG; }
  else {
    printf("Unknown source flag: %s\n",$flag);
    exit(1);
  }
}

##------------------------------------------------------------------------------
# @signature void parse_raw_files(String directory, String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $directory The directory where the file to be parsed is located.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parse_raw_file {
    my ($self,$directory,$file) = @_;
    
    printf("Processing file: %s\n",$file);
    
    open(my $FILE,$directory."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);

    $directory =~ /\/([^\/]+)$/;

    # Generated the header for the file.
    my $header = $self->parse_header($file,$1,@lines[0..20]);
   
    if (defined($header)) 
      { print ("Header defined.\n"); }
    else
      { print ("Header Undefined. Do NOT process this file.\n"); }

 
    # Only process the file if a header was generated.
    if (defined($header)) {

      my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"});
      if (!defined($station)) {
	$station = $self->build_default_station($header->getId(),$self->{"NETWORK"});
	$self->{"stations"}->addStation($station);
      }
      $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

      open(my $OUT,sprintf(">%s/%s_%04d%02d%02d%02d%02d%02d.cls",
			   $self->{"OUTPUT_DIR"},$header->getId(),    # WAS FINAL
			   split(/,/,$header->getActualDate()),
			   split(/:/,$header->getActualTime()))) or
			     die("Can't open class file for $file\n");

      my @records = ();
      for (my $i = @lines - 1; $i > 20; $i--) {

	my @data = split(' ',$lines[$i]);

	my $record = ClassRecord->new($WARN,$file,@records ? $records[-1] : undef());
	$record->setTime($data[1]);
	$record->setPressure($data[2],"mb") unless ($data[2] == -999);
	$record->setTemperature($data[3],"C") unless ($data[3] == -999);
	$record->setRelativeHumidity($data[4]) unless ($data[4] == -999);
	$record->setAltitude($data[5],"m") unless ($data[5] == -999);
	$record->setWindDirection($data[6]) unless ($data[6] == -999);
	$record->setWindSpeed($data[7],"m/s") unless ($data[7] == -999);
	$record->setUWindComponent($data[8],"m/s") unless ($data[8] == -999);
	$record->setVWindComponent($data[9],"m/s") unless ($data[9] == -999);

	$record->setPressureFlag($self->parse_flag($data[13])) unless ($record->getPressure() == 9999);
	$record->setTemperatureFlag($self->parse_flag($data[14])) unless ($record->getTemperature() == 999);
	$record->setRelativeHumidityFlag($self->parse_flag($data[15])) unless ($record->getRelativeHumidity() == 999);
	my $wind_flag = $self->parse_flag($data[16]);
	$record->setUWindComponentFlag($wind_flag) unless ($record->getUWindComponent() == 9999);;
	$record->setVWindComponentFlag($wind_flag) unless ($record->getVWindComponent() == 9999);
	
	my $lat = $data[17];
	my $lat_fmt = $lat < 0 ? "-" : "";
	while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	$record->setLatitude($lat,$lat_fmt) unless ($lat == -999);
	
	my $lon = $data[18];
	my $lon_fmt = $lon < 0 ? "-" : "";
	while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	$record->setLongitude($lon,$lon_fmt) unless ($lon == -999);

	# Set the starting data point location as the release location
	if ($i == 21) {
	  $header->setLatitude($lat,$lat_fmt) unless ($lat == -999);
	  $header->setLongitude($lon,$lon_fmt) unless ($lon == -999);
	  $header->setAltitude($record->getAltitude(),"m");
	}

        push(@records,$record);
      }

      print($OUT $header->toString());
      foreach my $record (@records) {
        print($OUT $record->toString());
      }

      close($OUT);
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
    my ($self,$directory) = @_;
    
    opendir(my $RAW,$directory) or die("Can't read raw directory: $directory\n");
    my @files = sort(readdir($RAW));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
    foreach my $file (@files) {
      if (-d sprintf("%s/%s",$directory,$file) && $file !~ /^\.+$/) {
	$self->read_data_files(sprintf("%s/%s",$directory,$file));
      } elsif ($file =~ /\.frd/) {
	$self->parse_raw_file($directory,$file);
      }
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
