#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The SanJose_SND_Converter.pl script is used for converting the sounding data
# from the San Jose site in Costa Rica from the raw Vaisala variant to the 
# CLASS format.</p>
#
# @author Joel Clawson
# @version NAME_1.0 This was originally created for the NAME project.
##Module-------------------------------------------------------------------------
package LeMoore_SND_Converter;
use strict;
use lib "/work/software/TREX/library/conversion_modules/Version5";
use Sounding::ClassHeader;
use Sounding::ClassRecord;

my ($WARN);
*STDERR = *STDOUT;

&main();

# A collection of functions that contain constants
sub getNetworkName { return "LeMoore"; }
sub getOutputDirectory { return "."; }
sub getProjectName { return "T-REX"; }

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = LeMoore_SND_Converter->new();
    $converter->convert($ARGV[0]);
}

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert all of the raw data and create the output files for the conversion.</p>
##------------------------------------------------------------------------------
sub convert {
    my ($self,$file) = @_;

    $WARN = *STDOUT;

    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());

    $self->readRawFile($file);
}

##------------------------------------------------------------------------------
# @signature int getMonth(String abbr)
# <p>Get the number of the month from an abbreviation.</p>
#
# @input $abbr The month abbreviation.
# @output $mon The number of the month.
# @warning This function will die if the abbreviation is not known.
##------------------------------------------------------------------------------
sub getMonth {
    my $self = shift;
    my $month = shift;
    
    if ($month =~ /MAR/i) { return 3; }
    elsif ($month =~ /APR/i) { return 4; }
    elsif ($month =~ /MAY/i) { return 5; }
    elsif ($month =~ /JUN/i) { return 6; }
    elsif ($month =~ /JUL/i) { return 7; }
    elsif ($month =~ /AUG|agosto/i) { return 8; }
    elsif ($month =~ /SEP/i) { return 9; }
    else { die("Unknown month: $month\n"); }
}

##------------------------------------------------------------------------------
# @signature SanJose_SND_Converter new()
# <p>Create a new SanJose_SND_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    return $self;
}

##------------------------------------------------------------------------------
# @signature void readData(FileHandle FILE)
# <p>Read in the raw data from the file pointer and convert it to CLASS format
# by placing it in the ClassSounding object that will hold it.
#
# @input $FILE The file handle containing the data.
##------------------------------------------------------------------------------
sub readData {
    my $self = shift;
    my ($FILE,$OUT,$filename,$header) = @_;

    # Read blank lines before data.
    my $line = <$FILE>; while ($line =~ /^\s*$/) { $line = <$FILE>; }

    my @search_lines = <$FILE>;

    my @sfc_lines = grep(/^\s*0\s+0\s+/,@search_lines);
    my $record;
    foreach my $line (@sfc_lines) {
	if (!defined($record)) {
	    $record = Sounding::ClassRecord->new($WARN,$filename);
	    print($OUT $header->toString());
	}

	my @data = split(' ',$line);
	if (@data < 9) {
	    $record->setWindDirection($data[4]);
	    $record->setWindSpeed($data[5],"knot");
	} else {
	    $record->setTime($data[0],$data[1]);
	    $record->setAltitude($data[2],"m");
	    $record->setPressure($data[3],"hPa");
	    $record->setTemperature($data[4],"C");
	    $record->setRelativeHumidity($data[5]);
	    $record->setDewPoint($data[6],"C");

	    my $lat = $header->getLatitude();
	    my $lon = $header->getLongitude();

	    my $lat_fmt = $lat < 0 ? "-" : "";
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    my $lon_fmt = $lon < 0 ? "-" : "";
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	    
	    $record->setLatitude($lat,$lat_fmt);
	    $record->setLongitude($lon,$lon_fmt);
	}
    }

    printf($OUT $record->toString()) if (defined($record));


    # Read in the data
    my $start = 1;
    my $previous_record;

    if (defined($record)) {
	$start = 0;
	$previous_record = $record;
    }

    my $line_length = 0;
    while ($line) {
	chomp($line);

	if (!$start && $line =~ /^\s*$/) { last; }


	# Ignore blank lines.
	if ($line =~ /^\s*$/) { 
	    #$line = <$FILE>; 
	    $line = shift(@search_lines);
	    next;
	}

	my @data = split(' ',$line);


	my $record = Sounding::ClassRecord->new($WARN,$filename,$previous_record);
	$record->setTime($data[0],$data[1]) if ($data[0] !~ /\/+/ && $data[1] !~ /\/+/);
	$record->setAscensionRate($data[2],"m/s") if ($data[2] !~ /\/+/ && !$start);
	$record->setAltitude($data[3],"m") if ($data[3] !~ /\/+/);
	$record->setPressure($data[4],"hPa") if ($data[4] !~ /\/+/);
	$record->setTemperature($data[5],"C") if ($data[5] !~ /\/+/);
	$record->setRelativeHumidity($data[6]) if ($data[6] !~ /\/+/);
	$record->setDewPoint($data[7],"C") if ($data[7] !~ /\/+/);
	$record->setWindDirection($data[8]) if ($data[8] !~ /\/+/);
	$record->setWindSpeed($data[9],"knot") if ($data[9] !~ /\/+/);

	# Set the first line of the latitude and longitude to the station data.
	if ($start) {
	    my $lat = $header->getLatitude();
	    my $lon = $header->getLongitude();

	    my $lat_fmt = $lat < 0 ? "-" : "";
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    my $lon_fmt = $lon < 0 ? "-" : "";
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	    
	    $record->setLatitude($lat,$lat_fmt);
	    $record->setLongitude($lon,$lon_fmt);

	    $start = 0;

	    print($OUT $header->toString());
	}

	print($OUT $record->toString());

#	last;

	$previous_record = $record;
#	$line = <$FILE>;
	$line = shift(@search_lines);
    }



}

##------------------------------------------------------------------------------
# @signature ClassSounding readHeader(FileHandle FILE, String file)
# <p>Read in the header information from the file handle.</p>
#
# @input $FILE The FileHandle containing the data to be read.
# @input $file The name of the file being read.
# @output $cls The ClassSounding holding the header data.
##------------------------------------------------------------------------------
sub readHeader {
    my $self = shift;
    my ($FILE,$file) = @_;

    my $header = Sounding::ClassHeader->new();

    my $line = <$FILE>;
    # Loop until the last line of the header is reached.
    while ($line !~ /^\s*min\s+s\s+m\/s\s+m/) {
	chomp($line);
	$line = trim($line);
	
	# These are to be put into the class header.
	if ($line =~ /^sounding\s*program.+using\s+(.+)\s*$/i) {
	    $header->setLine("Wind Finding Methodology:",$1) if (defined($1));
	} elsif ($line =~ /^location\s*[:\.]?\s*([\d\.]+)\s*([NS])\s+([\d\.]+)\s*([EW])\s+([\d\.]+)\s*(\S+)/i) {
	    
	    # Get the values from the matching.
	    my ($lat,$lat_unit,$lon,$lon_unit,$elev,$elev_unit) =
		($1,uc($2),$3,uc($4),$5,lc($6));
	    $lat =~ s/:/./g;
	    $lon =~ s/:/./g;
	    $elev_unit = "m" if ($elev_unit eq "mts");
	    
	    my ($lat_fmt,$lat_inc,$lat_mult);
	    if ($lat_unit =~ /N/i) {
		$lat_fmt = "";
		$lat_mult = 1;
	    } else {
		$lat_fmt = "-";
		$lat_mult = -1;
	    }
	    $lat *= $lat_mult;
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    $header->setLatitude($lat,$lat_fmt);


	    my ($lon_fmt,$lon_inc,$lon_mult);
	    if ($lon_unit =~ /E/i) {
		$lon_fmt = "";
		$lon_mult = 1;
	    } else {
		$lon_fmt = "-";
		$lon_mult = -1;
	    }
	    $lon *= $lon_mult;
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	    $header->setLongitude($lon,$lon_fmt);
	    
	    $header->setAltitude($elev,$elev_unit);

	    if ($self->getProjectName() eq "T-REX") {
		$header->setLatitude("36.20","DDDDD");
		$header->setLongitude("-119.57","-DDDDDD");
		$header->setAltitude(72,"m");
	    }

	} elsif ($line =~ /^station\s*:?\s*(.+)/i) {
	    $header->setId("NAS");
	    $header->setSite(trim($1));
	} elsif ($line =~ /started\s+at\s*[:\.]?\s*(\d+)\s*([\D\S]+)\s*(\d+)?\s+(\d+)\D(\d+)/i) {
	    my $year = defined($3) ? $3 : getProjectName() eq "T-REX" ? 6 : 99;
	    my $date = sprintf("%04d%02d%02d",2000+$year,$self->getMonth($2),$1);
	    my $time = sprintf("%02d%02d",$4,$5);
	    $header->setActualRelease($date,"YYYYMMDD",$time,"HHMM",0);
	} elsif ($line =~ /start\s+up(\s+date)?\s*:?\s*(\d+)\s+([\D\S]+)\s*(\d+)?\s+(\d+)\D(\d+)/i) {
	    my $year = defined($4) ? $4 : getProjectName() eq "T-REX" ? 6 : 99;
	    my $date = sprintf("%04d%02d%02d",2000+$year,$self->getMonth($3),$2);
	    my $time = sprintf("%02d%02d",$5,$6);
	    $header->setActualRelease($date,"YYYYMMDD",$time,"HHMM",0);	    
	} elsif ($line =~ /^\s*(\d+)\s+de\s+(\w+)\s+del\s+(\d+)/i) {
	    my $date = sprintf("%04d%02d%02d",$3,$self->getMonth($2),$1);
	    $header->setActualRelease($date,"YYYYMMDD",$header->getActualTime(),
				      "HH:MM:SS",0);
	} elsif ($line =~ /^\s*sondeo\s+.*(\d+)z/i) {
	    my $time = sprintf("%02d00",$1);
	    $header->setActualRelease($header->getActualDate(),"YYYY, MM, DD",$time,"HHMM",0);
	} elsif ($line =~ /^(rs.+)?numb?er\s*(at)?\s*:?\s*(.?\d+)/i) {
	    $header->setLine("Radiosonde Serial Number:",$3);
	} elsif ($line =~ /radiosond[ea]\s+model\s*:?\s*([\w\-]+)/i || 
		 $line =~ /(rs\s*\d+.*)/i) {
	    $header->setLine("Radiosonde Model:",$1);
	} elsif ($line =~ /soundin?g\s*:?\s*(\d+)/i) {
	    $header->setLine("Ascension No:",$1);
	} elsif ($line =~ /^\s*$/ || $line =~ /Time AscRate/ ||
		 $line =~ /system\s*test/i || $line =~ /ground\s*check/i ||
		 $line =~ /(pressure|temperature|humidity)/i ||
		 $line =~ /(\d+\s+)+/ || $line =~ /signal\s+strength/i ||
		 $line =~ /continued/i || $line =~ /\.txt/i || $line =~ /\d+z/i) {
	} elsif ($line =~ /datos perdidos por cortes de corriente/i) {
	    $header->setLine("System Notes:","Data missing from power failure.");
	} elsif ($line =~ /^\s*(([a-zA-Z0-9]{4,6})+\s*)+\s*$/) {
	    printf($WARN "%s: No header terminator (unit line) was found.  No output will be created.\n",$file);
	    return undef();
	} elsif ($line =~ /(\d+)\/(\d+)\/(\d+)/) {
	    $header->setActualRelease(sprintf("%04d/%02d/%02d",$3,$2,$1),"YYYY/MM/DD",
				      sprintf("%02d:00",substr($file,6,2)),"HH:MM",0);
	} else {
	    printf("Header line: %s not recognized\n",$line);
	    die();
	}
    

	$line = <$FILE>;

	return undef() if (!$line);
    }

    # Define the new CLASS formatted sounding.
    $header->setType("LeMoore Sounding");
    $header->setProject($self->getProjectName());


    $header->setNominalRelease($header->getActualDate(),"YYYY, MM, DD",
			       $header->getActualTime(),"HH:MM:SS",0,0);


    return $header;
}

##------------------------------------------------------------------------------
# @signature void readRawFile(String file_name)
# <p>Convert the specified file into the CLASS format.</p>
#
# @input $file_name The name of the raw data file to be converted.
##------------------------------------------------------------------------------
sub readRawFile {
    my ($self,$file) = @_;
#    my $file_name = shift;
#    my $file = sprintf("%s/%s",$self->getRawDirectory(),$file_name);

    printf("Processing file: %s\n",$file);

    open(my $FILE,$file) or die("Cannot open file: $file\n");

    my $header = $self->readHeader($FILE,$file);

    if (defined($header)) {

	my $filename = sprintf("%s_%04d%02d%02d%02d%02d.cls",$header->getId(),
			       split(/, /,$header->getNominalDate()),
			       split(/:/,$header->getNominalTime()));

	open(my $OUT, sprintf(">%s/%s",getOutputDirectory(),$filename)) or
	    die("Cannot open $filename\n");
	
	$self->readData($FILE,$OUT,$filename,$header);
	
	close($OUT);
#	die();
    } else {
#	printf($WARN "No header data for file: %s.  Not creating output file.\n",$file);
    }
	

    close($FILE);
}

##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the leading and trailing whitespace around a String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed line.
##------------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}
