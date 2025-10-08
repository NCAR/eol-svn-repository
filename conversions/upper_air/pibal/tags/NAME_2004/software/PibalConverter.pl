#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The PibalConverter.pl is a script that converts Pilot Balloon data into the
# CLASS format.</p>
#
# @author Joel Clawson
# @version NAME This was updated from pibal2class.pl originally written by
# Darren Gallant.
##Module------------------------------------------------------------------------
package PibalConverter;
use strict;
use lib "/work/software/NAME/library/conversion_modules/Version4";
use Sounding::ClassHeader;
use Sounding::ClassRecord;
use Station::SimpleStationMap;
use Station::Station;

my $WARN;
*STDERR = *STDOUT;

&main();

# Define constants to be used.
sub getCentury { return 2000; }
sub getFinalDirectory { return "../final"; }
sub getNetworkName { return "Pibal"; }
sub getOutputDirectory { return "../final"; }
sub getProjectName { return "NAME"; }
sub getProjectYear { return 2004; }
sub getRawDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getStationList { return "../docs/station.list"; }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##-------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the script.</p>
##-------------------------------------------------------------------------------
sub main {
    my $converter = PibalConverter->new();
    $converter->convert();
}

##-------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data into the CLASS format.</p>
##-------------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir(getOutputDirectory()) unless (-d getOutputDirectory());
    mkdir(getFinalDirectory()) unless (-d getFinalDirectory());

    open($WARN,">".getWarningFile()) or die("Cannot open the warning file.\n");

    $self->loadStations();
    $self->readRawDataFiles();
    $self->printStationFiles();

    close($WARN);
}

##---------------------------------------------------------------------------------------
# @signature int get_utc_offset(String stn)
# <p>Get the UTC offset for the specified station.</p>
#
# @input $stn The station id
# @output $offset The UTC offset for the station in standard time.
##---------------------------------------------------------------------------------------
sub get_utc_offset {
    my ($self,$stn) = @_;
    my $state = (split(' ',$stn))[-1];

    if ($state =~ /BCS/i || $state =~ /Chiha/i ||
        $state =~ /NY/i || $state =~ /Sin/i || $state =~ /Son/i) {
        return 7;
    } elsif ($state =~ /BC/i) { return 8; }
    else { return 6; }
}

##-------------------------------------------------------------------------------
# @signature void loadStations()
# <p>Read in the station information from the station list.</p>
##-------------------------------------------------------------------------------
sub loadStations {
    my $self = shift;

    open(my $STNS, $self->getStationList()) or die("Cannot read station list file.\n");

    while(<$STNS>) {
	my @data = split(",");

	my $station = Station::Station->new($data[0],$self->getNetworkName());
	$station->setStationName(sprintf("%s %s",$data[1],$data[3]));
	$station->setCountry($data[2]);
	$station->setStateCode($data[3]) if ($data[2] =~ /US/);

	my $lat_fmt = $data[4] < 0 ? "-" : "";
	while (length($lat_fmt) < length($data[4])) { $lat_fmt .= "D"; }
	my $lon_fmt = $data[5] < 0 ? "-" : "";
	while (length($lon_fmt) < length($data[5])) { $lon_fmt .= "D"; }
	$station->setLatitude($data[4],$lat_fmt);
	$station->setLongitude($data[5],$lon_fmt);
	$station->setElevation($data[6],"m");

	$station->setReportingFrequency("no set schedule");
	$station->setNetworkIdNumber(99);
	$station->setPlatformIdNumber(203);

	$self->{"stations"}->addStation($station);
    }

    close($STNS);
}

##-------------------------------------------------------------------------------
# @signature SMN_SND_Converter new()
# <p>Create a new converter that will handle the conversion process.</p>
##-------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = Station::SimpleStationMap->new();

    return $self;
}

sub parse_header_line {
    my ($self,$line,$header,$filename) = @_;
    my ($year,$month,$day,$hour,$min,$ampm,$balloon_color,$balloon_size,$asc_rate,$asc_unit,$notas,$lanzamiento_number,$balloon_loss);
    my ($debug_clause);
    my ($utc_flag,$id) = (0,substr($filename,12,2));
    chomp($line);
    my $original_line = $line;

#    printf("$line");

    # Remove some balloon information that confuses parsing
    $line =~ s/(color del )?globos?/ /gi;
    $line =~ s/balloon?s?/ /gi;
    $line =~ s/hora de lanzamiento/ /gi;

    if ($line =~ /W\=(\d+\.\d+)\s*([^\)]*)/i) {
	($asc_rate,$asc_unit) = ($1,$2);
	$line =~ s/\(W\=\d+\.\d+\s*[^\)]*\)//i;
    }
    if ($line =~ /(\d+)\s*(g|gr|grs|gms?|gram|gramos)[^a-z]/i) {
	$balloon_size = $1;
	$line =~ s/\d+\s*(gramos|gram|gms?|grs?|g)//i;
    }
    
    # Parse out balloon colors
    if ($line =~ /(rojoo?|red?)/i) {
	$balloon_color = "Red";
    } elsif ($line =~ /(blanco|white?)/i) {
	$balloon_color = "White";
    } elsif ($line =~ /(fiesta?)/i) {
	$balloon_color = "Party";
    } elsif ($line =~ /(RW)/) {
	$balloon_color = "RW";
    }

    # Parse out the notes
    if ($line =~ /not[ea]s?:?\s+(.+)$/i) {
	$notas = $1;
	$line =~ s/not[ea]s?:?\s+.+$/ /i;
    }

    # Parse out the release number
    if ($line =~ /lanzamiento:?\s+(\d+)/i) {
	$lanzamiento_number = $1;
	$line =~ s/lanzamiento:?\s+\d+/ /gi;
    } elsif ($line =~ /[\-]?\s*lanz?[\.\-]?\s*(\d+)\.?\s*[\-]?(\s*\d{1,2}:\d[\s])?/i) {
	$lanzamiento_number = $1;
	$line =~ s/[\-]?\s*lanz?[\.\-]?\s*(\d+)\.?\s*[\-]?(\s*\d{1,2}:\d[\s])?/ /i;
    }

    # Parse out the balloon loss information
    if ($line =~ /perdida:?\s+(.+)$/i) {
	$balloon_loss = $1;
	$line =~ s/perdida:?\s+.+$/ /i;
    }

    # Remove the date/time pattern to force a file read.
    $line =~ s/dd\/mm\/aaaa\s+hh:mm/ /i;

    # Handle time templates that were never filled out
    if ($line =~ /hh:mm/i) {
	my $h = sprintf("%02d",substr($filename,18,2));
	$line =~ s/hh:mm/$h:00/i;
	printf($WARN "No time for template hh:mm in file $filename.  Setting to $h:00\n");
    }

#    printf("%s\n",$line);

    # Convert a or p to am or pm when after a time
    $line =~ s/(\d{1,2}[:]\d{2})\s*([ap]\.?)\s+/$1 $2m/gi;

    $line =~ s/(vel\.\s*)?\d+\.\d+\.?\s*\.?m\/?s?e?g?/ /gi;
    $line =~ s/(vel|ascenso)\.?\s*\d+[\.,]\d+/ /gi;

    $line =~ s/\d{4}\.(tx|wi)/ /gi;
    
    $line =~ s/peso/ /gi;
    $line =~ s/ascent\s*rate\s*(\=|of)?\s*/ /gi;
    $line =~ s/\s+nubo[sc]idad:?\s+\d+\s*\/\s*(\d+)?/ /gi;
    $line =~ s/\s+nubo[sc]idad:?\s+(\d+)?/ /gi;
    $line =~ s/titul/ /gi;
    $line =~ s/rita\s*[y\/]\s*marcel/ /gi;
    $line =~ s/marcela?\s*[y\/]\s*rita?/ /gi;

    $line =~ s/\s+del?\s+/ /gi;
    $line =~ s/\,\s*a[^a-z]/ /gi;
    $line =~ s/ a / /gi;
    
    my $proj = getProjectName();
    $line =~ s/$proj/ /g;

    $line =~ s/x+/ /gi;

    $line =~ s/\"//gi;
    $line =~ s/[,;]/ /gi;
    $line =~ s/[,:\.]+\s+/ /gi;
    $line =~ s/\s+([,:\.]+)/ /gi;
    $line =~ s/\s+\-\s+/ /gi;
    $line =~ s/fecha/ /gi;
    $line =~ s/local\s+time/ /gi;
    $line =~ s/hora\s+local/ /gi;
    $line =~ s/(hs|hrs|horas?)[:\.]?/ /gi;
    $line =~ s/ h / /gi;
    $line =~ s/a\.?m\.?/am/gi;
    $line =~ s/p\.?m\.?/pm/gi;

    $line =~ s/am\s+am/am/gi;
    $line =~ s/pm\s+pm/pm/gi;

    # Remove balloon colors
    $line =~ s/(rojo|blanco)\s*\d+\.\d+\s+/ /gi;
    $line =~ s/rojoo?/ /gi;
    $line =~ s/blanco/ /gi;
    $line =~ s/fiesta?/ /gi;
    $line =~ s/red?/ /gi;
    $line =~ s/white?/ /gi;
    $line =~ s/color/ /gi;
    $line =~ s/ rw / /gi;

    $line =~ s/(\d+)[:\.](\d+)/$1:$2/gi;
    $line =~ s/(\d+)\/(\d+)\/(\d+)/$1\-$2\-$3/gi;
    $line =~ s/(\d+)[,](\d+)/$1 $2/gi;
    $line =~ s/(\d{2})\s+(\d{2})\s+(\d{2})/$1\-$2\-$3/gi;

    $line =~ s/ \. //gi;
    $line =~ s/h\s*$//gi;

    $line =~ s/\s+/ /g;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;

    
    # 9 Values: (Day) (Month - text) (Year - 4) (hour)[:](min) Z (hour)[:](min) T
    if ($line =~ /(\d{1,2})\s+([a-z]+)\s+(\d{4})\s+(\d{1,2})[:](\d{2})\s+[z]\s+(\d{1,2})[:](\d{2})\s+[t]$/i) {
	($day,$month,$year,$hour,$min,$utc_flag) = 
	    ($1,$self->parse_month($2),$3,$4,$5,1);
	$debug_clause = 18;
    }

    # 7 Values: (Day) (Month - text) (Year - 4) (hour)[:](min) [am|pm] (balloon_size)
    elsif ($line =~ /(\d{1,2})\s+([a-z]+)\s+(\d{4})\s+(\d{1,2}):?(\d{2})\s*(am|pm)?\s+(\d{1,2})$/i) {
	($day,$month,$year,$hour,$min,$ampm,$balloon_size) = 
	    ($1,$self->parse_month($2),$3,$4,$5,$6,$7);
	$debug_clause = 3;	
    }

    # 7 Values:  (Year - 4) (Month - text) (Day) (hour)[:](min) [am|pm] (balloon_size) 
    elsif ($line =~ /(\d{4})\s+([a-z]+)\s+(\d{1,2})\s+(\d{1,2}):?(\d{2})\s*(am|pm)?\s+(\d{1,2})$/i) {
	($year,$month,$day,$hour,$min,$ampm,$balloon_size) = 
	    ($1,$self->parse_month($2),$3,$4,$5,$6,$7);
	$debug_clause = 7;	
    }

    # 6 Values:  (Day) (Month - text) (Year - 4) (hour)[:](min) Z
    elsif ($line =~ /(\d{1,2})\s+([a-z]+)\s+(\d{4})\s+(\d{1,2}):?(\d{2})\s*z$/i) {
	($day,$month,$year,$hour,$min,$utc_flag) = ($1,$self->parse_month($2),$3,$4,$5,1);
	$debug_clause = 19;
    }

    # 6 Values:  (Day) (Month - text) (Year - 4) (hour)[:](min) [am|pm]
    elsif ($line =~ /(\d{1,2})\s+([a-z]+)\s+(\d{4})\s+(\d{1,2}):?(\d{2})\s*(am|pm)?$/i) {
	($day,$month,$year,$hour,$min,$ampm) = ($1,$self->parse_month($2),$3,$4,$5,$6);
	$debug_clause = 1;
    }

    # 6 Values:  (Day) (Month - text) (Year - 4) (hour)[:](min) [am|pm] - bad minute
    elsif ($line =~ /(\d{1,2})\s+([a-z]+)\s+(\d{4})\s+(\d{1,2})[:](\d{1})\s*(am|pm)?$/i) {
	($day,$month,$year,$hour,$min,$ampm) = ($1,$self->parse_month($2),$3,$4,$5,$6);
	printf($WARN "Only found 1 digit minute in time (%d:%d) in file %s. Setting minute to default of 0.\n",$hour,$min,$filename);
	$min = 0;
	$debug_clause = 11;
    }

    # 6 Values:  (Day)-(Month - text)-(Year-2) (hour)[:](min) [am|pm]
    elsif ($line =~ /(\d{1,2})\-([a-z]+)\-(\d{2})\s+(\d{1,2})[:]?(\d{2})\s*(am|pm)?$/i) {
	($day,$month,$year,$hour,$min,$ampm) = ($1,$self->parse_month($2),$3 + getCentury(),$4,$5,$6);
	$debug_clause = 17;
    }

    # 6 Values: (Month - text) (Day) (Year - 4) (hour)[:](min) [am|pm]
    elsif ($line =~ /([a-z]+)\s*(\d{1,2})\s+(\d{4})\s+(\d{1,2}):?(\d{2})\s*(am|pm)?$/i) {
	($month,$day,$year,$hour,$min,$ampm) = ($self->parse_month($1),$2,$3,$4,$5,$6);
	$debug_clause = 2;
    }

    # 6 Values:  (Year - 4) (Month - text) (Day) (hour)[:](min) [am|pm]
    elsif ($line =~ /(\d{4})\s+([a-z]+)\s+(\d{1,2})\s+(\d{1,2}):?(\d{2})\s*(am|pm)?$/i) {
	($year,$month,$day,$hour,$min,$ampm) = ($1,$self->parse_month($2),$3,$4,$5,$6);
	$debug_clause = 8;	
    }
    
    # 6 Values:  (Day)-(Month)-(Year - 2) (hour)[:](min) [am|pm]
    elsif ($id =~ /(ji|le|lr|ma)/i && $line =~ /(\d{1,2})[\-]?(\d{1,2})[\-]?(\d{2})\s+(\d{1,2})[:]?(\d{2})\s*(am|pm)?$/i) {
	($day,$month,$year,$hour,$min,$ampm) = ($1,$2,$3 + getCentury(),$4,$5,$6);
	$debug_clause = 14;
    }

    # 6 Values:  (Month)-(Day)-(Year-4) (hour)[:](min) [am|pm]
    elsif ($id =~ /(lh)/i && $line =~ /(\d{1,2})\-(\d{1,2})\-(\d{4})\s+(\d{1,2})[:]?(\d{2})\s+(am|pm)?$/i) {
	($month,$day,$year,$hour,$min,$ampm) = ($1,$2,$3,$4,$5,$6);
	$debug_clause = 15;
    }

    # 5 Values: (Month - text) (Day) (hour)[:](min) year (Year - 4)
    elsif ($line =~ /([a-z]+)\s+(\d{1,2})\s+(\d{1,2})[:]?(\d{2})\s+year\s+(\d{4})$/i) {
	($month,$day,$hour,$min,$year) = ($self->parse_month($1),$2,$3,$4,$5);
	$debug_clause = 12;
    }

    # 5 Values: (Month)-(Day)-(hour)[:](min) year (Year - 4)
    elsif ($line =~ /(\d{1,2})\-(\d{1,2})\-(\d{1,2})[:]?(\d{2})\s+year\s+(\d{4})$/i) {
	($month,$day,$hour,$min,$year) = ($1,$2,$3,$4,$5);
	$debug_clause = 13;
    }

    # 5 Values:  (Day)-(Month - text)-(Year - 2) (hour)[:](min)
    elsif ($line =~ /(\d{1,2})\-([a-z]+)\-(\d{2})\s+(\d{1,2})[:]?(\d{2})$/i) {
	($day,$month,$year,$hour,$min) = ($1,$self->parse_month($2),$3 + getCentury(),$4,$5);
	$debug_clause = 16;
    }

    # 5 Values: (Day) (Month - text) (hour)[:](min) (am|pm)
    elsif ($line =~ /(\d{1,2})\s+([a-z]+)\s+(\d{1,2})[:]?(\d{2})\s+(am|pm)$/i) {
	($day,$month,$hour,$min,$ampm) = ($1,$self->parse_month($2),$3,$4,$5);
	$year = getProjectYear();
	$debug_clause = 22;
    }

    # 4 Values: stn_id(DayMonthHour) (hour)[:](min) [am|pm]
    elsif ($line =~ /$id(\d{6})\s+(\d{1,2})[:]?(\d{2})\s*(am|pm)?$/i) {
	($day,$month,$hour,$min,$ampm) = (substr($1,0,2),substr($1,2,2),substr($1,4,2),$3,$4);
	$year = getProjectYear();
	if ($hour != $2 && ($min >= 30 && $2 + 1 != $hour)) {
	    printf($WARN "Hour mismatch within header of file %s: %d %d\n",$filename,$hour,$2);
	}
	$debug_clause = 4;
    }

    # 4 Values: (MonthDayYear-2) (hour)[:](min) [am|pm]
    elsif ($id =~ /hb/i && $line =~ /\s+(\d{6})\s+(\d{1,2})[:]?(\d{2})\s*(am|pm)?$/i) {
	($month,$day,$year,$hour,$min,$ampm) = (substr($1,0,2),substr($1,2,2),substr($1,4,2),$2,$3,$4);
	$year += getCentury();
	$debug_clause = 23;
    }

    # 4 Values: (DayMonthYear-2) (hour)[:](min) [am|pm]
    elsif ($line =~ /\s+(\d{6})\s+(\d{1,2})[:]?(\d{2})\s*(am|pm)?$/i) {
	($day,$month,$year,$hour,$min,$ampm) = (substr($1,0,2),substr($1,2,2),substr($1,4,2),$2,$3,$4);
	$year += getCentury();
	$debug_clause = 10;
    }

    # 3 Values: (Day) (Month - text) (Year - 4)
    elsif ($line =~ /(\d{1,2})\s+([a-z]+)\s+(\d{4})$/i) {
	($day,$month,$year) = ($1,$self->parse_month($2),$3);
	($hour,$min) = (substr($filename,18,2),0);
	$debug_clause = 20;
    }

    # 3 Values: (Day) (Month - text) (Year - 2)
    elsif ($line =~ /(\d{1,2})[\-\s]([a-z]+)[\-\s](\d{2})$/i) {
	($day,$month,$year) = ($1,$self->parse_month($2),$3 + getCentury());
	($hour,$min) = (substr($filename,18,2),0);
	$debug_clause = 21;
    }
    
    # 3 Values: (Day)-(Month)-(Year - 2)
    elsif ($id =~ /(lr)/i && $line =~ /(\d{1,2})[\-]?(\d{1,2})[\-]?(\d{2})$/i) {
	($day,$month,$year) = ($1,$2,$3 + getCentury());
	($hour,$min) = (substr($filename,18,2),0);
	$debug_clause = 16;
    }

    # 2 Values: (hour)[:](min)
    elsif ($line =~ /^[^\d]+(\d{1,2})[:]?(\d{2})$/i) {
	($hour,$min) = ($1,$2);
	($month,$day) = (substr($filename,14,2),substr($filename,16,2));
	$year = getProjectYear();
	$debug_clause = 9;
    }

    # 1 Value: stn_id(MonthDayHour).da
    elsif ($line =~ /$id\s*(\d{6})\.(da|ad)$/i) {
	($month,$day,$hour) = (substr($1,0,2),substr($1,2,2),substr($1,4,2));
	($year,$min) = (getProjectYear(),0);
	$debug_clause = 5;
    }

    # Empty Header of No Digit
    elsif ($line =~ /^\s*$/ || $line !~ /\d/i) {
	if (substr($filename,12,3) =~ /p5(5|6)/i) {
	    ($month,$day) = (substr($filename,15,2),substr($filename,17,2));
	    ($hour,$min) = substr($filename,19,1) == 1 ? (13,30) : (23,30);
	} else {
	    ($month,$day,$hour) = (substr($filename,14,2),substr($filename,16,2),substr($filename,18,2));
	    $min = 0;
	}
	$year = getProjectYear();
	$debug_clause = 6;
    }

    #printf("Debug Clause: %d!\n",$debug_clause);

    # Adjust hour to 24 hour if it is in 12 hour
    if (defined($ampm) && $ampm =~ /pm/i && $hour < 12) { $hour += 12; }

    if (getProjectName() =~ /NAME/ && $year == 2005) {
	$year = 2004;
    }


    if (substr($filename,12,3) =~ /p5(5|6)/i) {
	if (substr($filename,15,2) != $month || substr($filename,17,2) != $day) {
	    print($WARN "Filename did not match data in header ($filename) - Month: $month Day: $day\n");
	}
    } else {
	if (substr($filename,14,2) != $month || substr($filename,16,2) != $day) {
	    print($WARN "Filename did not match data in header ($filename) - Month: $month Day: $day Hour: $hour\n");
	} elsif (substr($filename,18,2) == $hour) {
	    # Do nothing
	} elsif ($min < 30) {
	    if (substr($filename,18,2) != $hour) {
		if (substr($filename,18,2) == $hour + 1 && $min > 0) {
		    # Do Nothing
		} else {
		    print($WARN "Filename did not match data in header ($filename) - Month: $month Day: $day Hour: $hour\n");
		}
	    }
	} else {
	    if (substr($filename,18,2) != $hour + 1) {
		print($WARN "Filename did not match data in header ($filename) - Month: $month Day: $day Hour: $hour\n");
	    }
	}
#	} elsif ((substr($filename,18,2) != $hour && $min < 30) && (substr($filename,18,2) != $hour+1 && $min >= 30)) {
#	    print($WARN "Filename did not match data in header ($filename) - Month: $month Day: $day Hour: $hour\n");
#	}
    }

    if ($utc_flag) {
	$header->setActualRelease(sprintf("%04d%02d%02d",$year,$month,$day),"YYYYMMDD",
				  sprintf("%02d%02d",$hour,$min),"HHMM",0);
    } else {
	$header->setActualRelease(sprintf("%04d%02d%02d",$year,$month,$day),"YYYYMMDD",
				  sprintf("%02d%02d",$hour,$min),"HHMM",
				  $self->get_utc_offset($header->getSite()));
    }
    $header->setNominalRelease($header->getActualDate(),"YYYY, MM, DD",
			       $header->getActualTime(),"HH:MM:SS",0);

    $header->setLine("Assumed Ascension Rate:",sprintf("%.2f %s",$asc_rate,$asc_unit)) if (defined($asc_rate) && defined($asc_unit));
    $header->setLine("System Operator/Comments:",$notas) if (defined($notas));
    $header->setLine("Perdida:",$balloon_loss) if (defined($balloon_loss));
    $header->setLine("Lanzamiento:",sprintf("%d",$lanzamiento_number)) if (defined($lanzamiento_number));
    
    my $balloon_data = "";
    $balloon_data .= sprintf("Color: %s",$balloon_color) if (defined($balloon_color));
    $balloon_data .= sprintf("Size: %s grams",$balloon_size) if (defined($balloon_size));
    $header->setLine("Balloon Data:",$balloon_data) if ($balloon_data ne "");

}

sub parse_month {
    my ($self,$month) = @_;

    return 4 if ($month =~ /abril|april|apr/i);
    return 5 if ($month =~ /mayo|may/i);
    return 6 if ($month =~ /junio|june?/i);
    return 7 if ($month =~ /julio|july?/i);
    return 8 if ($month =~ /agost?o|august|aug|ago/i);
    return 9 if ($month =~ /septiembre|september|sep/i);

    die("Unrecognized month: $month\n");
}

sub parse_unit_line {
    my ($self,$line) = @_;
    chomp($line);
    my $units = {};

    my @data = split(' ',$line);

    my $index = 0;
    foreach my $value (@data) {
	
	if ($value =~ /t\[s(eg)?\]/i) { $units->{"time"} = $index; }
	elsif ($value =~ /zm\[m\]/i) { $units->{"alt"} = $index; }
	elsif ($value =~ /dir\[o\]/i) { $units->{"dir"} = $index; }
	elsif ($value =~ /mag\[m\/s\]/i) { $units->{"spd"} = $index; }
	elsif ($value =~ /u\[m\/s\]/i) { $units->{"ucomp"} = $index; }
	elsif ($value =~ /v\[m\/s\]/i) { $units->{"vcomp"} = $index; }
	elsif ($value =~ /zt\[m\]/i) {}
	elsif ($value =~ /[xy]\[m\]/i) {}
	else { die("Unknown value in unit line: $value\n"); }

	$index++;
    }

    return $units;
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $self = shift;
    my ($STN, $SUMMARY);

    open($STN, ">".$self->getStationFile()) || 
	die("Cannot create the ".$self->getStationFile()." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open($SUMMARY, ">".$self->getSummaryFile()) || 
	die("Cannot create the ".$self->getSummaryFile()." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

##-------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Read in the files that contain the raw data to be converted and convert each
# file individually.</p>
##-------------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,$self->getRawDirectory()) or die("Cannot open raw directory.\n");
    my @files = grep(/\.WIN$/i,readdir($RAW));
    closedir($RAW);

    # Loop through files in the station directory.
    foreach my $file (sort(@files)) {
	$self->readRawFile($file);
    }
}

##-------------------------------------------------------------------------------
# @signature void readRawFile(String stn_id, String file_name)
# <p>Parse the data in the specified file and convert it to the CLASS format.</p>
#
# @input $stn_id The station id for the data in the file.
# @input $file_name The name of the file containing the raw data.
##-------------------------------------------------------------------------------
sub readRawFile {
    my $self = shift;
    my $file_name = shift;

    my $file = sprintf("%s/%s",$self->getRawDirectory(),$file_name);

    printf("Processing file: %s\n",$file_name);
    #printf("Processing file: %s ",$file_name);

    my $station_id = substr($file_name,0,2);
    $station_id = substr($file_name,0,3) if ($station_id eq "P5");
    my $station = $self->{"stations"}->getStation($station_id,$self->getNetworkName());

    # Define the header part of a class file.
    my $header = Sounding::ClassHeader->new($WARN,$station);
    $header->setType("PIBAL");
    $header->setProject($self->getProjectName());

    open(my $FILE,$file) or die("Cannot open file: $file\n");

    my $line = <$FILE>;
    $self->parse_header_line($line,$header,$file);
    $station->insertDate($header->getActualDate(),"YYYY, MM, DD");

    $line = <$FILE>;
    my $indicies = $self->parse_unit_line($line);


    # Define the output file.
    my $file_time = sprintf("%s%s",$header->getActualDate(),$header->getActualTime());
    $file_time =~ s/[,\s:]//g;
    my $filename = sprintf("%s/%s_%s.cls",getOutputDirectory(),$station_id,substr($file_time,0,12));

#    printf(" %s\n",$filename);

    open(my $OUT,sprintf(">%s",$filename)) or die("Can't open output file.\n");
    printf($OUT "%s",$header->toString());

    my $previous_record;
    while (<$FILE>) {
	$line = $_;
	chomp($line);

#	printf("%s\n",$line);
	my @data = split(' ',$line);

	my $record = Sounding::ClassRecord->new($WARN,$filename,$previous_record);
	
	$record->setTime($data[$indicies->{"time"}],0);
	$record->setAltitude($data[$indicies->{"alt"}],"m");
	$record->setUWindComponent($data[$indicies->{"ucomp"}],"m/s");
	$record->setVWindComponent($data[$indicies->{"vcomp"}],"m/s");
	$record->setWindDirection($data[$indicies->{"dir"}]);
	$record->setWindSpeed($data[$indicies->{"spd"}],"m/s");

	printf($OUT "%s",$record->toString());
	$previous_record = $record;
    }


    close($OUT);
    close($FILE);

    if (-z $filename) {
	printf($WARN "%s: File has zero size and is being removed.\n",$filename);
	unlink($filename);
    }
}

##-------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the surrounding whitespace of a String.
#
# @input $line The line to be trimmed.
# @output $line The trimmed line.
##-------------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}
