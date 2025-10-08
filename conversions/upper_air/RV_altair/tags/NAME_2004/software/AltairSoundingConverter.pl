#! /usr/bin/perl -w

##Module-------------------------------------------------------------------
# <p>The AltairSoundingConverter.pl is a script for converting the Altair's 
# sounding data.  It is a wrapper for the <code>altair_ascii2class.pl</code>
# script.  It reads the files in the raw data directory and calls the 
# <code>altair_ascii2class.pl</code> script to actually convert the files 
# to CLASS format.</p>
#
# @author Joel Clawson
# @version NAME_1.0 This was originally created for the NAME project.
##Module-------------------------------------------------------------------
package AltairSoundingConverter;
use strict;
use lib "/work/software/NAME/library/conversion_modules/Version2";
use SimpleStationMap;
use Station;

&main();

# Constants
sub getConverter { return "../software/altair_ascii2class.pl"; }
sub getNetworkName { return "ALTAIR"; }
sub getRawDirectory { return "../raw_data"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "NAME"; }
sub getStationFile { 
    return sprintf("../final/%s_%s_stationCD.out",getNetworkName(),
		   getProjectName());
}
				    

##-------------------------------------------------------------------------
# @signature void main()
# <p>Convert
##-------------------------------------------------------------------------
sub main {
    my @sondes = get_unique();

    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());
    mkdir("../final") unless (-e "../final");

    chdir(getOutputDirectory()) or die("Cannot change to output directory.\n");
    foreach my $sonde (@sondes) {
	printf("Processing Sonde Files: %s...\n",$sonde);

	my $ptu = sprintf("%s/%s.PTU",getRawDirectory(),$sonde);
	my $wnd = sprintf("%s/%s.WND",getRawDirectory(),$sonde);

	if (-e $ptu && -e $wnd) {
	    system(sprintf("%s %s %s",getConverter(),$ptu,$wnd));
	} elsif (-e $ptu) {
	    system(sprintf("%s %s",getConverter(),$ptu));
	} else {
	    system(sprintf("%s %s",getConverter(),$wnd));
	}
    }

    create_station_list();
}

##-------------------------------------------------------------------------
# @signature void create_station_list()
# <p>Read the class files and generate a station list from it.</p>
##-------------------------------------------------------------------------
sub create_station_list {
    my $stations = SimpleStationMap->new();
    my $CLASS;

    # Only load the class files
    opendir($CLASS,getOutputDirectory()) or die("Cannot open output directory");
    my @files = grep(/\.cls$/,readdir($CLASS));
    closedir($CLASS);

    foreach my $file (@files) {
	my $FILE;
	open($FILE,sprintf("%s/%s",getOutputDirectory(),$file)) or
	    die(sprintf("Cannot open file: %s/%s\n",getOutputDirectory(),$file));
	
	# Get the Station id from the file name.
	$file =~ /^(\D+)/;
	my $id = $1;

	<$FILE>;<$FILE>; # Ignore the first two lines.

	# Get the name of the station.
	my $name = trim((split(/:/,<$FILE>))[1]);

	# Load the location information to be used later.
	my @location = split(/,/,trim((split(/:/,<$FILE>))[1]));
	foreach my $val (@location) { $val = trim($val); }

	my $station;
	if ($stations->hasStation($id,getNetworkName())) {
	    # Station already exists, so get it.
	    $station = $stations->getStation($id,getNetworkName());
	} else {
	    # Station needs to be created.
	    $station = Station->new($id,getNetworkName());
	    $station->setStationName($name);
	    
	    $station->setReportingFrequency("6 hourly");
	    $station->setStateCode("XX");
	    $station->setCountry("MX");
	    $station->setLatLongAccuracy(0);
	    $station->setMobilityFlag("m");
	    $station->setNetworkIdNumber(4);
	    $station->setPlatformIdNumber(306);
	    
	    $stations->addStation($station);
	}

	# Ignore these header lines.
	<$FILE>;<$FILE>;<$FILE>;<$FILE>;<$FILE>;<$FILE>;<$FILE>;

	# Update the date for the station with the nominal date of the file.
	my @date = split(/,/,trim((split(/:/,<$FILE>))[1]));
	$station->insertDate(sprintf("%04d/%02d/%02d",$date[0],$date[1],$date[2]));

	close($FILE);
    }

    printStationFiles($stations);
}

##-------------------------------------------------------------------------
# @signature String[] get_unique()
# <p>Get a list of the soundings that were taken based off of the file names.</p>
#
# @output files The list of unique sounding file times.
##-------------------------------------------------------------------------
sub get_unique {
    my $RAW;
    my @files = ();
    opendir($RAW,getRawDirectory()) or die("Cannot open raw directory\n");

    foreach my $file (sort(readdir($RAW))) {
	if ($file !~ /^\.+$/) {
	    $file =~ /^(\d+)\./;
	    if (scalar(@files) == 0 || $files[-1] != $1) { push(@files,$1); }
	}
    }
    close($RAW);
    return @files;
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $stations = shift;
    my ($STN);

    open($STN, ">".getStationFile()) || die("Cannot create the ".getStationFile()." file\n");
    foreach my $station ($stations->getAllStations()) {
        print($STN $station->toQCF_String()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);
}

##---------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the surrounding whitespace around a String.</p>
#
# @input $line The line to be trimmed.
# @output $line The trimmed line.
##---------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}
