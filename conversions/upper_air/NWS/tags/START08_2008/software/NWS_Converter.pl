#! /usr/bin/perl -w

#----------------------------------------------------------
# NWS_Converter.pl
#  This s/w converted the NWS (MicroArt) sounding data.
#  Believe it was originally written by Kendal Southwick.
#  Darren Gallant took it over from Kendal. Possibly a
#  variety of students worked on this s/w. Joel Clawson
#  then took the s/w over from Darren and made upgrades
#  until he left NCAR in spring 2008.  
#
# Created Circa 1990's by Kendal Southwick.
# Updated Circa late 1990's by Darren Gallant.
# Updated Circa early 2000's by Joel Clawson.
#
# Updated Spring 2009 - L. Cully.
#   General cleanup. Added header with best guess info.
#   Updated to use latest libs and calls. Changed "nice"
#   calls to vaisala and viz from -19 to -5. Updated
#   project to be T-PARC. Changed to print Error in
#   processTxtFile() where check on "CHS Charleston" use
#   to "die". Now prints error message. Search for HERE
#   for issues with the station info for state and country
#   codes for the whole world. Added debug. Set $debug=1
#   for output or to zero for none. Search for "Check HERE"
#   for more work that needs to be done on station info.
#   Also note that there's "numerically()" fn that apparently
#   is never called, but this should be verified before deleting.
#   There are other lines commented out by previous progrogrammers
#   but not sure why. These should be investigated.
#
# August 2009 - L. Cully
#   Updated project to process PLOWS_2008-2009 data.
# September 2009 - L. Cully
#   Updated project to process ICE-L_2007 data.
#   Changed "nice" to be -3.
# October 2009 - L. Cully
#   Updated project to process VORTEX2_2009 data.
#   Changed "nice" to be -3.
# November 2009 - L. Cully
#   Updated project to process START08 data.
#
#  BEWARE: If a stn is not listed in the station_id() list,
#      this s/w bombs. Always ensure that all new stations
#      are included.
#  BEWARE: Search for "HARDCODED" and reset the project
#      name to the correct project before running.
#----------------------------------------------------------

use strict;

use lib "../lib"; 

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
    use lib "/net/work/lib/perl/Station";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
    use lib "/work/lib/perl/Station";
}

use Formats::Class qw(:DEFAULT &windqc %WINDQC &calc_WND);

use DpgCalculations;
use DpgDate;

use Station;
use ElevatedStationMap;

use ClassHeader;
use ClassRecord;

use Time::Local; 

$! = 1;

#----------------------------------------------------------
# Define the programs that convert the data to CLASS format
#----------------------------------------------------------
my $VAISALA = "/usr/bin/nice -3 ./nwsVaisala"; 
my $VIZ = "/usr/bin/nice -3 ./nwsVIZ";        

#---------------------------------------------------------------------
# Define the abbreviations for the file name for the known WMO Numbers
#---------------------------------------------------------------------
my %station_id = ("14929"=>"abr","54775"=>"alb","23050"=>"abq",
"23047"=>"ama","24011"=>"bis","53829"=>"rnk","24131"=>"boi","12919"=>"bro",
"14733"=>"buf","14607"=>"car","04833"=>"ilx","13880"=>"chs","94983"=>"mpx",
"14684"=>"chh","12924"=>"crp","22010"=>"drt","23062"=>"dnr","03160"=>"dra",
"13985"=>"ddc","04105"=>"lkn","03020"=>"epz","53103"=>"fgz","03990"=>"fwd",
"04837"=>"apx","94008"=>"ggw","23066"=>"gjt","54762"=>"gyx","04102"=>"tfx",
"14898"=>"grb","13723"=>"gso","14918"=>"inl","03940"=>"jan","13889"=>"jax",
"12850"=>"eyw","03937"=>"lch","03952"=>"lzk","24225"=>"mfr","92803"=>"mfl",
"23023"=>"maf","13897"=>"bna","93768"=>"mhx","03948"=>"oun","24023"=>"lbf",
"23230"=>"oak","53819"=>"ffc","94823"=>"pit","94982"=>"dvn","94240"=>"uil",
"94043"=>"rap","03198"=>"rev","24061"=>"riw","24232"=>"sle","24127"=>"slc",
"03190"=>"nkx","53823"=>"bmx","13957"=>"shv","53813"=>"sil","04106"=>"otx",
"13995"=>"sgf","93734"=>"lwx","93805"=>"tlh","12842"=>"tbw","13996"=>"top",
"23160"=>"tus","94703"=>"okx","94980"=>"oax","13841"=>"iln","04830"=>"dtx",
"27502"=>"brw","11641"=>"sju",
"40308"=>"tya", "40504"=>"ttp", "40309"=>"tkr", "40505"=>"tkk", 
"40710"=>"kmr", "21504"=>"hto", "22536"=>"hli", "41406"=>"gum"
);

#-------------------------
# Define Global variables.
#-------------------------
my ($TRUE,$FALSE) = (1,0);
my ($INTERP,$REMOVE,$UNCHECK,$AVG,$TIME_LIMIT,$SMOOTH);
my %LIMIT;
my $STATIONS = ElevatedStationMap->new();

my $debug = 0;

#----------------------------------------------------------------------------------
# Removes Bad thu entire sounding, Quest in first 360 seconds and interpolates thur 
#----------------------------------------------------------------------------------
my $WIND_FLAG = $TRUE;

#-----------------------------------------
# Define directories and files to be used.
#-----------------------------------------
sub getFinalDirectory() { return "../final"; }
sub getLogDirectory() { return "../logs"; }
sub getOutputDirectory() { return "../output"; }
sub getRawDirectory() { return "../raw_data"; }

sub getNetworkName() { return "NWS"; }
sub getProjectName() { return "START08"; }  # HARDCODED Project Name

sub getStationFile() { return sprintf("%s/%s_%s_sounding_stationCD.out",getFinalDirectory(),
				      getNetworkName(),getProjectName()); }
sub getSummaryFile() { return "../output/station_summary.log"; }
sub getWarningFile() { return "warning.log"; }

my $WARN_LOG;

&main();

##-----------------------------------------------------------------------
# @signature void main()
# <p>Convert the raw data into the CLASS format.</p>
##-----------------------------------------------------------------------
sub main {
    if ($debug){ print "Begin Main\n"; }

    #----------------------------------------
    # Create output directories as necessary.
    #----------------------------------------
    if ($debug){ print "Make Dirs\n"; }

    mkdir(getLogDirectory()) unless (-e getLogDirectory());
    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());
    mkdir(getFinalDirectory()) unless (-e getFinalDirectory());

    #----------------------------------------------------
    # Setup variables to be used for the wind calculation
    #----------------------------------------------------
    if ($debug){ print "Setup hardcoded wind calc processing vars.\n"; }

    if($WIND_FLAG){
	$INTERP = $TRUE;
	$REMOVE = $TRUE;
	$UNCHECK = $TRUE;
	$AVG = 60;
	$TIME_LIMIT = 4*$WINDQC{"INTERVAL"};
	$LIMIT{BAD} = 10000;
	$LIMIT{QUEST} = 359;
	$LIMIT{INTERP} = 359;
	$SMOOTH = 60;
    }else{
	$INTERP = $TRUE;
	$REMOVE = $TRUE;
	$UNCHECK = $TRUE;
	$AVG = 60;
	$TIME_LIMIT = 4*$WINDQC{"INTERVAL"};
	$LIMIT{BAD} = 10000;
	$LIMIT{QUEST} = 0;
	$LIMIT{INTERP} = 120;
	$SMOOTH = 60;
    }
 
    if ($debug){ print "Open dirs for processing. Log and Raw\n"; }

    open($WARN_LOG,sprintf(">%s/%s",getLogDirectory(),getWarningFile())) or
	die("Cannot open warning file.\n");
    
    opendir(my $RAW,getRawDirectory()) or die("Cannot open raw directory.\n");
    my @files = readdir($RAW);
    closedir($RAW);

    #----------------------------------------
    # Process the files in the raw directory.
    #----------------------------------------
    if ($debug){ print "Process all the files in the Raw dir. Files:: @files\n"; }
    foreach my $file (sort(@files)) {

	processTxtFile($file) if ($file =~ /\.txt$/);

	processFile($file) if ($file =~ /^\d{7}\.\d{2}(\.\d+)?(\.gz)?$/);
    }

    close($WARN_LOG);

    if ($debug){ print "Cat *.asc.log into nws.log file. Rm the asc.log file.\n"; }
    system(sprintf("cat *.asc.log > %s/nws.log",getLogDirectory()));
    system("rm *.asc.log");

    if ($debug){ print "Open the StationFile. Output stns to this file.\n"; }
    open(my $STN, ">".getStationFile()) || die("Cannot create the ".getStationFile()." file\n");
    foreach my $station ($STATIONS->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    if ($debug){ print "Open the SummaryFile. Output summary to file.\n"; }
    open(my $SUMMARY, ">".getSummaryFile()) || die("Cannot create the ".getSummaryFile()." file.\n");
    print($SUMMARY $STATIONS->getStationSummary());
    close($SUMMARY);

    print "End Main\n";

} # main()

##-----------------------------------------------------------------------
# @signature int check_ascension_numbers(String infile, String outfile)
# <p>Find the ascension numbers in the files and compare them to see
# if the infile should replace the outfile.</p>
#
# @input $infile The new file to compare.
# @input $outfile The old file to compare.
# @output $comp A true value if the new file can replace the old file.
##-----------------------------------------------------------------------
sub check_ascension_numbers {
    my $infile = shift;
    my $outfile = shift;

    #----------------------------------------------
    # Only compare if the old file actually exists.
    #----------------------------------------------
    if (-e sprintf("%s/%s",getOutputDirectory(),$outfile)) {

        #----------------------------------------------
	# Pull the ascension numbers from the files.
        #----------------------------------------------
	my $new_ascension = getAscensionNumber($infile);
	my $old_ascension = getAscensionNumber($outfile);

        #-------------------------------------------------------------
	# Want to return true if the old number is smaller or the
	# same as the new one.  We want to keep the largest ascension
	# number.
        #-------------------------------------------------------------
	return ($old_ascension <= $new_ascension);
    }

    return 1;
}

##-----------------------------------------------------------------------
# @signature void check_wind(* array_ref, * wnd_ref, * stat_ref, * intervale_ref)
# <p>Check the wind values.</p>
# <p>I am not entirely sure what this function is doing exactly.  It comes
# from Darren's software and has not been modified much (except to change
# some variable names and little things like that to make it more 
# understandable).  - Joel</p>
##-----------------------------------------------------------------------
sub check_wind {
    my($array_ref,$wnd_ref,$stat_ref,$interval_ref) = @_;
    my(@OUTFILE);

    if ($debug) {print "Enter check_wind\n"; }

    $WINDQC{"INTERVAL"} = $$interval_ref;

    foreach my $line (&windqc($array_ref,$wnd_ref,$stat_ref)) {
	my @input = split(' ',$line);

	# Check for Bad flagged wind components
	if($input[$field{Qu}]==3.0 && $input[$field{Qv}]==3.0 && $REMOVE){
	    unless($input[$field{time}] > $LIMIT{BAD}){
		foreach my $parem("Dir","Spd","Ucmp","Vcmp"){
		    $input[$field{$parem}] = $MISS{$parem}
		}
		foreach my $parem("Qu","Qv"){
		    $input[$field{$parem}] = 9.0;
		}
	    }
        }

	# Check for Questionable wind components
        if($input[$field{Qu}]==2.0 && $input[$field{Qv}]==2.0 && $REMOVE){
	    unless($input[$field{time}] > $LIMIT{QUEST}){
		foreach my $parem("Dir","Spd","Ucmp","Vcmp"){
		    $input[$field{$parem}] = $MISS{$parem}
		}
		foreach my $parem("Qu","Qv"){
		    $input[$field{$parem}] = 9.0;
	        }
	    }
        }

        push(@OUTFILE,&line_printer(@input));	
    }
    if ($debug) {print "Exit check_wind\n"; }
    return @OUTFILE;
}

##-----------------------------------------------------------------------
# @signature void convert(String data, String outfile)
# <p>Convert the data into CLASS format.</p>
#
# @input $data The data to be converted.
# @input $outfile The output file of the converted data.
##-----------------------------------------------------------------------
sub convert {
    my $data = shift;
    my $outfile = shift;

    if ($debug) {print "Enter convert:: outfile = xxx $outfile xxx\n"};
    if ($debug) {print "Enter convert:: data = xxx $data xxx\n"};

    if(grep(/Incorrect record length for DR_(VZPR|VZCAL|MET6)/i,$data)){ 
        if ($debug) {print "Incorrect rec length\n"};
	print $WARN_LOG "$outfile contains errors - Can't process (Incorrect record length for DR)\n";
	print "$outfile contains errors - Can't process\n";

    }elsif(!grep(/6 Second Met Data/,$data)){
        if ($debug) {print "No data\n"};
	print $WARN_LOG "$outfile does not contain data - Can't process (6 Second Met Data)\n";
	print "$outfile does not contain data - Can't process\n";

    }else{

        #--------------------------------------------------------------------
	# Need to save the data to a file so it can be read by the conversion
	# programs.
        #--------------------------------------------------------------------
        if ($debug) {print "Open outfile and output data and close.\n"};

	open(my $FILE,">$outfile") or die("Cannot write: $outfile\n");
	print($FILE $data);
	close($FILE);
        if ($debug) {print "-------> $data <-------\n"};
	
        #----------------------------------------------------------
	# Convert the data to the CLASS format.
        # Define the programs that convert the data to CLASS format.
        #
        # where $VAISALA = "/usr/bin/nice -8 ./nwsVaisala";
        # and VIZ = "/usr/bin/nice -8 ./nwsVIZ";
        #----------------------------------------------------------
        if ($debug) {print "System call to convert the data to CLASS format. Either call nwsVaisala or nwsVIZ. Outfile = $outfile \n"; }

	if ( 1 ) {
           if (grep(/UNKNOWN/i,$data) )
              { print "Calling VAISALA\n"; }
           else
              { print "Calling VIZ\n"; }
           }

	if ($debug) {
               my $cmd = sprintf("%s %s",grep(/UNKNOWN/i,$data) ? $VAISALA : $VIZ, $outfile); 
               print "Execute:: $cmd \n";  }

	system(sprintf("%s %s",grep(/UNKNOWN/i,$data) ? $VAISALA : $VIZ, $outfile));

        #---------------------------------------------------------
	# Remove the original ASCII file.  It is no longer needed.
        #---------------------------------------------------------
        if ($debug) {print "Remove outfile\n"};
	unlink($outfile);

        #---------------------------------------------------------
	# Correct the wind parameters in the CLASS file.
        #---------------------------------------------------------
        if ($debug) {print "convert:: call correct_winds()\n"; }
	correct_winds();

    if ($debug) {print "Exit convert\n"; }
    }

} # convert()


##-----------------------------------------------------------------------
# @signature void correct_winds()
# <p>Run the wind correction algorithm on the CLASS files.  This should only
# pick up the most recent file, since all of the files are removed once
# they are used.  But, it will pick up any CLASS files in the current 
# directory, so if the converter would crash and the previous file was not
# removed, it will convert that as well.</p>
##-----------------------------------------------------------------------
sub correct_winds {

    if ($debug) {print "Enter correct_winds()\n"; }

    #---------------------------------------------
    # Get the list of files in the current directory.
    #---------------------------------------------
    opendir(my $CURR,".") or die("Cannot read current directory.\n");
    my @files = grep(/\.cls$/,readdir($CURR));
    closedir($CURR);    

    #---------------------------------------------
    # Loop through all of the CLASS files
    #---------------------------------------------
    foreach my $file (@files) {

        #---------------------------------------------
	# Create the final file name from the CLASS file name.
        #---------------------------------------------
	my $outfile = sprintf("%s/%s",getOutputDirectory(),$file);

        #--------------------------------------------------------------------
	# Only correct winds if this file has a smaller ascension number than
	# a previous run.
        #--------------------------------------------------------------------
	if (!check_ascension_numbers($file,$outfile)) {
	    unlink($file);
	    return;
	}

        #----------------------------------------------
	# Create the log files for the wind correction.
        #----------------------------------------------
	my $wndfile = sprintf("%s/%s.wnd",getLogDirectory(),$file);
	my $statfile= sprintf("%s/%s.stat",getLogDirectory(),$file);
	
        #---------------------------------------------
	# Open the files needed
        #---------------------------------------------
	open(my $FILE,$file) || die "Can't open $file\n";
        open(my $OUT,">$outfile") || die "Can't open $outfile\n";
        open(my $WND,">$wndfile") || die "Can't open $wndfile\n";
	open(my $STAT,">$statfile") || die "Can't open $statfile\n";

	my @OUTFILE;

        #---------------------------------------------
	# Get a list of the data from the file.
        #---------------------------------------------
	my $station = Station->new();
	my $date = "00000000";

	for my $line (<$FILE>) {
	    if($line =~ /(\-|)\d+\.\d+/ && $line !~ /[a-zA-Z\/]/){

                if ($debug) {print "Output line:: $line\n"; }
		push(@OUTFILE,$line);

	    }else{
                if ($debug) {print "(hdr Info: Output line:: $line\n"; }
                if ($debug) {print "(Reset the Project ID at this point. Everything else is set.\n"; }

		if ($line =~ /Project ID:/) {$line = "Project ID:                        ".getProjectName()."\n";}

		print $OUT $line;
		
		if ($line =~ /Release Site Type/) {

		    my @data = split(' ',$line);

                    #---------------------------------------------
                    # Need to handle the case where a space was not used.
                    #---------------------------------------------
                    if (length($data[-1]) != 2) { $data[-1] = substr($data[-1],length($data[-1])-2); }

                    if ($debug) {print "Set StnInfo:: Country State Code::  $data[-1]\n"; } 

                    #------------------------------------------------------------------------------
                    # Check HERE all info for station.
                    # Issue with country and state codes now that processing data from whole world.
                    # This section needs to be reworked to handle the world, if possible.
                    # PR = Puerto Rico.
                    #------------------------------------------------------------------------------
                    # Orig Code:
		    # $station->setCountry($data[-1] eq "PR" ? $data[-1] : "US"); 
		    # $station->setStateCode($data[-1] eq "PR" ? "XX" : $data[-1]);
                    #------------------------------------------------------------------------------
                    #-------------------------------------------------
                    # This check should be the other way around.
                    # Check for known states, otherwise set to missing.
                    #--------------------------------------------------
                    if ($data[-1] ne "CI" && $data[-1] ne "Is" && $data[-1] ne "PR") 
                       { 
                       $station->setCountry ("US");
                       $station->setStateCode($data[-1]);
                       }
                    else # Set to Missing state code for everything else.
                      {
                      $station->setCountry ("99"); # Consider: $station->setCountry ("$data[-1]");
                      $station->setStateCode("99");
                      }

                    $station->setStationId($data[4]);
                    $station->setStationName(trim((split(/:/,$line))[1]));

                    $station->setNetworkName(getNetworkName());
                    $station->setNetworkIdNumber(15);
                    $station->setPlatformIdNumber(53);
                    $station->setReportingFrequency("12 hourly");

		} elsif ($line =~ /Release Location/) {

                    if ($debug) {print "Set the release location\n"; }

		    my @data = split(' ',$line);
		    $station->setElevation($data[-1],"m");

		    my $lat = $data[-2]; $lat =~ s/,//g;
		    my $lon = $data[-3]; $lon =~ s/,//g;

		    my $lat_fmt = $lat < 0 ? "-" : "";
		    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
		    my $lon_fmt = $lon < 0 ? "-" : "";
		    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }

		    $station->setLatitude($lat,$lat_fmt);
		    $station->setLongitude($lon,$lon_fmt);
		    $station->setLatLongAccuracy(1);

		} elsif ($line =~ /Nominal Release Time/) {

                    if ($debug) {print "Set the release time\n"; }

		    $line =~ /(\d+), (\d+), (\d+), \d+:\d+:\d+/;
		    $date = sprintf("%04d%02d%02d",$1,$2,$3);
		}
	    }
	}
	close($FILE);

	if ($STATIONS->hasStation($station->getStationId(),$station->getNetworkName(),
				  $station->getLatitude(),$station->getLongitude(),
				  $station->getElevation())) {
	    $station = $STATIONS->getStation($station->getStationId(),
					     $station->getNetworkName(),
					     $station->getLatitude(),$station->getLongitude(),
					     $station->getElevation());
	} else {
	    $STATIONS->addStation($station);
	}
	$station->insertDate($date,"YYYYMMDD");

        #----------------------------
	# Correct the wind parameters
        #----------------------------
        @OUTFILE = &check_wind(\@OUTFILE,\$WND,\$STAT,\$AVG);
	if($INTERP){
	    my $parem = "Ucmp";
	    my @y2U = &spline(\@OUTFILE,\$parem);
	    @OUTFILE = &splint(\@OUTFILE,\@y2U,\$parem);
	    
	    $parem = "Vcmp";
	    my @y2V = &spline(\@OUTFILE,\$parem);
            if(scalar(@y2U) > 10 && scalar(@y2V) > 10){
		@OUTFILE = &splint(\@OUTFILE,\@y2V,\$parem);
	    }
	}

        #----------------------------
	# Write the data to the file.
        #----------------------------
        &writefile(\@OUTFILE,\$OUT);

	close($WND);
	close($STAT);
	close($OUT);

        #-----------------------------------------------------------------------
	# Remove the old CLASS file to prevent confusion with the corrected wind
	# file and because it is no longer needed.
        #-----------------------------------------------------------------------
	unlink($file);
    }
} # correct_winds()

##-----------------------------------------------------------------------
# @signature int getAscensionNumber(String file)
# <p>Get the ascension number of a sounding in CLASS file format.</p>
#
# @input $file The name of the file in CLASS format.
# @output $number The ascension number in the file.
##-----------------------------------------------------------------------
sub getAscensionNumber {
    my $file = shift;

    if ($debug) {print "Enter getAscensionNumber\n"; }

    # Get the Ascension Number line from the file.
    open(my $FILE,$file);
    my @line = grep(/Ascension No/,<$FILE>);
    close($FILE);
    
    # Parse out the ascension number and return it.
    $line[0] =~ /(\d+)/;

    if ($debug) {print "Exit getAscensionNumber\n"; }

    return $1;

} # getAscensionNumber()


##-----------------------------------------------------------------------
# numerically() - apparently never called. (LEC)
##-----------------------------------------------------------------------
####sub numerically { $a <=> $b; }


##-----------------------------------------------------------------------
# @signature void processFile(String file)
# <p>Convert a raw data file into individual sounding files in CLASS format.</p>
#
# @input $file The raw data file to be converted.
##-----------------------------------------------------------------------
sub processFile {
    my $input_file = shift;
    my $wban = substr($input_file,0,5);

    if ($debug){ print "Enter processFile\n"; }
    if ($debug){ print "Processing RAW file: $input_file ...\n"; }

    open(my $FILE,sprintf("gzcat %s/%s|",getRawDirectory(),$input_file)) || 
	die "Can't open $input_file\n";


    my $miss_ASC = 9000;
    my $file_cnt = 0;
    
    my $out = "";
    my $outfile = "";
    my ($ASC,$beg_file) = (0,1);

    if ($debug){ print "Create header\n"; }
    my %header;

    #-------------------------------------------
    # Loop through all of the lines in the file.
    #-------------------------------------------
    if ($debug){ print "Loop thru file\n"; }
    foreach my $line (<$FILE>) {
        #------------------------------------------------------
	# Remove the new line character at the end of the line.
        #------------------------------------------------------
	chomp($line);
        if ($debug){ print "Processing line:: $line\n"; }

	if($line =~ /WMO NUM/){
            #-------------------------------------------
	    # Marks the beginning of a new sounding
            #-------------------------------------------
            if ($debug){ print "Found WMO Num - begin new sounding\n"; }

            #------------------------------------------------------------------
	    # All of the data has been gathered for the previous sounding so it
	    # is now ready to be converted.
            #------------------------------------------------------------------
            if ($debug) {print "processFile:: Call convert\n"; }
	    if($file_cnt){ convert($out,$outfile); }

	    $line =~ tr/A-Za-z#:\///d;	
	    $ASC = (split(' ',$line))[4];
            if ($debug){ print "split line: ASC:: $ASC\n"; }

            #------------------------------------------------------------------
	    # Make sure there is an ascension number for the sounding
            #------------------------------------------------------------------
	    if($ASC !~ /\d{4}/){
		$ASC = $miss_ASC;
		$miss_ASC++;
                if ($debug){ print "Bad or missing ascension number\n"; }
		print $WARN_LOG "Bad/missing ascension number\n";
		print $WARN_LOG "Using $ASC\n";
	    }

            #---------------------------------------
	    # Reset the variables for the new file.
            #---------------------------------------
	    $outfile = sprintf("%s%s.asc",$station_id{$wban},$ASC);
            if ($debug) {
                 print "Form outfile ($outfile) name: wban, ASC, station_id(wban):: $wban, $ASC, xxx $station_id{$wban} xxx\n";}

	    $out = "";
	    %header = ();
	    $beg_file = 0;

            $file_cnt++;

	    $header{$ASC}{"beg_line"} = $line;

            if ($debug){ print "Create outfile ($outfile), file_cnt= $file_cnt, header(ASC)=xxx $line xxx\n"; } 

	} elsif ($line =~ /File:/) {
            #-------------------------------------------------------
	    # Marks the beginning of the data for the current file.
            #-------------------------------------------------------
            if ($debug) {print "processFile:: found begin of data: $line\n"; }

	    $out .= "\n$line\n";
	    $beg_file = 1;
	} elsif($beg_file) {
            #--------------
	    # Save the data
            #--------------
            if ($debug) {print "processFile:: Save data to out= $line\n"; }
	    $out .= $line."\n";
	}

        #-------------------------------------------------------
	# Used for the output to the warning file.
	# Not sure if it used anymore.
        #-------------------------------------------------------
        if ($debug) {print "processFile:: Output for warning file?\n"; }
	if($line =~ /Ascension:/){
	    $header{$ASC}{"asc"} = (split(' ',$line))[1];
	}elsif($line =~ /Created:/){
	    $header{$ASC}{"created"} = $line;
	}elsif($line =~ /Index:/){
	    $header{$ASC}{"wmo"} = substr($line,0,32);
	}elsif($line =~ /Date:/ && $line !~ /Mfg./){
	    $header{$ASC}{"date"} = $line;
	}elsif($line =~ /Hour:/){
	    $header{$ASC}{"hour"} = $line;
	}elsif($line =~ /Ascension No.:/){
	    $header{$ASC}{"asc_no"} = $line;
	}
    }

    close($FILE);

    if($file_cnt) { 
        if ($debug) {print "processFile:: Call convert( $out , $outfile ) - end of processFile()\n"; }
	convert($out,$outfile); 
	
    }

if ($debug) {print "Exit processFile()\n"; }

} # processFile()

#-------------------------------------------------------
#  processTxtFile()
#-------------------------------------------------------
sub processTxtFile {
    my ($file) = @_;
    my $stn_id = substr($file,0,3);
    my $asc_num = substr($file,3,4);

    if ($debug) { print "Enter processTxtFile()\n"; }

    my $manufact = {};
    $manufact->{"006"} = "Sippican";
    my $type = {};
    $type->{"012"} = "VIZ Mark II MICROSONDE";

    printf("%s: %d\n",$stn_id,$asc_num);

    my $has_header = 1;

    open(my $HEAD,sprintf("%s/H%d",getRawDirectory(),$asc_num)) or $has_header = 0;

    if (!$has_header) {
      printf("Cannot open header file for $asc_num\n");
      return;
    }

    my $headLine = <$HEAD>;
    close($HEAD);

    my $lat = sprintf("%s%s",substr($headLine,13,1) =~ /n/i ? "" : "-",substr($headLine,9,4));
    my $lon = sprintf("%s%s",substr($headLine,19,1) =~ /e/i ? "" : "-",substr($headLine,14,5));

    if (!defined($manufact->{substr($headLine,49,3)})) {
	die("Manufacturer Code "+substr($headLine,49,3)+" not known.");
    }
    if (!defined($type->{substr($headLine,52,3)})) {
	die("Sonde Type Code "+substr($headLine,52,3)+" not known.");
    }

    my $header = ClassHeader->new($WARN_LOG);

    if ($stn_id eq "CHS") {
	$header->setSite("CHS Charleston, SC");
    } else {
	print "ERROR: Unknown station id: $stn_id\n"; # Changed from "die"
    }

    $header->setType("National Weather Service Sounding");

    $header->setProject(getProjectName());

    $header->setLatitude($lat,$lat < 0 ? "-DDMM" : "DDMM");
    $header->setLongitude($lon,$lon < 0 ? "-DDDMM" : "DDDMM");
    $header->setAltitude(substr($headLine,20,4),"m");

    my $actDate = (adjustDateTime(substr($headLine,24,8),"YYYYMMDD","00:00","HH:MM",substr($headLine,34,4) >= 2330 ? -1 : 0,0,0,0))[0];

    $header->setActualRelease($actDate,"YYYYMMDD",substr($headLine,34,4),"HHMM",0);
    $header->setNominalRelease(substr($headLine,24,8),"YYYYMMDD",substr($headLine,32,2),"HH",0);
    $header->setLine(6, "Ascension Number:",substr($headLine,38,4));
    $header->setLine(7, "Sonde Manufacturer:",$manufact->{substr($headLine,49,3)});
    $header->setLine(8, "Sonde Type:",$type->{substr($headLine,52,3)});
    $header->setLine(5, substr($headLine,55,1) ? "Baroswitch Number:" : "Sonde Serial Number:",
		     trim(substr($headLine,56,20)));
    my $filename = sprintf("%s_%04d%02d%02d%02d%02d.cls",$stn_id,split(", ",$header->getActualDate()),
			   split(":",$header->getActualTime()));


    my $station = $STATIONS->getStation($stn_id,getNetworkName(),$header->getLatitude(),$header->getLongitude(),
					$header->getAltitude());

    if (!defined($station)) {
	$station = Station->new();
	my $lat_fmt = $header->getLatitude() < 0 ? "-" : "";
	while (length($lat_fmt) < length($header->getLatitude())) { $lat_fmt .= "D"; }
	my $lon_fmt = $header->getLongitude() < 0 ? "-" : "";
	while (length($lon_fmt) < length($header->getLongitude())) { $lon_fmt .= "D"; }
	
	$station->setLatitude($header->getLatitude(),$lat_fmt);
	$station->setLongitude($header->getLongitude(),$lon_fmt);
	$station->setElevation($header->getAltitude(),"m");

	my @data = split(' ',$header->getSite());
	
	# Need to handle the case where a space was not used.
	if (length($data[-1]) != 2) { $data[-1] = substr($data[-1],length($data[-1])-2); }
	
	$station->setCountry($data[-1]);  
        $station->setStateCode("XX"); 
	$station->setStationId($stn_id);
	$station->setStationName($header->getSite());

        #------------------------------------------------------------------------------
        # Check HERE all info for station. NEED TO UPDATE THIS SECTION.
        # Issue with country and state codes now that processing data from whole world.
        # PR = Puerto Rico.
        #------------------------------------------------------------------------------
        # Orig Code:
        $station->setCountry($data[-1] eq "PR" ? $data[-1] : "US");  
        $station->setStateCode($data[-1] eq "PR" ? "XX" : $data[-1]);
        $station->setStationId($stn_id);
        $station->setStationName($header->getSite());
        #------------------------------------------------------------------------------

	$station->setNetworkName(getNetworkName());
	$station->setNetworkIdNumber(15);
	$station->setPlatformIdNumber(53);
	$station->setReportingFrequency("12 hourly");

	$STATIONS->addStation($station);
    }
    $station->insertDate($header->getActualDate(),"YYYY, MM, DD");

    my @records = ();
    open(my $TXT,sprintf("%s/%s",getRawDirectory(),$file)) or die("Cannot open txt file $file\n");
    my $start_press;
    foreach my $line (<$TXT>) {
	my @data = split(' ',$line);
	if ($data[1] != 0 || $data[2] != 0 || $data[3] != 0) {
	    if (!defined($start_press) || ($start_press - $data[1] > 2 && $data[1] != -999.99)) {
		$start_press = $data[1] unless(defined($start_press));
		my $txtRec = ClassRecord->new($WARN_LOG,$filename);
		$txtRec->setTime($data[0]);
		$txtRec->setPressure($data[1],"mbar") unless ($data[1] == -999.99);
		$txtRec->setTemperature($data[2],"C");
		$txtRec->setRelativeHumidity($data[3]);
		push(@records,$txtRec);
	    }
	}
    }
    close($TXT);

    my @allRecords = ();
    my @noPressRecords = ();
    my $has_T_file = 1;
    open(my $TFILE,sprintf("%s/T%d",getRawDirectory(),$asc_num)) or $has_T_file = 0;


    if (!$has_T_file) {
      printf("Cannot open TFILE for $asc_num\n");
      push(@allRecords, @records);
    } else {
      foreach my $line (<$TFILE>) {
	
	my $tRec = ClassRecord->new($WARN_LOG,$filename);

	#$tRec->{"time"} = undef();    - THIS IS COMMENTED OUT! Verify why.

	$tRec->setPressure(substr($line,9,6)/100,"mbar") unless (substr($line,9,6) =~ /^9+$/);
	$tRec->setAltitude(substr($line,15,5),"m") unless (substr($line,15,5) =~ /^9+$/);
	$tRec->setTemperature(substr($line,20,4)/10,"C") unless (substr($line,20,4) =~ /^9+$/);
	$tRec->setRelativeHumidity(substr($line,24,4)/10) unless (substr($line,24,4) =~ /^9+$/);
	$tRec->setWindDirection(substr($line,31,3)) unless (substr($line,31,3) =~ /^9+$/);
	$tRec->setWindSpeed(substr($line,34,4)/10,"m/s") unless (substr($line,34,4) =~ /^9+$/);
	
	my $txtRec = shift(@records);
	while (defined($txtRec)) {
	  if ($tRec->getPressure() == 9999) {
	    push(@noPressRecords,$tRec);
	    undef($txtRec);
	  } elsif ($txtRec->getPressure() == 9999) {
	    push(@allRecords,$txtRec);
	    $txtRec = shift(@records);
	  } elsif (sprintf("%.1f",$tRec->getPressure()) == sprintf("%.1f",$txtRec->getPressure())) {
	    $tRec->setTime($txtRec->getTime());
	    if (sprintf("%.1f",$tRec->getTemperature()) != sprintf("%.1f",$txtRec->getTemperature())) {
	      printf($WARN_LOG "%s %d: Temperature mismatch (%s,%s) for pressure %s\n",$stn_id,$asc_num,
		     $tRec->getTemperature(),$txtRec->getTemperature(),$tRec->getPressure());
	    }
	    if (sprintf("%.1f",$tRec->getRelativeHumidity()) != sprintf("%.1f",$txtRec->getRelativeHumidity())) {
	      printf($WARN_LOG "%s %d: RH mismatch (%s,%s) for pressure %s\n",$stn_id,$asc_num,
		     $tRec->getRelativeHumidity(),$txtRec->getRelativeHumidity(),$tRec->getPressure());
	    }
	    push(@allRecords,$tRec);
	    undef($txtRec);
	  } elsif ($tRec->getPressure() < $txtRec->getPressure()) {
	    push(@allRecords,$txtRec);
	    $txtRec = shift(@records);
	  } else {
	    push(@allRecords,$tRec);
	    undef($txtRec);
	  }
	}
      }
    }
    close($TFILE);

    #------------------------
    # Calculate the altitudes
    #------------------------
    my $prev;
    foreach my $rec (@allRecords) {
	if (defined($prev)) {
	    $rec->setAltitude(calculateAltitude($prev->getPressure(),$prev->getTemperature(),$prev->getDewPoint(),
						$prev->getAltitude(),$rec->getPressure(),$rec->getTemperature(),
						$rec->getDewPoint(),1,$WARN_LOG),"m");
	} else {
	    my $lat_fmt = $header->getLatitude() < 0 ? "-" : "";
	    while (length($lat_fmt) < length($header->getLatitude())) { $lat_fmt .= "D"; }
	    my $lon_fmt = $header->getLongitude() < 0 ? "-" : "";
	    while (length($lon_fmt) < length($header->getLongitude())) { $lon_fmt .= "D"; }

	    $rec->setAltitude($header->getAltitude(),"m");
	    $rec->setLatitude($header->getLatitude(),$lat_fmt);
	    $rec->setLongitude($header->getLongitude(),$lon_fmt);
	}
	$prev = $rec if ($rec->getAltitude() != 99999);
    }


    open(my $OUT,sprintf(">%s/%s",getOutputDirectory(),$filename)) or die("Can't create file $filename\n");
    print($OUT $header->toString());

    my $record = shift(@allRecords);
    $prev = $record;
    foreach my $rec (@noPressRecords) {
	while (defined($record) && $record->getAltitude() < $rec->getAltitude()) {
	    if ($record->getTime() != 9999 && $record->getTime() != $prev->getTime()) {
		$record->setAscensionRate(($record->getAltitude() - $prev->getAltitude()) / 
					  ($record->getTime() - $prev->getTime()),"m/s");
	    }

	    print($OUT $record->toString());
	    $prev = $record if ($record->getTime() != 9999 && $record->getAltitude() != 99999);
	    $record = shift(@allRecords);
	}

	if (defined($record) && $record->getAltitude() == $rec->getAltitude()) {
	    printf("Need to handle the case where altitudes are equal!!\n");
	    die();
	} else {
	    if ($rec->getTime() != 9999 && $rec->getTime() != $prev->getTime()) {
		$rec->setAscensionRate(($rec->getAltitude() - $prev->getAltitude()) / 
				       ($rec->getTime() - $prev->getTime()),"m/s");
	    }
	    printf($OUT $rec->toString());
	    $prev = $rec if ($rec->getTime() != 9999 && $rec->getAltitude() != 99999);
	}
    }

    while (defined($record)) {
	if ($record->getTime() != 9999 && $record->getTime() != $prev->getTime()) {
	    $record->setAscensionRate(($record->getAltitude() - $prev->getAltitude()) / 
				      ($record->getTime() - $prev->getTime()),"m/s");
	}
	
	print($OUT $record->toString());
	$prev = $record if ($record->getTime() != 9999 && $record->getAltitude() != 99999);
	$record = shift(@allRecords);	
    }

if ($debug) {print "Exit processTxtFile::\n"; }

    close($OUT);
}

##-----------------------------------------------------------------------
# @signature Object[] spline(* array_ref, * param_ref)
# <p>This is a function that Darren created to help with the correction
# of the wind parameters.  I have no idea what it actually does besides
# taking in two array references and returning a different array. - Joel</p>
##-----------------------------------------------------------------------
sub spline{
    my($array_ref,$parem_ref) = @_;
    my(@x,@y,$n,$yp1,$ypn,@y2,@input);@y2 = ();@x = ();@y = ();
    my($p,$qn,$sig,$un,@u);

    if ($debug) {print "Enter spline\n"; }

    # creating arrays x containing times for non-missing paremeters contained in y
    foreach my $line(@{$array_ref}){
	@input = split(' ',$line);
	if($input[$field{$$parem_ref}] != $MISS{$$parem_ref} && ($input[$field{Qu}] != 2.0 || $input[$field{Qu}] != 3.0 || $input[$field{Qu}] != 4.0)){
	    if($input[$field{time}]%$SMOOTH == 0){ 
		push(@x,$input[$field{time}]);
		push(@y,$input[$field{$$parem_ref}]);
	        #print "$input[$field{time}] $input[$field{$$parem_ref}]\n";
            }
	}
    }

    $n = scalar(@x)-1;

    # setting derivatives for points 1 and n
    if($n > 0){
	if(abs($x[$n]-$x[$n-1]) > 0){
	    $ypn = ($y[$n]-$y[$n-1])/($x[$n]-$x[$n-1]);
	}else{
	    $ypn = 1.00e30;;
	}

	if(abs($x[1]-$x[0]) > 0){
	    $yp1 = ($y[1]-$y[0])/($x[1]-$x[0]);
	}else{
	    $yp1 = 1.00e30;
	}

	if($yp1 > 0.99e30){
	    $y2[0]=$u[0]=0.0;
	}else{
	    $y2[0] = -0.5;
	    $u[0] = (3.0/($x[1]-$x[0]))*(($y[1]-$y[0])/($x[1]-$x[0])-$yp1);
	}
	
        # tridiagnal decomposition, y2 and u are used for temporary storage
	for(my $i=1;$i<=$n-1;$i++){
	    $sig = ($x[$i]-$x[$i-1])/($x[$i+1]-$x[$i-1]);
	    $p=$sig*$y2[$i-1]+2.0;
	    $y2[$i]=($sig-1.0)/$p;
	    $u[$i]=($y[$i+1]-$y[$i])/($x[$i+1]-$x[$i])-($y[$i]-$y[$i-1]) / 
		($x[$i]-$x[$i-1]);
	    $u[$i]=(6.0*$u[$i]/($x[$i+1]-$x[$i-1])-$sig*$u[$i-1])/$p;
	}
	
	if($ypn > 0.99e30){
	    $qn = $un = 0.0;
	}else{
	    $qn = 0.5;
	    $un=(3.0/($x[$n-1]-$x[$n-2]))*($ypn-($y[$n-2])/($x[$n-1]-$x[$n-2]));
	}

	$y2[$n]=($un-$qn*$u[$n-1])/($qn*$y2[$n-1]+1.0);

        # back substitution loop of tridiagnol algorithm
	for (my $k=$n-1;$k>=0;$k--){
	    $y2[$k] = $y2[$k]*$y2[$k+1]+$u[$k];
	}
    }
    if ($debug) {print "Exit spline\n"; }
    return @y2;
}

##-----------------------------------------------------------------------
# @signature Object[] splint(* array_ref, * y2a_ref, * param_ref)
# <p>This is a function that Darren created to help with the correction
# of the wind parameters.  I have no idea what it actually does besides
# taking in three array references and returning a different array. - Joel</p>
##-----------------------------------------------------------------------
sub splint{
    my($array_ref,$y2a_ref,$parem_ref) = @_;
    my(@xa,@ya,$x,$y,$n,@input,$klo,$khi,$k,$h,$b,$a,$j,@OUTFILE,$qc_flag);
    my @y2a = @$y2a_ref;
    my $max;

    if ($debug) {print "Enter splint\n"; }

    foreach my $line(@{$array_ref}){
	@input = split(' ',$line);
        $x = $input[$field{time}];
	$y = $input[$field{$$parem_ref}];
	if($$parem_ref eq "Ucmp"){
	    $qc_flag = $input[$field{Qu}];
        }elsif($$parem_ref eq "Vcmp"){
	    $qc_flag = $input[$field{Qv}];
        }else{
	    $qc_flag = 99.0;
        }

	if($y != $MISS{$$parem_ref} && $qc_flag == 99.0){
	    if($x%$SMOOTH == 0){
		#printf("%6.1f %6.1f\n",$x,$y);
		push(@xa,$x);
		push(@ya,$y);
            }
	}
    }

    $n = scalar(@xa)-1;
    $klo = 0;$khi = $n;
    foreach my $line(@{$array_ref}){
        $klo = 0;$khi = $n;
	@input = split(' ',$line);
	$x = $input[$field{time}];
	$y = $input[$field{$$parem_ref}];
	if($x <= $LIMIT{INTERP}){
	    #if($y == $MISS{$$parem_ref}){
	    #if(($x%$SMOOTH != 0 || $y == $MISS{$$parem_ref}) && $x <= 360){
	    #if(($x%$SMOOTH != 0 || $y == $MISS{$$parem_ref})){ 

	    while($khi-$klo>1){
		#printf("%6.1f %6.1f %6.1f %6.1f\n",$y,$x,$xa[$klo],$xa[$khi]);
		$k = ($khi+$klo) >> 1;
		if($xa[$k] > $x){
		    $khi=$k;
		}else{
		    $klo=$k;
		}
	    }
	    
	    if($x < $xa[$khi] && $x > $xa[$klo]){
		#printf("%6.1f %6.1f %6.1f %6.1f\n",$y,$x,$xa[$klo],$xa[$khi]);   
		$h=$xa[$khi]-$xa[$klo];
		#printf("%6.1f %6.1f %6.1f\n",$h,$y2a[$klo],$y2a[$khi]);
		unless($h == 0 || $h > $TIME_LIMIT){
		    $a=($xa[$khi]-$x)/$h;
		    $b=($x-$xa[$klo])/$h;
		    $y=$a*$ya[$klo]+$b*$ya[$khi]+(($a*$a*$a-$a)*$y2a[$klo])+
			(($b*$b*$b-$b)*$y2a[$khi])*($h*$h)/6.0;
		    
		    $max = $ya[$klo];
                    if($ya[$khi] > $max){$max = $ya[$khi];}
		    unless($y > $max){
			$input[$field{$$parem_ref}] = $y;
			if($$parem_ref eq "Vcmp"){
			    $input[$field{Qv}] = 4.0;# estimated Qv
			}elsif($$parem_ref eq "Ucmp"){
			    $input[$field{Qu}] = 4.0;# estimated Qv
			}
			#printf("%6.1f %6.1f %6.1f\n",$y,$ya[$klo],$ya[$khi]);
		    }else{
			$input[$field{$$parem_ref}] = $MISS{$$parem_ref};
			if($$parem_ref eq "Vcmp"){
			    $input[$field{Qv}] = 9.0;# missing Qv
			}elsif($$parem_ref eq "Ucmp"){
			    $input[$field{Qu}] = 9.0;# missing Qv
			}
		    }
		}else{
		    $input[$field{$$parem_ref}] = $MISS{$$parem_ref};
		    if($$parem_ref eq "Vcmp"){
			$input[$field{Qv}] = 9.0;# missing Qv
		    }elsif($$parem_ref eq "Ucmp"){
			$input[$field{Qu}] = 9.0;# missing Qv
		    }
		}
		
	    }
	}

	push(@OUTFILE,&line_printer(@input));
    }
    if ($debug) {print "Exit splint\n"; }
    return @OUTFILE;
}

##-----------------------------------------------------------------------
# @signature void timestamp(FILE* CHK)
# <p>Print out a time stamp to the specified file handle.</p>
# 
# @input $CHK The file handle to print the time stamp to.
##-----------------------------------------------------------------------
sub timestamp{
    my $CHK = shift;
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$julian,$isdst) = gmtime(time);
    $mon+=1;$year+=1900;
    my $TIME = sprintf("%02d%s%02d%s%02d",$hour,":",$min,":",$sec);
    my $DATE = sprintf("%02d%s%02d%s%4d",$mon,"/",$mday,"/",$year);
    print $CHK "GMT time and day $TIME $DATE\n";  

} # timestamp()

##-----------------------------------------------------------------------
# @signature void writefile(String[]* array_ref, FILE* fh_ref)
# <p>Write the data that is pointed to by the array_ref into the file handle.</p>
#
# @input $array_ref The reference to the data.
# @input $fh_ref The file handle to be written to.
##-----------------------------------------------------------------------
sub writefile{
    my($array_ref,$fh_ref) = @_;
    my @array = @{$array_ref};
    
    if ($debug) {print "Enter writefile\n"; }
    foreach my $line(@array) {
	my @input;
        if($INTERP){
	    @input = &calc_WND(split(' ',$line));
        }else{
	    @input = split(' ',$line);
        }
	
	if($UNCHECK){
	    unless(($input[$field{Qv}]==4.0 && $input[$field{Qu}]==4.0) || 
		   ($input[$field{Qv}]==9.0 && $input[$field{Qu}]==9.0)) {
	        $input[$field{Qu}] = 99.0;
		$input[$field{Qv}] = 99.0;
	    }
	    
	    if($input[$field{Spd}]==$MISS{Spd} || $input[$field{Ucmp}]==$MISS{Ucmp} ||
	       $input[$field{Vcmp}]==$MISS{Vcmp}) {
		
		foreach my $elem("Spd","Dir","Ucmp","Vcmp"){
		    $input[$field{$elem}] = $MISS{$elem};
                }
		$input[$field{Qu}] = 9.0;
		$input[$field{Qv}] = 9.0;
	    }
        }
	
        &printer(\@input,$fh_ref);
    }
if ($debug) {print "Exit writefile\n"; }
} # writefile()
