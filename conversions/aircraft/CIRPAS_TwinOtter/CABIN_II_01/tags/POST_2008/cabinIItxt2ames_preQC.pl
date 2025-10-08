#!/usr/bin/perl
#
# Convert the POST cabin data, which is columnar ascii with variable names
# and units on a single header line, and records starting with time since
# start of the flight in seconds and time since midnight in hours.hundredths 
# to NASA Ames format ala 
# http://badc.nerc.ac.uk/help/formats/NASA-Ames/na-for-dummies-1D.html
# so that asc2cdf can convert it to netCDF and the metadata will be preserved.
#
# Created JAG 2009/01/28
#
# Updated JAG 2009/04/30
#	Misunderstood times. Hours.hundredths is since midnight of flight day, 
#	seconds is since start of flight (and we don't know when flight started).
#	Correct calc to work from Hours.hundredths column rather than seconds column.

use POSIX;

# Define some metadata for these files that is not in the input files but
# will be needed to create the Ames-formatted files.
$PI = "Jonsson, Haf";
$Institute = "CIRPAS, Navel Postgraduate School, Monterey, CA";
$project = "Physics Of Stratocumulus Tops (POST)";
$timestep = 1;
$missing = -32767;
%varlist = (
    'Lat' => ['GLAT','Lat (deg)','GPS latitude (CIRPAS)'],
    'Long' => ['GLON','Long (deg)','GPS longitude (CIRPAS)'],
    'NovAtel Alt (m)' => ['GALT','Nov Atel (m)','GPS altitude (CIRPAS)'],
    'East Vel (m/s)' => ['GWIE','East Vel (m/s)','GPS East aircraft velocity (CIRPAS)'],
    'North Vel (m/s)' => ['GWIN','North Vel (m/s)','GPS North aircraft velocity (CIRPAS)'],
    'Up Vel (m/s)' => ['GWIU','Up Vel (m/s)','GPS up aircraft velicity (CIRPAS)'],
    'Roll (deg)' => ['ROLL','Roll (deg)','roll of aircraft'],
    'Pitch (deg)' => ['PITCH','Pitch (deg)','pitch of aircraft'],
    'Heading (deg)' => ['THDG','Heading (deg)','aircraft heading'],
    'Tamb (C)' => ['AT','T amb (C)','static ambient temperature'],
    'Tdamb (C)' => ['DT','Td amb (C)','ambient dew-point temeprature'],
    'RHamb (%)' => ['RHUM','RH amb (%)','ambient relative humidity'],
    'Ps (mb)' => ['PS','Ps (mb)','static atmospheric pressure'],
    'Wind Speed (m/s)' => ['WSC','Wind Speed (m/s)','horizontal wind speed'],
    'Wind dir(Deg)' => ['WDC','Wind Dir (deg)','wind direction'],
    'Vert. Wind (m/s)' => ['WVC','Vert. Wind (m/s)','vertical wind velocity'],
    'SST (C)' => ['SST','SST (C)','sea-surface temperature'],
    'Palt (m)' => ['PALT','P Alt (m)','pressure altitude'],
    'Rad Alt.(m)' => ['RADALT','Rad Alt (m)','radar altitude'],
    'TAS (m/s)' => ['TAS','TAS (m/s)','true air speed'],
    'Theta (K)' => ['THETA','Theta (K)','potential temperature'],
    'Thetae' => ['THETAE','Thetae (K)','equivalent potential temperature'],
    'MR-h2o (g/Kg)' => ['MRLA1','MR-H2O (g/Kg)','mixing ratio (from dew point, CIRPAS)'],
    'SPHUM (g/Kg)' => ['SPHUM','SP Hum (g/Kg)','specific humidity (from dew point)'],
    'Rho-dry (Kg/M^3)' => ['RHO','Rho-dry (Kg/m^3)','ambient density of dry air'],
    'LWC-Wire (g/m^3)' => ['LWC1','LWC-wire (g/m^3)','liquid water content (CIRPAS)'],
    'PCASP (#/CC)' => ['CONC_PCASP','PCASP (#/cc)','PCASP concentration'],
    'PCASP (Vol/CC)' => ['VOL_PCASP','PCASP (Vol/cc)','PCASP volume'],
    'CASFWD (#/CC)' => ['CONC_CAS','CASFWD (#/cc)','CAS concentration, 1 - 50 um diameter'],
    'CASFWD (Vol/CC)' => ['VOL_CAS','CASFWD (Vol/cc)','CAS volume'],
    'FSSP (#/CC)' => ['CONC_FSSP','FSSP (#/cc)','FSSP concentration'],
    'FSSP (Vol/CC)' => ['VOL_FSSP','FSSP (Vol/cc)','FSSP volume'],
    'CPC1 (#/CC)' => ['CONC_CPCI','CPCI (#/cc)','CN concentration > 10 nm'],
    'UFCPC (#/CC)' => ['CONC_UFCPC','UFCPC (#/cc)','CN concentration > 3 nm'],
    'CIP (#/CC)' => ['CONC_CIP','CIP (#/cc)','CIP concentration, 50 - 1550 um diameter'],
    'CIP (Vol/CC)' => ['VOL_CIP','CIP (Vol/cc)','CIP volume'],
);

# List all the CABIN files in the current dir into an array.
@files = <CABIN_II_RF*_*_*[0-9]>;
if (@files == ()) {print "There are no CABIN files in the current dir. Exiting\n"; exit(1);}

#Loop through the files and process each individually.
foreach $file (@files) {
    print "Processing $file\n";

    # Parse the date from the filename
    if ($file !~ /^CABIN_II_RF[0-9][0-9]_[0-9]{8}_[0-9]{6}/) {
	print "Filename $file does not match naming convention ";
	print "CABIN_II_RF##_YYYYMMDD_HHMMSS. Please rename input ";
	print "files before running this code.\n";
	exit(1);
    } else {
	($inst1,$inst2,$flight,$ymd,$hms) = split(/_/,$file);
	#rejoin the first two fields to be the instrument name.
	$instrument = $inst1."_".$inst2;
	$year = substr($ymd,0,4);
	$month = substr($ymd,4,2);
	$day = substr($ymd,6,2);
	$hour = substr($hms,0,2);
	$min = substr($hms,2,2);
	$sec = substr($hms,4,2);
	$sec_since_midnight = $sec + $min*60 + $hour*3600;
    }

    $now = strftime "%Y %m %d",localtime();

    # Start an output file to contain the AMES formatted data.
    open (OUTFILE, ">$file.ames") or die "Can't open $file.ames:$!\n";
    print OUTFILE "51 1001\n";
    print OUTFILE "$PI\n$Institute\n$instrument\n$project\n1 1\n";
    print OUTFILE "$year $month $day $now\n$timestep\n";
    print OUTFILE "Time (seconds since ".$year."-".$month."-".$day." ";
    # Since adding hour:min:sec to time of each rec, recs are now secs
    # since 00:00:00
    #print OUTFILE $hour.":".$min.":".$sec." +00:00)\n";
    print OUTFILE "00:00:00 +00:00)\n";
    

    # Open the input file we are working on.
    open (FILE, $file);

    # Parse the date from the input file

    # Read the single header line from the input file. This line contains
    # variable names and units.
    $header=<FILE>;

    # Remove the carriage-return line-feed from the end of the header line.
    chop $header;
    chop $header;

    #######################################################################
    # The header line in these files looks like a tab-separated list of
    # groups of space-delimited variable names and optional parenthesized
    # units, e.g.:
    # "Mission Time (s)	HH.hhhhh (Hours)	Lat	Long ..."
    # Split the variable names and units into two arrays. Missing units
    # are stored as an empty string "". Duplicate variable names
    # differentiated only by thier units do occur. Check for and deal 
    # with duplicate variable names as these will cause asc2cdf to finish 
    # without generating a netCDF file and without any errors.
    #######################################################################

    # Split the header into variable/unit groups, e.g. $header[0] would
    # contain "Mission Time (s)" in our example above.
    @header = split("	",$header);

    # Now loop through the groups and parse out the variable and units.
    @vars = (); $numvars = 0;
    @units = (); 
    @longnames = ();
    # Remove the times from the start of the line
    shift @header;
    shift @header;
    foreach $group (@header) {
	#print "$group\n";

	# Some of the variable names have spaces in them, so we can't reliably
	# split on space. So split on open parenthesis instead.
	($var,$unit) = split(/\(/,$group);

	# Remove spaces from variable name.
	#$var =~ s/ //g;

	# Check for duplicates
	#for ($v=0;$v<$numvars;$v++) {
	#    if ($vars[$v] eq $var) {$var .= "_2";}
	#}

	# If there are no units in this group, then we won't find a close
	# paren.
	if ($unit !~ /\)/) {$unit = "";} 

	# Remove close paren from units.
	else {$unit =~ s/(.*?)\)/$1/g;}

	# Change the var name to the RAF abbreviation as given in the "FINAL POST Twin Otter
	# Output Parameter List (10/25/2008)"
	$var = $varlist{$group}[0];
	$longname = $varlist{$group}[2];

    	# Now push the var info into the arrays
	$numvars = push(@vars,$var);
	$numunits = push(@units,$unit);
	$numlongnames = push(@longnames,$longname);

	# Debug
	#print "*** $vars[$numvars-1] $units[$numunits-1]\n";
	#exit(1);
    }
    print OUTFILE "$numvars\n";
    foreach $var (@vars) {print OUTFILE "1.0 ";} print OUTFILE "\n";
    foreach $var (@vars) {print OUTFILE "$missing ";} print OUTFILE "\n";
    for ($i=0; $i < $numvars; $i++) {
	print OUTFILE "$longnames[$i] ($units[$i])\n";
    }
    print OUTFILE "0\n0\n";
    print OUTFILE "Time";
    foreach $var (@vars) {print OUTFILE " $var";} print OUTFILE "\n";

    while (<FILE>) {
	    (@params) = split(/\s/, $_);
	    # remove the MissionTime
	    shift @params; 

	    # Grab the Hours.hundredths
	    $date = $params[0]; 
	    shift @params; # remove the Hours.hundredths from the remaining list

	    # Multiply the Hours.hundredths by 3600 to get the seconds
	    $sec = int($date*3600 + .5);

	    # replace missing values in the remaining params
	    foreach $param (@params) { 
		if ($param =~ /^$/) {$param = $missing;}
	    }
	    # add the MissionTime back in
	    print OUTFILE $sec." ".join(' ',@params)."\n"; 
    }
    close(OUTFILE);
    close(FILE);

    # Convert the ames file to netCDF
    system("asc2cdf -a $file.ames $file.nc");
}


