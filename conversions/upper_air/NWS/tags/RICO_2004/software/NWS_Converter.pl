#! /bin/perl -w


use strict;
use lib "../lib";
use lib "/work/software/NAME/library/conversion_modules/Version3";
use Formats::Class qw(:DEFAULT &windqc %WINDQC &calc_WND);
use Station::Station;
use Station::ElevatedStationMap;
use Time::Local; 
$! = 1;

# Define the programs that convert the data to CLASS format
my $VAISALA = "/usr/bin/nice -19 ./nwsVaisala";
my $VIZ = "/usr/bin/nice -19 ./nwsVIZ";

# Define the abbreviations for the file name for the known WMO Numbers
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
"27502"=>"brw","11641"=>"sju"
);

# Define Global variables.
my ($TRUE,$FALSE) = (1,0);
my ($INTERP,$REMOVE,$UNCHECK,$AVG,$TIME_LIMIT,$SMOOTH);
my %LIMIT;
my $STATIONS = Station::ElevatedStationMap->new();

# Removes Bad thu entire sounding, Quest in first 360 seconds and interpolates thur 
my $WIND_FLAG = $TRUE;

# Define directories and files to be used.
sub getFinalDirectory() { return "../final"; }
sub getLogDirectory() { return "../logs"; }
sub getNetworkName() { return "NWS"; }
sub getOutputDirectory() { return "../output"; }
sub getProjectName() { return "RICO"; }
sub getRawDirectory() { return "../raw_data"; }
sub getStationFile() { return sprintf("%s/%s_%s_stationCD.out",getFinalDirectory(),
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

    # Create output directories as necessary.
    mkdir(getLogDirectory()) unless (-e getLogDirectory());
    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());
    mkdir(getFinalDirectory()) unless (-e getFinalDirectory());

    # Setup variables to be used for the wind calculation
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
 

    open($WARN_LOG,sprintf(">%s/%s",getLogDirectory(),getWarningFile())) or
	die("Cannot open warning file.\n");

    
    opendir(my $RAW,getRawDirectory()) or die("Cannot open raw directory.\n");
    my @files = readdir($RAW);
    closedir($RAW);

    # Process the files in the raw directory.
    foreach my $file (sort(@files)) {
		processFile($file) if ($file !~ /^\.+$/);
#		processFile($file) if ($file =~ /^2316004\.10\.gz$/);
    }

    close($WARN_LOG);

    system(sprintf("cat *.asc.log > %s/nws.log",getLogDirectory()));
    system("rm *.asc.log");

    open(my $STN, ">".getStationFile()) || die("Cannot create the ".getStationFile()." file\n");
    foreach my $station ($STATIONS->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open(my $SUMMARY, ">".getSummaryFile()) || die("Cannot create the ".getSummaryFile()." file.\n");
    print($SUMMARY $STATIONS->getStationSummary());
    close($SUMMARY);
}

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

    # Only compare if the old file actually exists.
    if (-e sprintf("%s/%s",getOutputDirectory(),$outfile)) {

	# Pull the ascension numbers from the files.
	my $new_ascension = getAscensionNumber($infile);
	my $old_ascension = getAscensionNumber($outfile);

	# Want to return true if the old number is smaller or the
	# same as the new one.  We want to keep the largest ascension
	# number.
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

    if(grep(/Incorrect record length for DR_(VZPR|VZCAL|MET6)/i,$data)){ 
	print $WARN_LOG "$outfile contains errors - Can't process\n";
	print "$outfile contains errors - Can't process\n";
    }elsif(!grep(/6 Second Met Data/,$data)){
	print $WARN_LOG "$outfile does not contain data - Can't process\n";
	print "$outfile does not contain data - Can't process\n";
    }else{

	# Need to save the data to a file so it can be read by the conversion
	# programs.
	open(my $FILE,">$outfile") or die("Cannot write: $outfile\n");
	print($FILE $data);
	close($FILE);
	
	# Convert the data to the CLASS format.
	system(sprintf("%s %s",grep(/UNKNOWN/i,$data) ? $VAISALA : $VIZ, $outfile));

	# Remove the original ASCII file.  It is no longer needed.
	unlink($outfile);

	# Correct the wind parameters in the CLASS file.
	correct_winds();
    }
}

##-----------------------------------------------------------------------
# @signature void correct_winds()
# <p>Run the wind correction algorithm on the CLASS files.  This should only
# pick up the most recent file, since all of the files are removed once
# they are used.  But, it will pick up any CLASS files in the current 
# directory, so if the converter would crash and the previous file was not
# removed, it will convert that as well.</p>
##-----------------------------------------------------------------------
sub correct_winds {

    # Get the list of files in the current directory.
    opendir(my $CURR,".") or die("Cannot read current directory.\n");
    my @files = grep(/\.cls$/,readdir($CURR));
    closedir($CURR);    

    # Loop through all of the CLASS files
    foreach my $file (@files) {

	# Create the final file name from the CLASS file name.
	my $outfile = sprintf("%s/%s",getOutputDirectory(),$file);

	# Only correct winds if this file has a smaller ascension number than
	# a previous run.
	if (!check_ascension_numbers($file,$outfile)) {
	    unlink($file);
	    return;
	}

	# Create the log files for the wind correction.
	my $wndfile = sprintf("%s/%s.wnd",getLogDirectory(),$file);
	my $statfile= sprintf("%s/%s.stat",getLogDirectory(),$file);
	
	# Open the files needed
	open(my $FILE,$file) || die "Can't open $file\n";
        open(my $OUT,">$outfile") || die "Can't open $outfile\n";
        open(my $WND,">$wndfile") || die "Can't open $wndfile\n";
	open(my $STAT,">$statfile") || die "Can't open $statfile\n";

	my @OUTFILE;

	# Get a list of the data from the file.
	my $station = Station::Station->new();
	my $date = "00000000";
	for my $line (<$FILE>) {
	    if($line =~ /(\-|)\d+\.\d+/ && $line !~ /[a-zA-Z\/]/){
		push(@OUTFILE,$line);
	    }else{
		print $OUT $line;
		
		if ($line =~ /Release Site Type/) {
		    my @data = split(' ',$line);
		    $station->setCountry($data[-1]);
		    $station->setStateCode("XX");
		    $station->setStationId($data[4]);
		    $station->setStationName(trim((split(/:/,$line))[1]));
		    
		    $station->setNetworkName(getNetworkName());
		    $station->setNetworkIdNumber(15);
		    $station->setPlatformIdNumber(53);
		    $station->setReportingFrequency("6 second");
		} elsif ($line =~ /Release Location/) {
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

	# Correct the wind parameters
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

	# Write the data to the file.
        &writefile(\@OUTFILE,\$OUT);

	close($WND);
	close($STAT);
	close($OUT);

	# Remove the old CLASS file to prevent confusion with the corrected wind
	# file and because it is no longer needed.
	unlink($file);
    }
}

##-----------------------------------------------------------------------
# @signature int getAscensionNumber(String file)
# <p>Get the ascension number of a sounding in CLASS file format.</p>
#
# @input $file The name of the file in CLASS format.
# @output $number The ascension number in the file.
##-----------------------------------------------------------------------
sub getAscensionNumber {
    my $file = shift;

    # Get the Ascension Number line from the file.
    open(my $FILE,$file);
    my @line = grep(/Ascension No/,<$FILE>);
    close($FILE);
    
    # Parse out the ascension number and return it.
    $line[0] =~ /(\d+)/;
    return $1;
}

sub numerically { $a <=> $b; }

##-----------------------------------------------------------------------
# @signature void processFile(String file)
# <p>Convert a raw data file into individual sounding files in CLASS format.</p>
#
# @input $file The raw data file to be converted.
##-----------------------------------------------------------------------
sub processFile {
    my $input_file = shift;
    my $wban = substr($input_file,0,5);

    print "Processing RAW file: $input_file ...\n";

    open(my $FILE,sprintf("gzcat %s/%s|",getRawDirectory(),$input_file)) || 
	die "Can't open $input_file\n";


    my $miss_ASC = 9000;
    my $file_cnt = 0;
    
    my $out = "";
    my $outfile = "";
    my ($ASC,$beg_file) = (0,1);

    my %header;

    # Loop through all of the lines in the file.
    foreach my $line (<$FILE>) {
	# Remove the new line character at the end of the line.
	chomp($line);

	if($line =~ /WMO NUM/){
	    # Marks the beginning of a new sounding

	    # All of the data has been gathered for the previous sounding so it
	    # is now ready to be converted.
	    if($file_cnt){ convert($out,$outfile); }

	    $line =~ tr/A-Za-z#:\///d;	
	    $ASC = (split(' ',$line))[4];

	    # Make sure there is an ascension number for the sounding
	    if($ASC !~ /\d{4}/){
		$ASC = $miss_ASC;
		$miss_ASC++;
		print $WARN_LOG "Bad/missing ascension number\n";
		print $WARN_LOG "Using $ASC\n";
	    }

	    # Reset the variables for the new file.
	    $outfile = sprintf("%s%s.asc",$station_id{$wban},$ASC);
	    $out = "";
	    %header = ();
	    $beg_file = 0;

	    $file_cnt++;

	    $header{$ASC}{"beg_line"} = $line;
	} elsif ($line =~ /File:/) {
	    # Maks the beginning of the data for the current file.
	    $out .= "\n$line\n";
	    $beg_file = 1;
	} elsif($beg_file) {
	    # Save the data
	    $out .= $line."\n";
	}

	# Used for the output to the warning file.
	# Not sure if it used anymore.
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
	convert($out,$outfile); 
	
    }


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
}

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
}
