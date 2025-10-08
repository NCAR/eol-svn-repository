#!/bin/perl -w
# program TAOTenderCruise.pl May 4th,2003
# Darren R. Gallant JOSS
use POSIX;
$TRUE = 1;$FALSE = 0;
$stn_name = "NOAA Research Ship";$stn_id = "TAO";
$data_type = "High Resolution Sounding";
$project_id = "PACS";
$site = "$stn_name $stn_id";
# constants
$wind_tol = 0.0001;$pi = 3.14159265;$radian = $pi/180.;$degree = 1/$radian;
$ship_elev = 9.0;
$K = 273.15;# Kelvin
$header[0] = "Data Type:                         ";
$header[1] = "Project ID:                        ";
$header[2] = "Release Site Type/Site ID:         ";
$header[3] = "Release Location (lon,lat,alt):    ";
$header[4] = "UTC Release Time (y,m,d,h,m,s):    ";
$header[5] = "Ascension No:                      ";
$header[6] = "Nominal Release Time (y,m,d,h,m,s):";
$header[7] = "System Operator/Comments:          ";
$header[8] = "Additional comments:               ";
#%field = (0,"time",1,"pressure",2,"temperature",3,"dewpoint",4,"RH",
#5,"Ucmp",6,"Vcmp",7,"Spd",8,"Dir",9,"Wcmp",10,"Lon",11,"Lat",12,"Rng",
#13,"Azi",14,"Alt",15,"Qp",16,"Qt",17,"Qrh",18,"Qu",19,"Qv",20,"Qdz"); 
$line[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azim   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$line[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code";
$line[2] ="------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----";
%MONTHS = (
	   january => {
	       FILE => "1",
	       MM   => "01",
	       DAYS => "31",
	       SPANISH => "encro",
	       ABREV   => "jan",
               NEXT    => "february",
	   },
	   february => {
	       FILE => "2",
	       MM   => "02",
	       DAYS => "28",
	       SPANISH => "feb",
	       ABREV   => "feb",
	       NEXT    => "march",
	   },
	   march => {
	       FILE => "3",
	       MM   => "03",
	       DAYS => "31",
	       SPANISH => "marzo",
	       ABREV   => "mar",
	       NEXT    => "april",
	   },
	   april => {
	       FILE => "4",
	       MM   => "04",
	       DAYS => "30",
	       SPANISH => "abril",
	       ABREV   => "apr",
	       NEXT    => "may",
	   },
	   may => {
	       FILE => "5",
	       MM   => "05",
	       DAYS => "31",
	       SPANISH => "mayo",
	       ABREV   => "may",
	       NEXT    => "june",
	   },
	   june => {
	       FILE => "6",
	       MM   => "06",
	       DAYS => "30",
	       SPANISH => "junio",
	       ABREV   => "jun",
	       NEXT    => "july",
	   },
	   july => {
	       FILE => "7",
	       MM   => "07",
	       DAYS => "31",
	       SPANISH => "julio",
	       ABREV   => "jul",
	       NEXT    => "august",
	   },
	   august => {
	       FILE => "8",
	       MM   => "08",
	       DAYS => "31",
	       SPANISH => "agosto",
	       ABREV   => "aug",
	       NEXT    => "september",
	   },
	   september => {
	       FILE => "9",
	       MM   => "09",
	       DAYS => "30",
	       SPANISH => "septiembre",
	       ABREV   => "sep",
	       NEXT    => "october",
	       MISSPELL => "setiembre",
	   },
	   october => {
	       FILE => "a",
	       MM   => "10",
	       DAYS => "31",
	       SPANISH => "octubre",
	       ABREV   => "oct",
	       NEXT    => "november",
	   },
	   november => {
	       FILE => "b",
	       MM   => "11",
	       DAYS => "30",
	       SPANISH => "noviembre",
	       ABREV   => "nov",
	       NEXT    => "december",
	   },
	   december => {
	       FILE => "c",
	       MM   => "12",
	       DAYS => "31",
	       SPANISH => "diciembre",
	       ABREV   => "dec",
	       NEXT    => "january",
	   },
	   );
if(@ARGV < 1){
  print "Usage is TAOTenderCruise.pl file(s)\n";
  exit;
}
@months = keys %MONTHS;
@files = grep(/\w+\.(dat|out)(|\.gz)$/i,@ARGV);
$TIME = $FALSE;if(grep(/nominal/i,@ARGV)){$TIME = $TRUE;}
$POSITION = $FALSE;if(grep(/position/i,@ARGV)){$POSITION = $TRUE;}
foreach $file(@files){
    $TIMESTAMP = $FALSE;$DATA_FLAG = $FALSE;
    if($file =~ /\.gz/){
	open(DATA,"gzcat $file|") || die "Can't open $file\n";
        $DATA_FLAG = $TRUE;
    }else{
	open(DATA,$file) || die "Can't open $file\n";
	$DATA_FLAG = $TRUE;
    }# end if-else
    if($DATA_FLAG){
	$l_time = 9999.0;$l_alt = 99999.0;
	while(defined($line = <DATA>)){
	    @input = split(' ',$line);
            if(@input){
	    if($input[0] =~ /\d{7,8}|\d{10}/){
		&filename($line);
		$FLAG{$TIMESTAMP}{"DATA"} = $DATA_FLAG;
		&location($line);
            }elsif(scalar(@input) == 9 && $FILE_OPEN){
		@outline = &init_line;
		($min,$sec,$alt,$press,$temp,$rh,$dewpt,$wdir,$wspd)= @input;
		$outline[0] = $min*60.0 + $sec; #time in seconds
	        $outline[1] = $press; #pressure in mb
		$outline[15] = 99; # unchecked Qp
		&calc_UV($wspd,$wdir);
		if($temp < 1000 && $temp > -99.9){
		    $outline[2] = $temp; # temperature in Celsius
		    $outline[16] = 99.0;# unchecked Qt
		}#end if
		if($rh >= 0 && $rh <= 100 && $dewpt < 1000 && $dewpt > -99.9){
		    $outline[4] = $rh; # relative humidity in percent
		    $outline[17] = 99;# unchecked Qrh           
		    $outline[3] = $dewpt; # DewPt in Celsius
                }elsif($dewpt < -99.9 && $rh >= 0 && $rh <= 100 ){
		    $outline[4] = $rh; # relative humidity in percent
		    $outline[17] = 4.0;# unchecked Qrh
		    $outline[3] = -99.9; # DewPt in Celsius
		}# end if
	        $outline[14] = $alt; # geopotential altitude in gpm
		if($outline[0] != 9990.0 && $outline[14] != 99999.0){
		    $outline[9]=&calc_w($outline[14],$l_alt,$outline[0],$l_time);
		    if($outline[9]!=999.0){$outline[20] = 99.0;} # unchecked Qdz
		    $l_time = $outline[0];$l_alt = $outline[14];
	        }# end if
		if($outline[0] == 0.0){
		    $outline[11] = $INFO{$TIMESTAMP}{"LAT"};#latitude
		    $outline[10] = $INFO{$TIMESTAMP}{"LON"};#longitude
		    $INFO{$TIMESTAMP}{ALT} = $outline[14];
		}# end if
		$metdata{$TIMESTAMP}{$outline[0]}=&line_printer(@outline);
		if(!grep(/$outline[0]/,@{$TIMES{$TIMESTAMP}})){
		    push(@{$TIMES{$TIMESTAMP}},$outline[0]);
		}# end if
	    }elsif(scalar(@input) == 8 && $FILE_OPEN){
		@outline = &init_line;
		($time,$alt,$press,$temp,$rh,$dewpt,$wdir,$wspd)= @input;
		$outline[0] = $time; #time in seconds
	        $outline[1] = $press; #pressure in mb
		$outline[15] = 99; # unchecked Qp
		&calc_UV($wspd,$wdir);
		if($temp < 1000 && $temp > -99.9){
		    $outline[2] = $temp-$K; # temperature in Celsius
		    $outline[16] = 99.0;# unchecked Qt
		}#end if
		if($rh >= 0 && $rh <= 100 && $outline[2] != 999.0){
		    $outline[4] = $rh; # relative humidity in percent
		    $outline[17] = 99;# unchecked Qrh           
		    $outline[3] = &calc_dewpt($outline[2],$outline[4]);
		}# end if
	        $outline[14] = $alt; # geopotential altitude in gpm
		if($outline[0] != 9990.0 && $outline[14] != 99999.0){
		    $outline[9]=&calc_w($outline[14],$l_alt,$outline[0],$l_time);
		    if($outline[9]!=999.0){$outline[20] = 99.0;} # unchecked Qdz
		    $l_time = $outline[0];$l_alt = $outline[14];
	        }# end if
		if($outline[0] == 0.0){
		    $outline[11] = $INFO{$TIMESTAMP}{"LAT"};#latitude
		    $outline[10] = $INFO{$TIMESTAMP}{"LON"};#longitude
		    $INFO{$TIMESTAMP}{ALT} = $outline[14];
		}# end if
		$metdata{$TIMESTAMP}{$outline[0]}=&line_printer(@outline);
		if(!grep(/$outline[0]/,@{$TIMES{$TIMESTAMP}})){
		    push(@{$TIMES{$TIMESTAMP}},$outline[0]);
		}# end if
	     }# end if
             }# end if
	}# end while loop
    }# end if $DATA_FLAG
    close(DATA);
}# end foreach file
foreach $time (keys %TIMES){
    if(scalar(@{$TIMES{$time}})){
	#print "TIMESTAMP $time\n";
	$outfile = $INFO{$time}{"FILE"};
        unless(open(OUT,">$outfile")){
        }else{
	    print "Class file:$outfile\n";
            &writeheader($time);
            &writefile($time);
        }# end if
    }# end if
}# end foreach $time
   
print "FINI\n";

sub writeheader{
   printf OUT ("%s%s\n",$header[0],$data_type);
   printf OUT ("%s%s\n",$header[1],$project_id);
   printf OUT ("%s%s\n",$header[2],$site);
   $stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %7.2f, %6.2f, %7.1f",&calc_pos($INFO{$_[0]}{"LON"},$INFO{$_[0]}{"LAT"}),$INFO{$_[0]}{"ALT"});
   printf OUT ("%s%s\n",$header[3],$stn_loc);
   printf OUT ("%s%s\n",$header[4],$INFO{$_[0]}{"GMT"});
   printf OUT ("%s%s\n",$header[5],$_[0]);
   for $i(0..4){ printf OUT ("%s\n","/");}
   printf OUT ("%s%s\n",$header[6],$INFO{$_[0]}{"NOMINAL"});
   for $i(0..2){printf OUT ("%s\n",$line[$i]);}
}# end sub write_header

sub init_line{
    my @tmp = ();
    for $i(0,1,5,6,10){$tmp[$i] = 9999.0;}
    for $i(2,3,4,7,8,9,11,12,13){$tmp[$i] = 999.0;}
    for $i(15..20){$tmp[$i] = 9.0;}
    $tmp[14] = 99999.0;
    return @tmp;
}# end sub init_line
    
sub printer{
    printf OUT ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]);
}# end sub printer

sub line_printer{
     $outline = sprintf ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]);
     return $outline;
}# end sub line_printer

sub calc_dewpt{
    my ($temp,$rh) = @_;
    my $ESO = 6.1121;
    if(($rh<0.0)||($rh>100.0)||($temp==999.0)){
      return 999.0;
    }else{
      if($rh == 0){ $rh = 0.005;}
      $emb = $ESO*($rh/100)* exp(17.67 * $temp/($temp+243.5));
      $dewpt = (243.5 * log($emb)-440.8)/ (19.48 - log($emb));
      if($dewpt < -99.9){
        $dewpt = -99.9;
      }# end if
    }# end if-else
    return($dewpt);
}# end sub calc_dewpt

sub calc_w{
    my ($alt,$l_alt,$time,$l_time) = @_;
    if($time != 9999.0 && $time - $l_time > 0 && $time >= 0 && $l_time >= 0){
      return ($alt - $l_alt)/($time - $l_time);
    }else{
      return 999.0;
    }# end if-else
}# end sub calc_w

sub calc_UV{
    my ($spd,$dir) = @_;
    if($spd < 999.0 && $dir <= 360.0 && $spd >= 0 && $dir >= 0.0){
	$outline[5] = sin(($dir+180.0)*$radian)*$spd;# U wind component
	$outline[6] = cos(($dir+180.0)*$radian)*$spd;# V wind component
	$outline[7] = $spd;$outline[8] = $dir;
        $outline[18] = 99.0;# Unchecked Qu
        $outline[19] = 99.0;# Unchecked Qv
    }else{
        $outline[5] = 9999.0; #missing U wind component
        $outline[6] = 9999.0; #missing V wind component
        $outline[8] = 999.0;
        $outline[18] = 9.0;# missing Qu
        $outline[19] = 9.0;# missing Qv
    }# end if-else
}# end sub calc_UV

sub by_number{$a<=>$b};

sub calc_pos{
    @pos_list = ();
    my ($lon,$lat) = @_;
    $lon_min = (abs($lon) - int(abs($lon)))*60;
    $lat_min = (abs($lat) - int(abs($lat)))*60;
    $lon_dir = "'W";if($lon > 0){$lon_dir = "'E";}
    $lat_dir = "'N";if($lat < 0){$lat_dir = "'S";}
    @pos_list = (abs(int($lon)),$lon_min,$lon_dir,abs(int($lat)),$lat_min,$lat_dir,$lon,$lat);
    return @pos_list;
}# end sub calc_pos

sub location{
    my @input = split(' ',$_[0]);
    $INFO{$TIMESTAMP}{"LAT"} = $input[1];
    $INFO{$TIMESTAMP}{"LON"} = $input[2];
    $INFO{$TIMESTAMP}{"ALT"} = $ship_elev;
}# end sub location

sub filename{
    my $min = "00";
    if($_[0] =~/(\d+)(\d{2})(\d{2})(\d{2})/){
	my($year,$mth,$dd,$hh) = ($1,$2,$3,$4);
	my $month = &findmth($mth);
        unless($year > 100){
	    if($year > 94){
		$year = "19$year";
	    }else{
		$year = sprintf("20%02d",$year);
	    }
        }# end unless
	if($dd > 0 && $dd <= $MONTHS{$month}{"DAYS"}){
	    $TIMESTAMP = substr($year,2,2).$mth.$dd.$hh;
	    #print "timestamp $TIMESTAMP\n"; 
	    $INFO{$TIMESTAMP}{"GMT"} = "$year, $mth, $dd, $hh:$min:00";
	    $outfile = $stn_id.$MONTHS{$month}{"FILE"}.$dd;
	    $INFO{$TIMESTAMP}{"FILE"} = $outfile.$hh.$min.".cls";
	    if($hh == 0){
		$nominal = "00:00:00";
	    }elsif($hh > 0 && $hh <= 3){
		$nominal = "03:00:00";
	    }elsif($hh > 3 && $hh <= 6){
		$nominal = "06:00:00";
	    }elsif($hh > 6 && $hh <= 9){
		$nominal = "09:00:00";
	    }elsif($hh > 9 && $hh <= 12){
		$nominal = "12:00:00";
	    }elsif($hh > 12 && $hh <= 15){
		$nominal = "15:00:00";
	    }elsif($hh > 15 && $hh <= 18){
		$nominal = "18:00:00";
	    }elsif($hh > 18 && $hh <= 21){
		$nominal = "21:00:00";
	    }else{
		$nominal = "00:00:00";$dd++;
		if($dd > $MONTHS{$month}{"DAYS"}){
		    $dd = 1;$month = $MONTHS{$month}{"NEXT"};
		    if($MONTHS{$month}{"MM"} eq "01"){$year++;}#end if
		}#end if
	    }#end if-elsif(6)-else
	
            my $mm = $MONTHS{$month}{"FILE"};
            $INFO{$TIMESTAMP}{"NOMINAL"} = "$year, $mth, $dd, $nominal";
	    if($TIME){
		$outfile = $stn_id.$mm.$dd;
		$INFO{$TIMESTAMP}{"FILE"} = $outfile.(substr($nominal,0,2)).".cls";
	    }# end if
            $FILE_OPEN = $TRUE;
        }# end if
    }# end if
}# end sub filename

sub writefile{
    foreach $time (sort by_number @{$TIMES{$_[0]}}){
	@outline = ();@outline = &init_line(@outline);
	$outline[0] = $time;
	if($FLAG{$_[0]}{"DATA"}){
	    if(exists($metdata{$_[0]}{$time})){
		@input = split(' ',$metdata{$_[0]}{$time});
		for $i(1,2,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20){
		    $outline[$i] = $input[$i];
		}# end for loop
	    }# end if
	}# end if
	if($POSITION){
	    push(@OUTFILE,&line_printer(@outline));
	}else{
	    &printer(@outline);   
        }# end if
    }# end foreach
    if($POSITION){&stereographic(@OUTFILE);}close(OUT);
}# end sub writefile

sub stereographic{
    my $wind_tol = 0.0001;my $pi = 3.14159265; my $radian = $pi/180.;
    my $degree = 1/$radian;my $radius = 6371220.0;my $time;
    $first = 0;
    foreach $line(@_){
	@input = split(' ',$line);
	if($first == 0){
	    print "Calculating longitude and latitude\n";
	    $l_time = $input[0];$l_uwind = $input[5];$l_vwind = $input[6];
	    $l_lat = $input[11];$l_lon = $input[10];
	    $radius = $radius + $input[14];
	    $l_alt = $input[14];
	    $first++;
	}else{
	    $time = $input[0];$uwind = $input[5];$vwind = $input[6];
	    if(($l_lon == 9999.000)||($uwind == 9999.0)||($l_uwind == 9999.0)){
		$input[10] = 9999.000;
		$input[11] = 999.000;
	    }else{
		if(abs($uwind) <= $wind_tol && abs($vwind) <= $wind_tol){
		    $input[11] = $l_lat;
		    $input[10] = $l_lon;
		}else{
		    $difft=abs($l_time-$time);$diffu=($uwind+$l_uwind)/2.0;
		    $diffv = ($vwind + $l_vwind)/2.0;
		    $x = $difft*$diffu;
		    $y = $difft*$diffv;
		    $rho = sqrt($x*$x + $y*$y);
		    $c = 2.0*POSIX::atan($rho/(2.0*$radius*1.0));
		    $input[11] = POSIX::asin(cos($c)*sin($l_lat*$radian)+$y*sin($c)*cos($l_lat*$radian)/$rho);$input[11] = $input[11]*$degree;# latitude
		    $input[10] = $l_lon + POSIX::atan($x*sin($c)/($rho*cos($l_lat*$radian)*cos($c)-($y*sin($l_lat*$radian)*sin($c))))*$degree;# longitude
		}# end if-else
		$l_time = $time;
		$l_uwind = $uwind;
		$l_vwind = $vwind;
		$l_lon = $input[10];
		$l_lat = $input[11];
		$radius = $radius + $input[14] - $l_alt;# increasing altitude
		$l_alt = $input[14];
	    }#end if-else
	}# end if
        &printer(@input);
    }# end foreach $line
}# end sub stereographic

sub findmth{
    my $i = 0;
    while($i < scalar(@months)-1){
        if($MONTHS{$months[$i]}{"MM"} == $_[0]){
            return $months[$i];
        }
        $i++;
    }
    return "NULL";
}# end sub findmth







