#!/bin/perl -w
# program PACS_create_class.pl November 10th ,1997
# Darren R. Gallant JOSS
# programs takes ascension number (XXX) and generates
# class format file from XXX.PTU and XXX.WND containing
# time,pressure,temp,calculated dew point,calculated u and v winds,
# calculated altitiude, calculated flight lat and lon, and missing 
# azimuth,elevation,qc values
use POSIX;
#$stn_name = "NOAA Ship Discoverer R/V  ";$stn_id = "DIS";
$stn_name = "NOAA Ship Ka`imimoana R/V  ";$stn_id = "KAI";
$data_type = "High Resolution Sounding";
$project_id = "PACS Ship sounding";
$site = "$stn_name $stn_id";
# constants
$pi = 3.14159265;$radian = 180.0/$pi;$degree = 1/$radian;
$TRUE = 1;$FALSE = 0;
$header[0] = "Data Type:                         ";
$header[1] = "Project ID:                        ";
$header[2] = "Launch Site Type/Site ID:          ";
$header[3] = "Launch Location (lon,lat,alt):     ";
$header[4] = "GMT Launch Time (y,m,d,h,m,s):     ";
$header[5] = "Ascension No:                      ";
$header[6] = "Nominal Launch Time (y,m,d,h,m,s): ";
$header[7] = "System Operator/Comments:          ";
$header[8] = "Additional comments:               ";
$header[9] = "Ascii Input File:                  ";
%months = ("july"=>"07","august"=>"08","september"=>"09","october"=>"10");
#%field = (0,"time",1,"pressure",2,"temperature",3,"dewpoint",4,"RH",
#5,"Ucmp",6,"Vcmp",7,"Spd",8,"Dir",9,"Wcmp",10,"Lon",11,"Lat",12,"Rng",
#13,"Azi",14,"Alt",15,"Qp",16,"Qt",17,"Qrh",18,"Qu",19,"Qv",20,"Qdz"); 
$line[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$line[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code";
$line[2] ="------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----";
$main_dir = "/raid3/ntdir/PACS/ship";$logfile = "SHIP.log";
$file_dir = "$main_dir/ascii_files";$class_dir = "$main_dir/class_files";
$prg = "$main_dir/stereographic_pos.pl";
%MONTHS = (
	   january => {
	       FILE => "1",
	       MM   => "01",
	       DAYS => "31",
	       SPANISH => "encro",
	       ABREV   => "jan",
               NEXT    => "feburary",
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
@months = keys %MONTHS;
if(@ARGV < 1){
  print "\nUsage is PACS_create_class.pl file\n\n";
  exit;
}
$file = shift(@ARGV);
if($file =~ /\.gz/){
    open(DAT,"gzcat $file_dir/$file|") || die "Can't open $file\n";
}else{
    open(DAT,"$file_dir/$file") || die "Can't open $file\n";
}# end if
open(LOG,">>$main_dir/$logfile") || die "Can't open $logfile\n";
print "processing file $file\n";print LOG "processing file $file ";
$FILE_OPEN = $FALSE;$FIRST = $TRUE;
$line_cnt = 0;$l_time = 9999.0;$l_alt = 99999.0;
while(defined($line = <DAT>)){
    if($line =~ /\d{8}/){
        #print "Calling filename\n";
	&filename($line);
    }# end
    if($FILE_OPEN){
	if($line =~ /\d{1,2}[ ]{1,2}\d{1,2}[ ]+\d{1,5}[ ]+\d{1,4}\.\d/){
	    @input = split(' ',$line);
            @outline = ();@outline = &init_line;
            $outline[0] = $input[0]*60.0 + $input[1]; #time in seconds
	    if($input[2] =~ /\d+/){
                if($input[2] > 0){ 
		    $outline[14] = $input[2]; #altitude in meters
                    $outline[9] = &calc_w($outline[14],$l_alt,$outline[0],$l_time);
		    if($outline[9] != 999.0){ $outline[20] = 99.0;} 
                    # unchecked Qdz
		    $l_time = $outline[0];$l_alt = $outline[14];
                }# end if
	    }# end if
            if($input[3] =~ /\d+\.\d/){
                if($input[3] > 0 && $input[3] < 1050){
		    $outline[1] = $input[3]; #pressure in mb
		    $outline[15] = 99; # unchecked Qp
	        }# end if
	    }# end if
            if($input[4] =~ /(|\-)\d+\.\d/){
                if($input[4] > -99.9 && $input[4] < 1000.0){
		    $outline[2] = $input[4]; # temperature in Celsius
		    $outline[16] = 99.0;# unchecked Qt
		}# end if
	    }# end if
	    if($input[5] =~ /\d+/ && $input[6] =~ /(|\-)\d+\.\d/){
		$VALID_DEWPT = $FALSE;$VALID_RH = $FALSE;
                if($input[5] > 0 && $input[5] < 101){ $VALID_RH = $TRUE;}
                if($input[6] > -99.9 && $input[6] <= $outline[2]){
		    $VALID_DEWPT = $TRUE;
                }# end if
		if($VALID_DEWPT && $VALID_RH){
		    $outline[4] = $input[5]; # relative humidity in percent
		    $outline[3] = $input[6]; # DewPt in Celsius
		    $outline[17] = 99;# unchecked Qrh
                }# end if           
	    }# end if
            if($input[7] =~ /\d{1,3}/ && $input[8] =~ /\d+\.\d/){
		$VALID_DIR = $FALSE;$VALID_SPD = $FALSE;
                if($input[7] > 0 && $input[7] <= 360){$VALID_DIR = $TRUE;}
                if($input[8] > 0 && $input[8] < 1000){
		    $VALID_SPD = $TRUE;
		}# end if
		if($VALID_DIR && $VALID_SPD){
		    $outline[7] = $input[8]; # wind speed
		    $outline[8] = $input[7]; # wind direction
		    &calc_UV($input[8],$input[7]);
		}# end if
	    }# end if
	    if($FIRST){
		$FIRST = $FALSE;
                if($outline[14] != 99999.0){
		    $altitude = $outline[14];
	        }else{
		    $altitude = 99999.0;
                }# end if-else
		$outline[10] = $LON;$outline[11] = $LAT;
		&writeheader;
            }# end if    
	    &printer(@outline);$line_cnt++;
	}# end if
    }# end if
}# end while loop
close(DAT);
if(!FILE_OPEN){print LOG "outfile not opened";}
else{close(OUT);&calc_lat_lon;
     $cmd = "mv $class_dir/$outfile $class_dir/$year";print $cmd,"\n";
     system($cmd);
     printf LOG ("%s%s%5d%s\n",$outfile," contains",$line_cnt," lines");
     printf ("%s%s%5d%s\n",$outfile," contains",$line_cnt," lines");
}# end if
print "FINI\n";

sub calc_lat_lon{
    chdir $class_dir;
    print "Calculating Latitude and Longitude\n"; 
    $cmd = "$prg $outfile";print $cmd,"\n";system($cmd);
    rename($outfile.".3",$outfile);
    chdir $main_dir;
}# end sub calc_lat_lon

sub writeheader{
   printf OUT ("%s%s\n",$header[0],$data_type);
   printf OUT ("%s%s\n",$header[1],$project_id);
   printf OUT ("%s%s\n",$header[2],$site);
   $stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %7.2f, %6.2f, %7.1f",&calc_pos($LON,$LAT),$altitude);
   printf OUT ("%s%s\n",$header[3],$stn_loc);
   printf OUT ("%s%s\n",$header[4],$GMT);
   printf OUT  ("%s%s\n",$header[9],$file);
   for $i(0..4){ printf OUT ("%s\n","/");}
   printf OUT ("%s%s\n",$header[6],$NOMINAL);
   for $i(0..2){printf OUT ("%s\n",$line[$i]);}
}# end sub writeheader

sub init_line{
    for $i(0,1,5,6,10){$_[$i] = 9999.0;}
    for $i(2,3,4,7,8,9,11,12,13){$_[$i] = 999.0;}
    for $i(15..20){$_[$i] = 9.0;}
    $_[14] = 99999.0;
    return @_;
}# end sub init_line
    
sub printer{
    printf OUT ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]);
}# end sub printer

sub line_printer{
     $outline = sprintf ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]);
     return $outline;
}# end sub line_printer

sub calc_dewpt{
    local ($temp,$rh) = @_;
    local $ESO = 6.1121;
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
    local ($alt,$l_alt,$time,$l_time) = @_;
    $GOOD_ALT = $FALSE;$GOOD_TIME = $FALSE;
    if($time != 9999.0 && $l_time != 9999.0 && $time > $l_time){
	$GOOD_TIME = $TRUE;
    }# end if
    if($alt != 99999.0 && $l_alt != 99999.0){$GOOD_ALT = $TRUE;}
    if($GOOD_TIME && $GOOD_ALT){
      return ($alt - $l_alt)/($time - $l_time);
    }else{
      return 999.0;
    }# end if-else
}# end sub calc_w


sub calc_UV{
    local ($spd,$dir) = @_;
    if($spd < 999.0 && $dir <= 360.0 && $spd >= 0 && $dir >= 0.0){
      $outline[5] = sin(($dir+180.0)*$degree)*$spd;# U wind component
      $outline[6] = cos(($dir+180.0)*$degree)*$spd;# V wind component
      $outline[18] = 99.0;# Unchecked Qu
      $outline[19] = 99.0;# Unchecked Qv
    }# end if-else
}# end sub calc_UV

sub remove_neg_w{
    $count = scalar(@_)-1;
    $i = 0;@input = split(' ',$_[$count]);
    while($input[9] < 0 || $input[9] == 999.0){
      pop(@_);$count--;
      @input = split(' ',$_[$count]);
    }# end while
    return @_;
}# end sub remove_neg_w

sub numerically{ $a<=>$b};

sub calc_pos{
    @pos_list = ();
    local ($lon,$lat) = @_;
    $lon_min = (abs($lon) - int(abs($lon)))*60;
    $lat_min = (abs($lat) - int(abs($lat)))*60;
    $lon_dir = "'W";if($lon > 0){$lon_dir = "'E";}
    $lat_dir = "'N";if($lat < 0){$lat_dir = "'S";}
    @pos_list = (abs(int($lon)),$lon_min,$lon_dir,abs(int($lat)),$lat_min,$lat_dir,$lon,$lat);
    return @pos_list;
}# end sub calc_pos

sub filename{
    $NOT_FOUND = $TRUE;local $i = 0;
    local @input = split(' ',$_[0]);
    if($input[0] =~ /\d{8}/){
	$year = "19".substr($input[0],0,2);
	$mth = substr($input[0],2,2);
	$day = substr($input[0],4,2);
	$hour = substr($input[0],6,2);
	$LAT = $input[1];
	$LON = $input[2];
        #print "$year $month $day $hour $LAT $LON\n";
        while($NOT_FOUND && exists($MONTHS{$months[$i]})){
	    if($mth eq $MONTHS{$months[$i]}{"MM"}){
		if($day > 0 && $day <= $MONTHS{$months[$i]}{"DAYS"}){
                    $month = $months[$i];
                    $GMT = "$year, $mth, $day, $hour:00:00";
		    $hh = $hour;$dd = $day;
		    if($dd < 10 && length($dd) < 2){$dd = "0$dd";} 
                    if($hh < 10 && length($hh) < 2){$hh = "0$hh";}
		    $outfile = $stn_id.$MONTHS{$month}{"FILE"}.$dd;
		    $outfile = $outfile.$hh.".cls";
                    if($hh == 0){
			$nominal = "00:00:00";
		    }elsif($hh > 0 && $hh <= 3) {
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
			}#end if
			if($MONTHS{$month}{"MM"} eq "01"){$year++;}#end if
		    }#end if-elsif(6)-else
	            if($dd < 10 && length($dd) < 2){$dd = "0$dd";} 
                    if($hh < 10 && length($hh) < 2){$hh = "0$hh";}
                    $mm = $MONTHS{$month}{"FILE"};
                    $NOMINAL = "$year, $mth, $dd, $nominal";
                    #$outfile = $stn_id.$mm.$dd;
		    #$outfile = $outfile.(substr($nominal,0,2)).".cls";
                    open(OUT,">$class_dir/$outfile")||die "Can't open $outfile\n";
		    print "Class file:$outfile\n";
                    $NOT_FOUND = $FALSE;$FILE_OPEN = $TRUE;
		}# end if
            }# end if
	    $i++;
        }# end while loop
    }# end if
}# end sub filename








