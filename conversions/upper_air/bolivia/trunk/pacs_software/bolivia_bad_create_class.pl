#!/bin/perl 
# program galapagos_create_class.pl June, 1999 
# modified from COCOS_create_class.pl by Chris Ceazan 
# Darren R. Gallant JOSS
# program takes galapagos sounding file YYYYMMDDHH
# time,pressure,temp,calculated dew point,calculated u and v winds,
# calculated altitiude, calculated flight lat and lon, and missing 
# azimuth,elevation,qc values

use POSIX;

$FALSE = 0; $TRUE = 1;

# constants
$pi = 3.14159265;$radian = 180.0/$pi;$degree = 1/$radian;

$main_directory = "/raid3/ntdir/PACS/bolivia_raobs";
$prg = "/raid3/ntdir/PACS/bolivia_raobs/galapagos_create_class.pl";
#$logfile = "$main_directory/galapagos_log.txt";

$stn_name = "Bolivia";$stn_id = "BOLI";
$data_type = "High Resolution Sounding";
$project_id = "PACS";
$site = "$stn_name $stn_id";

$header[0] = "Data Type:                         ";
$header[1] = "Project ID:                        ";
$header[2] = "Launch Site Type/Site ID:          ";
$header[3] = "Launch Location (lon,lat,alt):     ";
$header[4] = "GMT Launch Time (y,m,d,h,m,s):     ";
$header[5] = "Sounding No:                       ";
$header[6] = "Nominal Launch Time (y,m,d,h,m,s): ";
$header[7] = "System Operator/Comments:          ";
$header[8] = "Additional comments:               ";

#%field = (0,"time",1,"pressure",2,"temperature",3,"dewpoint",4,"RH",
#5,"Ucmp",6,"Vcmp",7,"Spd",8,"Dir",9,"Wcmp",10,"Lon",11,"Lat",12,"Rng",
#13,"Azi",14,"Alt",15,"Qp",16,"Qt",17,"Qrh",18,"Qu",19,"Qv",20,"Qdz"); 

$line[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$line[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code";
$line[2] ="------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----";

%MONTHS = (
	   january => {
	       FILE => "1",
	       MM   => "01",
	       DAYS => "31",
		   SPANISH => "enero",
	       ABREV   => "jan",
           NEXT    => "february",
	   },
	   february => {
	       FILE => "2",
	       MM   => "02",
	       DAYS => "28",
	       SPANISH => "febrero",
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
		   MISSPELL => "amyo",
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
		   MISSPELL => "octubrer",
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
		},);

if(@ARGV < 1){
  print "\nUsage is galapagos_create_class.pl sounding\n\n";
  exit;
}

$file = shift(@ARGV);
open(FILE,"$main_directory/$file") || die "Can't open $file file\n";
@infile = <FILE>;close(FILE);
#open(LOG, ">>$logfile") || die "Can't open logfile\n";

$l_time = 9999.0;$l_alt = 99999.0;

if($file =~ /(\d{2})(\d{2})(\d{2})(\d{2})\.dat/){
	$mth = $1; $day = $2; $hour = $3; $min = $4; $year = "1999"; 
}

&filename; 

@metdata = ();

$line_cnt = 0;

foreach $line(@infile){
    $wspd = 999.0;$wdir = 999.0;
	$line_cnt++;
    
	if($line =~ /Launch Time/i){
		@input = split(',',$line);
        if($input[1] =~ /(\d{1,2})\s+(\w{3})\s+\d{2}/){
			$day = $1; 
			$mth = $2;
		}elsif($input[1] =~ /(\d{1,2})\-(\w{3})\-\d{2}/){
			$day = $1; $mth = $2; 
		}elsif($input[0] =~ /\w+:\s+(\d{1,2})/){
			$day = $1; 
				if($input[1] =~ /(\w{3})/){
					$mth = $1;
				}if($input[2] =~ /(\d{4})/){
					$year = $1;
				}if($input[3] =~ /\w+\s+(\d{1,2}):(\d{2})/){
					$hour = $1; $min = $2;	
				}
		}
	
		if($input[2] =~ /(\d{1,2})\s+\w+\s+(\d{1,2}):(\d{2})/){
			$year = $1;
			$hour = $2;
			$min = $3;
		}elsif($input[2] =~ /^(\d+)\s+\w+$/){ 
			$year = $1;

			if($input[3] =~ /^(\d{1,2}):(\d{1,2})/){
            	$hour = $1;
            	$min = $2;
			}
		} 
		
		
		if($year =~ /\d{2}/) {
			$year = 1900 + $year;
		}
		
   	}# end if


	if($line =~ /Pres,sure/){
		$chopped = $TRUE;
	}
		
    
	if($line =~ /\d{1,2}:\d{2}:\d{2}/){
      	@input = split(',', $line);
		if($input[0] < 0){next;} 
		@outline = ();@outline = &init_line(@outline);
		
		@time = split(/(?::|\.)/, $input[0]); 

		$outline[0] = $time[0]*60 + $time[1]*60 + $time[2]; #time in seconds
        $last_time = $outline[0];

	if($line_cnt > 10 && $offset){
		$no_check = $TRUE;
	}

	unless($no_check || $chopped){
		if(!$input[1]){
        	$offset = $TRUE;
		}
	}

	

	if($offset && $input[3] =~ /\d+\.\d+/ && $input[3] !~ /\\+/){
        $outline[1] = $input[3]; #pressure in mb
        $outline[15] = 99; # unchecked Qp
    }elsif($chopped && $input[2]){
		$outline[1] = "$input[2]" . "$input[3]";
		$outline[15] = 99; # unchecked Qp
	}elsif(!$offset && $input[2] =~ /\d+\.\d+/){
        $outline[1] = $input[2]; #pressure in mb
        $outline[15] = 99; # unchecked Qp 
	}# end elsif



		if($offset && $input[4] =~ /(|\-)\d+\.\d+/ && $input[4] !~ /\\+/){
			$outline[2] = $input[4]; # temperature in Celsius
            $outline[16] = 99.0;# unchecked Qt
        	if($input[3] =~ /\d+\.\d+\s+\-/ && $input[4] !~ /^\-/){
            	$outline[2] = -$input[4];
			}
		}elsif($chopped && $input[4] =~ /(|\-)\d+\.\d+/ && $input[4] !~ /\\+/){
            $outline[2] = $input[4]; # temperature in Celsius
            $outline[16] = 99.0;# unchecked Qt
			if($input[3] =~ /\d+\.\d+\s+\-/ && $input[4] !~ /^\-/){
                $outline[2] = -$input[4];
            }
		}elsif(!$offset && $input[3] =~ /(|\-)\d+\.\d+/ && $input[3] !~ /\\+/){
        	$outline[2] = $input[3]; # temperature in Celsius
        	$outline[16] = 99.0;# unchecked Qt
      		if($input[2] =~ /\d+\.\d+\s+\-/ && $input[3] !~ /^\-/){
				$outline[2] = -$input[3];
			}# end if
		}
      

		if($offset && $input[5] =~ /\d+/ && $input[5] !~ /\\+/ && $input[5] != 0){
			$outline[4] = $input[5]; # relative humidity in percent
            $outline[17] = 99;# unchecked Qrh
      	}elsif($chopped && $input[5] =~ /\d+/ && $input[5] !~ /\\+/ && $input[5] != 0){
			$outline[4] = $input[5]; # relative humidity in percent
            $outline[17] = 99;# unchecked Qrh
		}elsif(!$offset && $input[4] =~ /\d+/ && $input[4] !~ /\\+/ && $input[4] != 0){
			$outline[4] = $input[4]; # relative humidity in percent
        	$outline[17] = 99;# unchecked Qrh           
		}

     	if($chopped){
			$offset = $TRUE;
		}
		
		unless($offset && $input[6] =~ /\\+/ || !$offset && $input[5] =~ /\\+/){

		if($offset && $input[6] == 0 && $input[5] == 0){
			$outline[3] = 999.0 #invalid rh & dewpt
		}elsif($offset && $input[6] <= -99.9){
			$outline[3] = -99.9;
		}elsif($offset && $input[6] =~ /(|\-)\d+\.*\d*/){
			$outline[3] = $input[6]; # DewPt in Celsius
		}elsif(!$offset && $input[5] == 0 && $input[4] == 0){
			$outline[3] = 999.0 #invalid rh & dewpt
		}elsif(!$offset && $input[5] <= -99.9) {
			$outline[3] = -99.9;
		}elsif(!$offset && $input[5] =~ /(|\-)\d+\.*\d*/){
			$outline[3] = $input[5]; # DewPt in Celsius
      	}# end elsif

		}#end unless
      
	if($outline[4] < 0){
	  	$outline[4] = 999.0;$outline[3] = 999.0;
      	}# end if
      
	if(!$offset && $input[1] =~ /\d+/){ 
		$outline[14] = $input[1]; # geopotential altitude in gpm
	}elsif($offset && $chopped){
		if($input[1] =~ /\d+/){
			$outline[14] = $input[1];
		}			
	}elsif($offset && $input[1] =~ /\d+/ && $input[2] =~ /\d+/){
		$input[1] =~ /(\d+)/;
		$input[1] = "$1" . "000";
		$outline[14] = $input[1] + $input[2];
    }elsif($offset && $input[2] =~ /\d+/){
		$outline[14] = $input[2]; # geopotential altitude in gpm
	}
      
	if($outline[0] != 9999.0 && $outline[14] != 99999.0){
	  	$outline[9] = &calc_w($outline[14],$l_alt,$outline[0],$l_time);
        if($outline[9] != 999.0){ $outline[20] = 99.0;} # unchecked Qdz
        $l_time = $outline[0];$l_alt = $outline[14];
      	}# end if
      if($outline[0] == 0.0){
        $outline[11] = $lat; # initial latitude
        $outline[10] = $lon; # initial longitude
      	}# end if
      
	if(!$offset && $input[7] && $input[7] =~ /\d+\.*\d*/ && $input[7] != 0.0 && $input[7] !~ /\\+/){
	  	$wspd = $input[7]; # wind speed in meters/second
	}elsif($offset && $input[8] && $input[8] =~ /\d+\.*\d*/ && $input[8] != 0.0 && $input[8] !~ /\\+/){
		$wspd = $input[8]; # wind speed in meters/second
    }else{
		$wspd = 999.0;  
	}#end if-else
	
	if(!$offset && $input[6] && $input[6] =~ /\d{1,3}/ && $input[6] != 0 && $input[6] !~ /\\+/){
	  	$wdir = $input[6]; #wind direction in degrees
	}elsif($offset && $input[7] && $input[7] =~ /\d{1,3}/ && $input[7] != 0 && $input[7] !~ /\\+/){ 
		$wdir = $input[7]; #wind direction in degrees
	}else{
		$wdir = 999.0;
	}# end if-else

      if($wspd != 999.0 && $wdir != 999.0){
	  	$outline[7] = $wspd; # wind speed
	  	$outline[8] = $wdir; # wind direction
	  	&calc_UV($wspd,$wdir);
     	}# end if
	  
	if($outline[1] != 9999.0){push(@metdata,&line_printer(@outline));}
    	}# end if
	
	if($chopped && $offset){
		$offset = $FALSE;
	}

}# end foreach loop

&writeheader;

@rev_metdata = reverse(@metdata);

while(@rev_metdata){
	@items = split(' ',$rev_metdata[0]);
	if($items[9] < 0){
		shift(@rev_metdata);
	}else{
		@new_metdata = reverse(@rev_metdata);
		last;
	}
}


foreach $line(@new_metdata){  
    &printer(split(' ',$line));
}# end foreach 

close(OUT);
#&calc_lat_lon;

$num_lines = scalar(@metdata);
print "outfile contains $num_lines lines of data\n\n";    
print LOG "$file contains $num_lines lines of data\n\n";

sub calc_lat_lon{
    print "Calculating Latitude and Longitude\n"; 
    $cmd = " /usr/bin/nice $prg $outfile";print $cmd,"\n";system($cmd);
    rename($outfile.".3",$outfile);
}# end sub calc_lat_lon


sub writeheader{
   printf OUT ("%s%s\n",$header[0],$data_type);
   printf OUT ("%s%s\n",$header[1],$project_id);
   printf OUT ("%s%s\n",$header[2],$site);
   $stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %7.2f, %6.2f, %7.1f",&calc_pos($lon,$lat),$altitude);
   printf OUT ("%s%s\n",$header[3],$stn_loc);
   if(($day < 10) && (length($day) < 2)){ $day = "0$day";}
   if(($mth < 10) && (length($mth) < 2)){ $mth = "0$mth";}
   $actual = "$year, $mth, $day, $hour:$min:00";
   printf OUT ("%s%s\n",$header[4],$actual);
   #printf OUT ("%s%s\n",$header[5],$snd_number);
   for $i(0..4){ printf OUT ("%s\n","/");}
   #printf OUT ("\n\n\n\n\n");
   printf OUT ("%s%s\n",$header[6],$GMT);
   for $i(0..2){printf OUT ("%s\n",$line[$i]);}
}# end sub write_header

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
    if($time != 9999.0 && $time - $l_time > 0 && $time >= 0 && $l_time >= 0){
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
    }else{
      $outline[5] = 9999.0; #missing U wind component
      $outline[6] = 9999.0; #missing V wind component
      $outline[8] = 999.0;
      $outline[18] = 9.0;# missing Qu
      $outline[19] = 9.0;# missing Qv
    }# end if-else
}# end sub calc_UV

sub remove_neg_w{
    $count = scalar(@_)-1;
    $i = 0;@input = split(' ',$_[$count]);
    while($outline[9] < 0 || $outline[9] == 999.0){
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

sub filename {
	my $hh = $hour; my $mth_num = $mth; 
	my $dd = $day; $found = $FALSE; my $i = 0; @months = keys %MONTHS;
	while(!$found && defined($months[$i])){
		$month = $months[$i];
		if($mth_num == $MONTHS{$month}{"MM"}){
			$found = $TRUE;
		}else{$i++;}
	}
	if($hh >= 0 && $hh <= 2) {
        $nominal = "03:00:00";
    }elsif($hh >= 3 && $hh <= 5){
        $nominal = "06:00:00";
    }elsif($hh >= 6 && $hh <= 8){
        $nominal = "09:00:00";
    }elsif($hh >= 9 && $hh <= 11){
        $nominal = "12:00:00";
    }elsif($hh >= 12 && $hh <= 14){
        $nominal = "15:00:00";
    }elsif($hh >= 15 && $hh <= 17){
        $nominal = "18:00:00";
    }elsif($hh >= 18 && $hh <= 20){
        $nominal = "21:00:00";
    }else{
        $nominal = "00:00:00";$dd++;
	}
	if ($dd > $MONTHS{$month}{"DAYS"}){
		$dd = "01"; $month = $MONTHS{$month}{"NEXT"};  
		if ($month eq "january"){$year++;}
	}
	if ($dd < 10 && length($dd) < 2) { $dd = "0$dd";}
	$GMT = sprintf("%4d, %s, %s, %s", $year, $MONTHS{$month}{"MM"}, $dd, $nominal);
	$outfile = $stn_id.$MONTHS{$month}{"FILE"}.$day.$hour.$min.".cls";
	open(OUT,">$main_directory/class_files/bad_files/$outfile") || die "Can't open $outfile\n";
	print "class file $outfile\n";#print LOG "class file $year_from_file/$outfile\n";
} #end sub filename

