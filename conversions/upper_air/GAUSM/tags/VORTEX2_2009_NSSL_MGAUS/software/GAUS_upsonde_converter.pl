#!/bin/perl -w
# Program dropsonde.pl by Darren R. Gallant JOSS February 12th, 1997
# program takes command line arguement directory dropsonde file name 
# and generates a class file with complete header information and
# reversed so that pressures are decreasing with height
# the ascension rates will remain negative indicating the data's dropsonde
# origin
use Time::Local;
$pi = 3.14159265;$radian = 180.0/$pi;
$K = 273.15;$R = 287.04;$Rv = 461.5;$ESO = 611;$EPS = .622;$G = 9.80665;
$miss_alt = 0;$miss_pos = 0;$ALT_write = 1;
#$main_dir = "./";
$header[0] = "Data Type:                         ";
$header[1] = "Project ID:                        ";
$header[2] = "Release Site Type/Site ID:         ";
$header[3] = "Release Location (lon,lat,alt):    ";
$header[4] = "UTC Release Time (y,m,d,h,m,s):    ";
$header[5] = "Input File:                        ";
$header[6] = "Nominal Release Time (y,m,d,h,m,s):";
$header[7] = "System Operator:                   ";
$header[8] = "Comments:                          ";
$header[9] = "Pre-launch Obs Data System/time:   ";
$header[10] = "Pre-launch Obs (p,t,d,h):          ";
$header[11] = "Pre-launch Obs (wd,ws):            ";
$header[12] = "Sonde Type/ID/Sensor ID/Tx Freq:   ";
#%field = (0,"time",1,"pressure",2,"temperature",3,"dewpoint",4,"RH",
#5,"Ucmp",6,"Vcmp",7,"Spd",8,"Dir",9,"Wcmp",10,"Lon",11,"Lat",12,"Rng",
#13,"Azi",14,"Alt",15,"Qp",16,"Qt",17,"Qrh",18,"Qu",19,"Qv",20,"Qdz");
$line[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$line[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code";
$line[2] ="------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----";
opendir($RAW,"../raw_data") or die("Can't read raw_data directory.\n");
@files = grep(/D\d{4}\d{4}\_\d{6}_P\.\da.+(|\.gz)$/,readdir($RAW));
closedir($RAW);
foreach $file (@files){
if($file =~ /\.gz/){
  open(FILE,"gzcat ../raw_data/$file|") || die "Can't open $file\n";
}else{
  open(FILE,"../raw_data/$file") || die "Can't open $file\n";
}# end if-else
@infile = <FILE>;close(FILE);
$stn_id = "EOL_GAUS_";
$line = shift(@infile);
@input = split(' ',$line);
$yy = substr($input[3],0,2);# year
$MM = substr($input[3],2,2);# month
#if($MM < 10 && length($MM) == 2){ 
#    $MM = substr($MM,1,1);
#}elsif($MM >= 10){
#    if($MM == 10){$MM = "a";}
#    elsif($MM == 11){$MM = "b";}
#    elsif($MM == 12){$MM = "c";}
#}# end if
$dd = substr($input[3],4,2);# day
$hh = substr($input[4],0,2);# actual hour
$mm = substr($input[4],2,2);# minutes
$ss = substr($input[4],4,2);# seconds
$outfile = "../output/".$stn_id."20".$yy.$MM.$dd.$hh.$mm.".cls";
$errfile = "../output/".$stn_id."20".$yy.$MM.$dd.$hh.$mm.".err";
print "infile:  $file\n";
print "outfile: $outfile\n";
open(OUT,">$outfile") || die "Can't open $outfile\n";
open(ERR,">$errfile") || die "Can't open $errfile\n";
if($ALT_write){open(ALT,">$outfile.alt") || die "Can't open $outfile.alt\n";}
print ERR "Input file:$file\n";
@metdata = &getheader(reverse @infile);&writeheader;
$beg_time = &findbeg(@metdata);
print "Beginning time:$beg_time\n";
@metdata = &writedata(@metdata);
if(grep(/9{3|4|5}\.0/,@metdata)){
  print "$outfile contains missing data\n";
  &missingdata(@metdata);
}# end if
if(scalar(@metdata) > 0){
  @metdata = &zerotime(@metdata);
  
  if($miss_alt || $miss_pos){&find_missing(reverse @metdata);}# end if
}# end if
$line_cnt = 0;$first = 1;
foreach $line(@header_lines){
   print OUT $line; 
   if($ALT_write){print ALT $line;}
}# end foreach
foreach $line(reverse(@metdata)){
    @output = split(' ',$line);
    &printer(@output);
    if($ALT_write){
	if($line_cnt == 0){$first_time = $output[0];}
	if($first){
	    if($output[14]!=99999.0 && $output[2]!=999.0 && $output[3]!=999.0){
		$first = 0;
		if($output[1] > 970.0){
		    $output[14] = abs($first_time - $output[0])*12.0;
		}
		$last_time = $output[0];
		$last_alt = $output[14];
		$last_press = $output[1];
		$last_temp = $output[2];
		$last_dewpt = $output[3];
		$T1 = $last_temp + $K;
		$DT1 = $last_dewpt + $K;
	    }
	}else{
	    $output[14] = 99999.0;
	    &calc_alt;
	}
	#$output[9] = 999.0;$output[20] = 9.0;
	print ALT &line_printer(@output);
    }
    $line_cnt++;
}

print "File contains $line_cnt lines of data\n";
print ERR "$outfile contains $line_cnt lines of data\n";
print "FINI\n";
close(OUT);close(ERR);

#last;

}# end foreach 

sub find_missing{
    if(scalar(@missing) == 1){
      @input = split(' ',$missing[0]);
      if($input[13] != 99999.0){ # keep non-missing altitude
        $alt = $input[13];$miss_alt = 0;
      }# end if
      if($input[11] != 999.0){ # keep non-missing position data
        $lon = $input[11];$lat = $input[12];$miss_pos = 0;
      }# end if
      if(!$miss_alt && !$miss_pos){ # have valid alt,lat, and lon don't look
        $looking = 0;
      }else{
        $looking = 1;
      }# end if-else
    }else{ # look for valid altitude and position data for header
      $i = 0;$looking = 1;
    }# end if-else
    while($looking and $_[$i]){
      @input = split(' ',$_[$i]);
      $time = $input[0];
      if($miss_alt && $time <= 15.0){
        if($input[14] != 99999.0){
          $alt = $input[14];$miss_alt = 0;
          print "Found altitude:$alt at time $time\n";
        }# end if
      }# end if
      if($miss_pos && $time <= 15.0){
        if($input[11] != 999.0){
          $lon = $input[10];$lat = $input[11];
          $miss_pos = 0;
          print "Found lon: $lon and lat: $lat at time $time\n";
        }# end if
      }# end if
      if($time > 10.0){ $looking = 0;}# end if
      $i++;
    }# end while
    if(!$miss_alt && !$miss_pos){
      $stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %9.5f, %9.5f, %7.1f",&calc_pos($lon,$lat),$alt);
      $header_lines[3] = sprintf("%s%s\n",$header[3],$stn_loc);
    }elsif(!$miss_alt){
      $stn_loc="999 99.99'W, 99 99.99'N, -99.99999,  99.99999,";
      $header_lines[3] = sprintf("%s%s %7.1f\n",$header[3],$stn_loc,$alt);
    }elsif(!$miss_pos){
      $stn_loc=", 99999.9";
      $stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %9.5f, %9.5f, %7.1f",&calc_pos($lon,$lat),"99999.9");
      $header_lines[3] = sprintf("%s%s\n",$header[3],$stn_loc);
    }# end if
}# end sub find_missing

sub missingdata{
    $i = 0;$miss_p = 0;$miss_t = 0;$miss_rh = 0;$miss_wnd = 0;
    $miss_position = 0;$miss_altitude = 0;
    while($_[$i]){
      @input = split(' ',$_[$i]);
      if($input[1] == 9999.0){$miss_p++;}
      if($input[2] == 999.0){$miss_t++;}
      if($input[4] == 999.0){$miss_rh++;}
      if($input[7] == 999.0){$miss_wnd++;}
      if($input[10] == 9999.0){$miss_position++;}
      if($input[14] == 99999.0){$miss_altitude++;}
      $i++;
    }# end while
    $line_cnt = scalar(@_);
    #print ERR "$file contains $total_lines missing press after launch point\n";
    print ERR "                  $miss_p missing pressure(s)\n";
    print ERR "                  $miss_t missing temperature(s)\n";
    print ERR "                  $miss_rh missing rh(s)\n";
    print ERR "                  $miss_wnd missing wind values\n";
    print ERR "                  $miss_position missing position data/datum\n";
    print ERR "                  $miss_altitude missing altitude(s)\n";
}# end sub missingdata

sub zerotime{
    my $i = 0;my @input = split(' ',$_[$i]);my @outfile = ();
    my $begin_time = $input[0];
    while($_[$i]){
      my @outline = &init_line;
      @input = split(' ',$_[$i]);
      $outline[0] = $input[0] - $begin_time;
      if($i == 0){
        $input[9] = 999.0;$input[20] = 9.0;
      }
      for $j(1..20){$outline[$j] = $input[$j];}
      push(@outfile,&line_printer(@outline));
      $i++;
    }# end while
    return reverse @outfile;
}# end sub zerotime

sub epochtime{
    my($yymmdd,$hhmmss) = @_;
    if($yymmdd =~ /(\d{2})(\d{2})(\d{2})/){
	($year,$mth,$day) = ($1,$2,$3);
    }# end if
    if($hhmmss =~ /(\d{2})(\d{2})(\d{2}\.\d)/){
	($hour,$min,$sec) = ($1,$2,$3);
    }#end if
    $year = "20$year";
    return timegm($sec,$min,$hour,$day,$mth-1,$year-1900);
}# end sub hhmmss_2_ss


sub findbeg{
    my @line = grep(/LAU/,@_);
    my @input = ();my $time = "99999999";my $beg_day = "99";
    my ($year,$mth,$day,$hour,$min,$sec) = (-1,-1,-1,-1,-1,-1);
    if(scalar(@line) == 1){
	@input = split(' ',$line[0]);
	$time = &epochtime(@input[3..4]);
    }else{
       my $i = 0;
       @line = grep(/P10/,@_);
       if(@line){
	   my $limit = scalar(@line);
	   @input = split(' ',$line[$i]);
	   while($i < $limit && $input[5] > 600 && $input[5] != 9999.0){
	       #print $input[5],"\n";
	       @input = split(' ',$line[$i]);
	       $time = &epochtime(@input[3..4]);
	       $i++;
	   }# end while
       }# end if
    }# end if-else
    #$time_tol = 20.0;
    if(@input > 3){$beg_day = substr($input[3],4,2);}
    print "beginning day: $beg_day\n";
    return $time;   
}# end findbeg


sub writedata{
    my @outfile = ();
    my $i = 0;my $total_lines = 0;
    my ($time,$w,$alt,$temp,$press,$WRITE_FLAG,$rap_p,$super,$dt);
    my $first = 1;
    my $l_temp = 999.0;
    my($l_alt,$l_temp_alt) = (99999.0,99999.0);
    my($l_time,$l_temp_time,$l_press) = (9999.0,9999.0,9999.0);
    my $prerelease = $pre_launch_ob[2];
    $prerelease =~ tr/[A-Za-z,():\-\#%\/]//d;
    my ($prepress,$pretemp,$predewpt,$prerh) = split(' ',$prerelease);

    while($_[$i]){
	$WRITE_FLAG = 1;
	my @outline = &init_line;
	
	$_[$i] =~ tr/[A-Za-z,():\-\#%\/]//d;#print $_[$i];

	my @input = split(' ',$_[$i]);
	if(@input == 20){
	    #$day_chk = substr($input[3],4,2);
	    $time = &epochtime($input[3],$input[4]);
	    $outline[0] = $time - $beg_time;# time

	    if($input[5] != 9999.0 && $outline[0] >= -1.5){
		
		$WRITE_FLAG = 1;
		$outline[1] = $input[5];# pressure
		    $press = $outline[1];
		$outline[15] = 99.0;# unchecked Qp
		if($input[6] != 99.00){ 
		    $outline[2] = $input[6]; # temperature
		    $outline[16] = 99.0; # unchecked Qt
		    $temp = $outline[2];
		}# end if
	        if($input[7] != 999.0){
		    $outline[4] = $input[7];# relative humidity
		    $outline[17] = 99.0;# unchecked Qrh
		    if($input[6] != 99.00){ 
			$outline[3] = &calc_dewpt($input[6],$input[7]);
                    }# end if
                }# end if
                if($input[9] != 999.0){
		    $outline[8] = $input[8];# wind direction
		    $outline[7] = $input[9];# wind speed
		    @outline = &calc_UV(@outline);
		}# end if
		if($input[11] != 999.0){
		    $outline[10] = $input[11]; # longitude
		    $outline[11] = $input[12]; # latitude
		}# end if
		$outline[14] = $input[13]; # altitude in meters
		$alt = $input[13];
		if($first){
		    
#		    printf("Zero: %s\n",join(" ",@input));
#		    printf("%s %s\t%s %s\n",$temp,$pretemp,$press,$prepress);

#		    if($temp != 999.0 && abs($temp-$pretemp) < .5 && abs($press-$prepress)<5.0){
		    if($temp != 999.0 && abs($temp-$pretemp) < 4){
			($l_time,$l_press) = @outline[0..1];
			($l_temp,$l_temp_alt) = ($temp,$alt);
			$l_temp_time = $outline[0];
			push(@outfile,&line_printer(@outline));
			$first = 0;$second = 1;
			if($alt != 99999.0){$l_alt = $alt;}
		    }#end if
       
		}elsif(($second)){

#		    printf("One: %s\n",join(" ",@input));

		    if($alt != 99999.0){
			$w = ($alt-$l_alt)/($outline[0]-$l_time);
			$outline[9] = $w;#calculated w
			$outline[20] = 99.0;#unchecked Qdz
			#print ALT "$alt $l_alt $outline[0] $l_time\n";
			if($w >= 10.0 || $w <= -40.0){$WRITE_FLAG = 0;}
		    }# end if
                    if(($WRITE_FLAG) && $temp != 999.0 && $l_temp != 999.0){
			if(($alt != 99999.0 && $l_temp_alt != 99999.0)&&($alt != $l_temp_alt)){
			    $super = 1000*($outline[2]-$l_temp)/($alt - $l_temp_alt);
			    if(abs($super) > 75.0){$WRITE_FLAG = 0;}
			    print ERR "Super $super $outline[2] $l_temp $alt $l_temp_alt\n"; 
			}elsif($outline[0]-$l_temp_time > 0){
			    $dt = $outline[0]-$l_temp_time;
			    $super = 1000*($outline[2]-$l_temp)/(10.0*$dt);
			    if(abs($super) > 75){$WRITE_FLAG = 0;}
			    print ERR "Super $super $outline[2] $l_temp $outline[0] $l_temp_time $second\n"
			}# end if-elsif
		    }# end if
#		    if($WRITE_FLAG){
			$second = 0;
			push(@outfile,&line_printer(@outline));
			($l_time,$l_press) = @outline[0..1];
			if($temp != 999.0){
			    ($l_temp,$l_temp_alt) = ($temp,$alt);
			    $l_temp_time = $outline[0];
			}#end if
			if($alt != 99999.0){$l_alt = $alt;}
#		    }# end if
		}elsif(!($second)){

#		    printf("Two: %s\n",join(" ",@input));

		    if($alt != 99999.0){
			$w = ($alt-$l_alt)/($outline[0]-$l_time);
			$outline[9] = $w;#calculated w
			$outline[20] = 99.0;#unchecked Qdz
			#print ALT "$alt $l_alt $outline[0] $l_time\n";
			if($w >= 10.0 || $w <= -40.0){$WRITE_FLAG = 0;}
		    }# end if
		    if($WRITE_FLAG){
			$rap_p = ($outline[1]-$l_press)/($outline[0]-$l_time);
			if(abs($rap_p) >= 5.0){$WRITE_FLAG = 0;}
			#print ERR "Rapid p $rap_p $outline[0] $l_time $outline[1] $l_press\n"; 
		    }# end if
		    if(($WRITE_FLAG) && $temp != 999.0 && $l_temp != 999.0){
			if(($alt != 99999.0 && $l_temp_alt != 99999.0)&&($alt != $l_temp_alt)){
			    $super = 1000*($outline[2]-$l_temp)/($alt - $l_temp_alt);
			    if(abs($super) > 75.0){$WRITE_FLAG = 0;}
			    print ERR "Super $super $outline[2] $l_temp $alt $l_temp_alt\n"; 
			}elsif($outline[0]-$l_temp_time > 0){
			    $dt = $outline[0]-$l_temp_time;
			    $super = 1000*($outline[2]-$l_temp)/(10.0*$dt);
			    if(abs($super) > 75){$WRITE_FLAG = 0;}
			    print ERR "Super $super $outline[2] $l_temp $outline[0] $l_temp_time $second\n"
			}# end if-elsif
		    }# end if
		    if($WRITE_FLAG){
			push(@outfile,&line_printer(@outline));
			($l_time,$l_press) = @outline[0..1];
			if($temp != 999.0){
			    ($l_temp,$l_temp_alt) = ($temp,$alt);
			    $l_temp_time = $outline[0];
			}#end if
			if($alt != 99999.0){$l_alt = $alt;}
		    }# end if
		}# end if-elsif
           }else{
               $total_lines++;
           }# end if
       }# end if
       $i++;  
    }#end while

    print ERR "$file contains $total_lines missing press after launch point\n";
    return(@outfile);
}# end sub writedata


sub writeheader{
   @header_lines = ();
   push(@header_lines,sprintf("%s%s\n",$header[0],$data_type));
   push(@header_lines,sprintf("%s%s\n",$header[1],$project_id));
   push(@header_lines,sprintf("%s%s\n",$header[2],$site));
   push(@header_lines,sprintf("%s%s\n",$header[3],$stn_loc));
   push(@header_lines,sprintf("%s%s\n",$header[4],$GMT));
   push(@header_lines,sprintf("%s%s\n",$header[12],$sonde_type));
   push(@header_lines,sprintf("%s%s\n",$header[9],$pre_launch_ob[3]));
   push(@header_lines,sprintf("%s%s\n",$header[10],$pre_launch_ob[2]));
   push(@header_lines,sprintf("%s%s\n",$header[11],$pre_launch_ob[1]));
   push(@header_lines,sprintf("%s%s\n",$header[7],$operator));
   push(@header_lines,sprintf("%s%s\n",$header[8],$comment));
   #push(@header_lines,sprintf(("%s\n","/")));
   push(@header_lines,sprintf("%s%s\n",$header[6],$GMT));
   for $i(0..2){push(@header_lines,sprintf("%s\n",$line[$i]));}
}# end sub writeheader
 

sub getheader{
    @pre_launch_ob = ();
    $data_type = "Dropsonde";$site = "EOL_GAUS";
    $comment = "No Header Information!";$operator = "UNKNOWN";
    for $i(0..4){$pre_launch_ob[$i] = "MISSING DATA";}
    @header_array = grep(/^GAUS-T0\d (COM|END|VER)/,@_);
    @missing = grep(/^GAUS-D0\d A00/,@_);
    #print @header_array;
    if(scalar(@header_array) > 0){
	for $i(0..scalar(@header_array)-1){shift(@_);} # removing header
	@pre_launch_ob=grep(/^GAUS-T0\d COM Pre-launch Obs/,@header_array);
	for $i(0..scalar(@pre_launch_ob)-1){
	    if($pre_launch_ob[$i]=~/^GAUS-T0\d COM Pre-launch Obs.+:\s+(.+)/){
		$pre_launch_ob[$i] = $1;
		#print $pre_launch_ob[$i],"\n";
            }# end if
        }# end for loop
	foreach $hdr_line(@header_array){
	    if($hdr_line =~ /Data Type.+:\s+(.+)/){
		$data_type = $1;
		#print $data_type,"\n"; 
	    }elsif($hdr_line =~ /Operator Name\/Comments:\s+(.+)/){
		$operator = $1;
		#print $operator,"\n";
	    }elsif($hdr_line =~ /Standard Comments:\s+(.+)/){
		$comment = $1;
		#print $comment,"\n";
	    }elsif($hdr_line =~ /Aircraft Type.+:\s+(.+)/){
		$site = $1;
		#print $site,"\n";
	    }elsif($hdr_line =~ /(Drop Name|Project Name\/Mission ID):\s+(.+)/){
		$project_id = $2;
		#print $site,"\n";
	    }elsif($hdr_line =~ /Sonde ID\/.+:\s+(.+)/){
		$sonde_type = $1;
		#print $site,"\n";
	    }elsif($hdr_line =~ /Launch Time.+:\s+(.+)/){
		$GMT = $1;
		if($GMT ne ""){
		    ($date,$time) = split(',',$GMT);$time = &trim($time);
		    ($year,$mth,$day) = split('/',$date);
		    $GMT = "$year, $mth, $day, $time";
		}else{
		    $GMT = "$20yy, $MM, $dd, $hh:$mm:$ss";
		}# end if-else
	    }# end if-elsif(4)
        }# end foreach hdr_line
        $i = 0;$stn_loc = "";
	while($pre_launch_ob[$i] && $stn_loc eq ""){
	    if($pre_launch_ob[$i] =~ /deg/ && $pre_launch_ob[$i] !~ /m\/s/){
		$pre_launch_ob[$i] =~ tr/A-Za-z,//d;
		($lon,$lat,$alt) = split(' ',$pre_launch_ob[$i]);
		$stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %9.5f, %9.5f, %7.1f",&calc_pos($lon,$lat),$alt);$miss_pos = 0;$miss_alt = 0; 
	    }# end if
	    $i++;
        }# end while
	if($stn_loc eq ""){
	    $stn_loc="999 99.99'W, 99 99.99'N, -99.99999,  99.99999, 99999.9";
	    $miss_pos = 1;$miss_alt = 1;
	}# end if
    }else{
	print "$outfile contains no header\n";
	$miss_alt = 1;$miss_pos = 1;
	print ERR "$outfile contains no header\n";
	if($MM < 10 && length($MM) < 2){ $MM = "0$MM";}
	$GMT = "20$yy, $MM, $dd, $hh:$mm:$ss";
	$stn_loc="999 99.99'W, 99 99.99'N, -99.99999,  99.99999, 99999.9";
    }# end if-else
    return reverse(@_);   
}# end sub getheader


sub trim{
    my @out = @_;
    for (@out){
	s/^\s+//;
        s/\s+$//;
    }
    return wantarray ? @out : $out[0];
}

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
     my $outline = sprintf ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]);
     return $outline;
}# end sub line_printer

sub calc_dewpt{
    my ($temp,$rh) = @_;
    my $ESO = 6.1121;
    if(($rh<0.0)||($temp==999.0)){
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

sub calc_UV{
    my $mult = 1/$radian;
    my($spd,$dir) = @_[7..8];
    if($spd < 999.0 && $dir <= 360.0 && $spd >= 0 && $dir >= 0.0){
	$_[5] = sin(($dir+180.0)*$mult)*$spd;# U wind component
	$_[6] = cos(($dir+180.0)*$mult)*$spd;# V wind component
	$_[18] = 99.0;# Unchecked Qu
	$_[19] = 99.0;# Unchecked Qv
    }# end if-else
    return @_;
}# end sub calc_UV


sub calc_pos{
    @pos_list = ();
    local ($lon,$lat) = @_;
    $lon_min = (abs($lon) - int(abs($lon)))*60;
    $lat_min = (abs($lat) - int(abs($lat)))*60;
    $lon_dir = "'W";if($lon > 0){$lon_dir = "'E";}
    $lat_dir = "'N";if($lat < 0){$lon_dir = "'S";}
    @pos_list = (abs(int($lon)),$lon_min,$lon_dir,abs(int($lat)),$lat_min,$lat_dir,$lon,$lat);
    return @pos_list;
}# end sub calc_pos

sub calc_alt {  
    $save_flag = 1;
    #/*dewpt does not have to be valid at highaltitudes*/
    if($output[1] < 200.0)
    {
       if(($save_flag) &&
          ($hold_p < $last_press))
       {
         $save_flag = 0;
         $last_press = $hold_p;
         $last_alt = $hold_alt;
         $T1 = $hold_t + $K;
         $DT1 = $hold_d + $K;
       }
       $press2 = $output[1];
       $T2 = $output[2] + $K;
       if(($output[3] > 998.0)||($DT2 < 1272))
       { #/*bad dewpoint*/
         #/*test if DT2 > 999 + 273.15,may have been reset w/ $hold_d*/
         $dens1 = &calc_density($last_press,$T1);
         $dens2 = &calc_density($press2,$T2);
         $Tbar = &calc_Tbar($dens1,$dens2,$T1,$T2);
         $output[14]=$last_alt +((($R*$Tbar)/$G)*(log($last_press/$press2)));
         $output[9] = &calc_w($output[14],$last_alt,$output[0],$last_time);
         $last_time = $output[0];
         $last_alt = $output[14];
         $last_press = $press2;
         $DT2 = $output[3] + $K;
         $DT1 = $DT2;
         $T1 = $T2;
       }elsif($output[3] <= 998.9)
       { #/*valid dewpt so use it!*/
         $DT2 = $output[3] + $K;
         $Q1 = &calc_q($last_press,$T1,$DT1);
         $Q2 = &calc_q($press2,$T2,$DT2);
         $TV1 = &calc_virT($T1,$Q1);
         $TV2 = &calc_virT($T2,$Q2);
         $dens1 = &calc_density($last_press,$TV1);
         $dens2 = &calc_density($press2,$TV2);
         $Tbar = &calc_Tbar($dens1,$dens2,$TV1,$TV2);
         $output[14] =$last_alt+((($R*$Tbar)/$G) * (log($last_press/$press2)));
         $output[9] = &calc_w($output[14],$last_alt,$output[0],$last_time);
         $last_time = $output[0];
         $last_alt = $output[14];
         $last_press = $press2;
         $T1 = $T2;
         $DT1 = $DT2;
       }
    }
    elsif(($output[1] >= 200.0 && $output[1] != 9999.0)&&
         ($output[3] < 998.0)  ){
       $press2 = $output[1];
       $T2 = $output[2] + $K;
       $DT2 = $output[3] + $K;
       $Q1 = &calc_q($last_press,$T1,$DT1);
       $Q2 = &calc_q($press2,$T2,$DT2);
       $TV1 = &calc_virT($T1,$Q1);
       $TV2 = &calc_virT($T2,$Q2);
       $dens1 = &calc_density($last_press,$TV1);
       $dens2 = &calc_density($press2,$TV2);
       $Tbar = &calc_Tbar($dens1,$dens2,$TV1,$TV2);
       $output[14] =$last_alt + ((($R*$Tbar)/$G) * (log($last_press/$press2)));
       $output[9] = &calc_w($output[14],$last_alt,$output[0],$last_time);
       $last_time = $output[0];
       $last_alt = $output[14];
       $last_press = $press2;
       $T1 = $T2;
       $DT1 = $DT2;
    }
    if($output[9] != 999.0){$output[20] = 99.0;}# Qdz unchecked
    if($output[1] != 9999.0){$output[15] = 99.0;}# Qp unchecked
    if(($save_flag) #/*savelast valit press w/ temp*/
      &&($output[1] <9998.9)
      &&($output[14] < 99998.9)
      &&($output[2] < 998.9) )
    {
      $hold_p = $output[1];
      $hold_t = $output[2];
      $hold_d = $output[3];
      $hold_alt = $output[14];
    }
    
} # end sub calc_alt


sub calc_q{
    local ($press,$temp,$dewpt) = @_;
    $L = 2500000 - 2369 * ($temp - $K);
    $X = (1/$K) - (1/$dewpt);
    $expo = ($L/$Rv) * $X;
    $es = $ESO * exp($expo);
    return (($es*$EPS)/($press*100)*(1-($es/($press*100))));
}# end sub calc_q
 
sub calc_Tbar{
    local ($d1,$d2,$t1,$t2) = @_;
    return ((($d1*$t1)+($d2*$t2))/($d1+$d2));
}# end sub calc_Tbar
 
sub calc_density{
    local ($press,$temp) = @_;
    return (($press*100)/($R*$temp));
}# end sub calc_density
 
sub calc_virT{
    local ($temp,$q) = @_;
    return ($temp*(1 + (.61 * $q)));
}# end calc_virT
 
sub calc_w{
   my($alt,$l_alt,$time,$l_time) = @_;
   $w = 0;
   $w = ($alt - $l_alt)/($time - $l_time);
   if($w < -99.9){
     return -99.9;
   }elsif($w > 999.9){
     return 999.9;
   }else{
     return $w;
   }# end if-elsif-else
}# end sub calc_w










