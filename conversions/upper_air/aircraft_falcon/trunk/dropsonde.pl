#!/bin/perl -w
# Program dropsonde.pl by Darren R. Gallant JOSS February 12th, 1997
# program takes command line arguement directory dropsonde file name 
# and generates a class file with complete header information and
# reversed so that pressures are decreasing with height
# the ascension rates will remain negative indicating the data's dropsonde
# origin
$pi = 3.14159265;$radian = 180.0/$pi;$day_const = 86400;
$K = 273.15;$R = 287.04;$Rv = 461.5;$ESO = 611;$EPS = .622;$G = 9.80665;
$time_offset = 20;$PRESS = 600;
$miss_alt = 0;$ALT_write = 1;
$main_dir = "/raid/7/INDOEX/dropsondes";
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
#%field = (0,"time",1,"pressure",2,"temperature",3,"dewpoint",4,"RH",
#5,"Ucmp",6,"Vcmp",7,"Spd",8,"Dir",9,"Wcmp",10,"Lon",11,"Lat",12,"Rng",
#13,"Azi",14,"Alt",15,"Qp",16,"Qt",17,"Qrh",18,"Qu",19,"Qv",20,"Qdz");
$line[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$line[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code";
$line[2] ="------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----";
if(@ARGV<3){
  print "Usage is dropsonde.pl directory dropsonde file id \n";
  exit;
}#end if
$directory = shift(@ARGV);$directory = lc($directory);
$file = shift(@ARGV);
$name = shift(@ARGV);
chdir "$main_dir/$directory";
if($file =~ /\.gz/){
  open(FILE,"gzcat $file|") || die "Can't open $file\n";
}else{
  open(FILE,"$file") || die "Can't open $file\n";
}# end if-else
@infile = <FILE>;
$line = shift(@infile);
@input = split(' ',$line);
$yy = substr($input[3],0,2);# year
$MM = substr($input[3],2,2);# month
if($MM < 10 && length($MM) == 2){ $MM = substr($MM,1,1);}# end if
$dd = substr($input[3],4,2);# day
$hh = substr($input[4],0,2);# actual hour
$mm = substr($input[4],2,2);# minutes
$ss = substr($input[4],4,2);# seconds
$outfile = $name.$MM.$dd.$hh.$mm.".cls";
$errfile = $name.$MM.$dd.$hh.$mm.".err";
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
foreach $line(@metdata){
  @output = split(' ',$line);&printer(@output);
  if($line_cnt == 0){
      $first_press = $outline[1];$first_temp = $outline[2];
  }# end if
  if($ALT_write){
      if($first_press > 970.0 && $first_press != 9999.0){
      # case 1 1st pres > 970mb and valid temp - recalculate altitudes
	  if($first_temp != 999.0){
	      if($line_cnt == 0){
		  $output[14] = 0.0;
		  $last_time = $output[0];
		  $last_alt = $output[14];
		  $last_press = $output[1];
		  $last_temp = $output[2];
		  $T1 = $last_temp + $K;
		  $last_dewpt = $output[3];
		  $DT1 = $last_dewpt + $K;
	      }else{
                  if($outline[2] != 999.0 && $outline[1] != 9999.0){
		      $output[14] = 99999.0;
		      &calc_alt;
                  }# end if
              }# end if
	  }else{
	  # case 2  1st press > 970mb and missing temp set alts to missing
              if($line_cnt == 0){
		  print ERR "1st pressure $output[1] is greater than 970mb\n";
		  print ERR "but 1st temperature is missing\n";
		  print ERR "All altitudes set to missing\n";
              }# end if
	      $output[14] = 99999.0; # altitude set to missing
          }# end if-else 
      }else{
      # case 3 1st press < 970mb or missing - no altitude recalculation
	  if($line_cnt == 0){
	      print ERR "1st pressure $first_press < 970mb\n";
	      print ERR "No altitude recalculation\n";
              $ALT_write = 0;
              $cmd = "rm -f $outfile.alt";print $cmd,"\n";system($cmd);
          }# end if  
      }# end if-else
  #$output[9] = 999.0;$output[20] = 9.0;
  print ALT &line_printer(@output);
  }# end if
  $line_cnt++;
}# end foreach loop
close(OUT);
if($ALT_write){
    close(ALT);rename("$outfile.alt",$outfile);
}# end if
print "File contains $line_cnt lines of data\n";
print ERR "$outfile contains $line_cnt lines of data\n";close(ERR);
$cmd = "gzip -f $outfile";print $cmd,"\n";system($cmd);
print "FINI\n";

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
    $i = 0;$miss_t = 0;$miss_rh = 0;$miss_wnd = 0;
    $miss_position = 0;$miss_altitude = 0;
    while($_[$i]){
      @input = split(' ',$_[$i]);
      #if($input[1] == 9999.0){$miss_p++;}
      if($input[2] == 999.0){$miss_t++;}
      if($input[4] == 999.0){$miss_rh++;}
      if($input[7] == 999.0){$miss_wnd++;}
      if($input[10] == 9999.0){$miss_position++;}
      if($input[14] == 99999.0){$miss_altitude++;}
      $i++;
    }# end while
    $line_cnt = scalar(@_);
    #print ERR "$file contains $total_lines missing press after launch point\n";
    #print ERR "                  $miss_p missing pressure(s)\n";
    print ERR "                  $miss_t missing temperature(s)\n";
    print ERR "                  $miss_rh missing rh(s)\n";
    print ERR "                  $miss_wnd missing wind values\n";
    print ERR "                  $miss_position missing position data/datum\n";
    print ERR "                  $miss_altitude missing altitude(s)\n";
}# end sub missingdata

sub zerotime{
    $i = 0;@input = split(' ',$_[$i]);@outfile = ();
    $begin_time = $input[0];
    while($_[$i]){
      @outline = ();@outline = &init_line(@outline);
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

sub hhmmss_2_ss{
    return 3600*substr($_[0],0,2)+60*substr($_[0],2,2)+substr($_[0],4,4);
}# end sub hhmmss_2_ss

sub findbeg{
   @line = grep(/LAU/,@_);
   if(scalar(@line) == 1){
     @input = split(' ',$line[0]);print $input[4],"\n";
     $time = &hhmmss_2_ss($input[4]);# time
   }else{
     $i = 2;$k = 1;
     @line = grep(/P10/,@_);
     $limit = scalar(@line);
     $NOT_FOUND = 0;@input = split(' ',$line[$i]);
     $CHECK_rap_t = 1;
     while($i+$k < $limit && !$NOT_FOUND){
       @before = split(' ',$line[$i-$k]);
       @input = split(' ',$line[$i]);
       @after = split(' ',$line[$i+$k]);
       $time = &hhmmss_2_ss($input[4]);# time
       print "$input[4] $input[5] $i $limit\n";
       if($input[5] < $PRESS){
         if($before[5] < $PRESS && $after[5] < $PRESS){
	     $rap_p1=($input[5]-$before[5])/($time-&hhmmss_2_ss($before[4])); 
	     $rap_p2=($after[5]-$input[5])/(&hhmmss_2_ss($after[4])-$time);
             if(abs($rap_p2)<5.0 && abs($rap_p1)<5.0){
	       if($CHECK_rap_t && $input[6] != 99.0){
                   if($before[6] != 99 && $after[6] != 99){
		       if($input[13] > 0 && $input[13] != 99999.0){
			   $BEFORE = 0;$AFTER = 0;
                           if($before[13]>0 &&$before[13]!=99999.0){$BEFORE=1;}
                           if(abs($before-$input[13])>0){$BEFORE = 1;}
                           if($after[13]>0 && $after[13]!=99999.0){$AFTER=1;}
                           if(abs($after[13]-$before[13])>0){$AFTER = 1;}
			   if($BEFORE*$AFTER){
			       $rap_t1=($input[6]-$before[6])/($input[13]-$before[13]);
			       $rap_t2=($after[6]-$input[6])/($after[13]-$input[13]);
			       if(abs($rap_t1)<75 &&abs($rap_t2)<75){
			          $NOT_FOUND = 1;
			       }# end if(abs($rap_t1)<75)&&abs($rap_t2)<75)
			   }# end if($BEFORE*$AFTER)
                       }# end if($input[13] > 0 && $input[13] != 99999.0)
                   }# end if($before[6] != 99 && $after[6] != 99)
               }else{
	          $NOT_FOUND = 1;
	       }# end if($CHECK_rap_t && $input[6] != 99.0
	     }#end if(abs($rap_p2)<5.0) && abs($rap_p1)<5.0) 
         }# end if    
       }# end if
       $i++;
       if($i + $k == $limit && !$NOT_FOUND){
	   $CHECK_rap_t = 0;$i = 2;
       }# end if
     }# end while 
   }# end if-else
   $time_tol = $time_offset;
   $beg_day = substr($input[3],4,2);
   print "beginning day: $beg_day\n";
   print "$input[4]\n";
   return $time;   
}# end findbeg

sub writedata{
   @outfile = ();
   $i = 0;$total_lines = 0;$second = 0;
   $first = 1;$l_alt = 99999.0;$l_press = 9999.0;$l_temp = 999.0;
   $l_press_time = 9999.0;$l_time = 9999.0;$l_temp_time = 9999.0;
   $l_temp_alt = 99999.0;
   while($_[$i]){
     $write_flag = 1;
     @outline = ();@outline = &init_line(@outline);
     @input = split(' ',$_[$i]);
     #if(scalar(@input) == 14 && $_[$i] !~ /A00/ && $_[$i] !~ /COM/){
     if(scalar(@input) == 15 && $_[$i] !~ /A00/ && $_[$i] !~ /COM/){
       $day_chk = substr($input[3],4,2);
       if($day_chk > $beg_day){
         $time = &hhmmss_2_ss($input[4]) + $day_const;
       }else{
         $time = &hhmmss_2_ss($input[4]);
       }# end if
       $outline[0] = $time - $beg_time;# time
       if($input[5] != 9999.0 && $outline[0] >= 0.0){
         $outline[1] = $input[5];# pressure
         $outline[15] = 99.0;# unchecked Qp
         if($input[6] != 99.00){ 
           $outline[2] = $input[6]; # temperature
           $outline[16] = 99.0; # unchecked Qt
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
           &calc_UV($input[9],$input[8]);
         }# end if
         if($input[11] != 999.0){
           $outline[10] = $input[11]; # longitude
           $outline[11] = $input[12]; # latitude
         }# end if
         $outline[14] = $input[13]; # altitude in meters
         $alt = $input[13];
         if($first){
           if($outline[0] >= $time_tol){
             if($outline[2] != 999.0 || $outline[4] != 999.0 || $outline[7] != 999.0 && $input[2] < 30.0){
               push(@outfile,&line_printer(@outline));
               #print "Beginning $outline[0] $outline[1]\n";
               $l_press = $outline[1];$l_press_time = $outline[0];
               if($outline[2] != 999.0){
                 $l_temp = $outline[2];$l_temp_time = $outline[0];
                 $l_temp_alt = $alt;
               }# end if
               if($alt != 99999.0){$l_alt = $alt;$l_time = $outline[0];}
               $first = 0;$second = 1;
               $outline[9] = 999.0;$outline[20] = 9.0;
             }# end if
           }# end if
         }else{
           if($outline[0] - $l_time > 0){
             if($alt != 99999.0){
               $w = ($alt-$l_alt)/($outline[0]-$l_time);
               $outline[9] = $w;#calculated w
               $outline[20] = 99.0;#unchecked Qdz
               #print ALT "$alt $l_alt $outline[0] $l_time\n";
               if($w >= 10.0 || $w <= -40.0){$write_flag = 0;}
             }else{
               $outline[9] = 999.0;$outline[20] = 9.0;
             }# end if_else
           }# end if
           if($write_flag && ($outline[0]-$l_press_time > 0)){
             $rap_p = ($outline[1]-$l_press)/($outline[0]-$l_press_time);
             if(abs($rap_p) >= 5.0){$write_flag = 0;}
             #print ERR "Rapid p $rap_p $outline[0] $l_press_time $outline[1] $l_press $second\n"; 
           }# end if
           if($outline[2] != 999.0){
             if($write_flag && $outline[2] > 30.0){$write_flag = 0;}
             if($write_flag && ($alt != 99999.0 && $l_temp_alt != 99999.0)&&($alt != $l_temp_alt)){
	       $super = 1000*($outline[2]-$l_temp)/($alt - $l_temp_alt);
               if(abs($super) > 75.0){$write_flag = 0;}
               
               #print ERR "Super $super $outline[2] $l_temp $alt $l_temp_alt $second\n"; 
             }elsif($outline[0]-$l_temp_time > 0){
               $dt = $outline[0]-$l_temp_time;
               $super = 1000*($outline[2]-$l_temp)/(15.0*$dt);
               if(abs($super) > 100){$write_flag = 0;}
               #print ERR "Super $super $outline[2] $l_temp $outline[0] $l_temp_time $second\n"
             }# end if-elsif
           }# end if
           if($write_flag && ($l_press >= $outline[1])){$write_flag = 0;}
           if($outline[0] >= $l_press_time){
             if($outline[2] != 999.0 || $outline[4] != 999.0 || $outline[7] != 999.0){
               if($write_flag && $second){
                 $second = 0;
                 push(@outfile,&line_printer(@outline));
                 $l_press = $outline[1];$l_press_time = $outline[0];
                 if($outline[2] != 999.0){
                   $l_temp = $outline[2];$l_temp_time = $outline[0];
                   $l_temp_alt = $alt;
                 }# end if
                 if($alt != 99999.0){$l_alt = $alt;$l_time = $outline[0];}
               }elsif(!$write_flag && $second){
                 $second = 0;$i--;$first = 1;@outfile = ();
               }elsif($write_flag && !$second){
                 push(@outfile,&line_printer(@outline));
                 $l_press = $outline[1];$l_press_time = $outline[0];
                 if($outline[2] != 999.0){
                   $l_temp = $outline[2];$l_temp_time = $outline[0];
                   $l_temp_alt = $alt;
                 }# end if
                 if($alt != 99999.0){$l_alt = $alt;$l_time = $outline[0];}
               }# end if
             }# end if
           }# end if
         }# end if-else
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
   push(@header_lines,sprintf("%s%s\n",$header[1],"FASTEX"));
   push(@header_lines,sprintf("%s%s\n",$header[2],$site));
   push(@header_lines,sprintf("%s%s\n",$header[3],$stn_loc));
   push(@header_lines,sprintf("%s%s\n",$header[4],$GMT));
   push(@header_lines,sprintf("%s%s\n",$header[9],$pre_launch_ob[3]));
   push(@header_lines,sprintf("%s%s\n",$header[10],$pre_launch_ob[2]));
   push(@header_lines,sprintf("%s%s\n",$header[11],$pre_launch_ob[1]));
   push(@header_lines,sprintf("%s%s\n",$header[7],$operator));
   push(@header_lines,sprintf("%s%s\n",$header[8],$comment));
   push(@header_lines,sprintf(("%s\n","/")));
   push(@header_lines,sprintf("%s%s\n",$header[6],$GMT));
   for $i(0..2){push(@header_lines,sprintf("%s\n",$line[$i]));}
}# end sub writeheader
 

sub getheader{
    @pre_launch_ob = ();
    $data_type = "AVAPS SOUNDING";$site = "NCAR C-130";
    $comment = "No Header Information!";$operator = "UNKNOWN";
    for $i(0..4){$pre_launch_ob[$i] = "MISSING DATA";}
    @header_array = grep(/^AVAPS-T0\d (COM|END|VER)/,@_);
    @missing = grep(/^AVAPS-D0\d A00/,@_);
    #print @header_array;
    if(scalar(@header_array) > 0){
      for $i(0..scalar(@header_array)-1){shift(@_);} # removing header
      @pre_launch_ob=grep(/^AVAPS-T0\d COM Pre-launch Obs/,@header_array);
      #print @pre_launch_ob; 
      for $i(0..scalar(@pre_launch_ob)-1){
        if($i != 3){
          $pre_launch_ob[$i] = &remove_spaces(split(':',$pre_launch_ob[$i]));
        }else{
          $pre_launch_ob[$i]=&remove_spaces(split('Time:',$pre_launch_ob[$i]));
        }# end if-else
        chop $pre_launch_ob[$i];
        #print @pre_launch_ob;
      }# end for loop
      foreach $hdr_line(@header_array){
        if($hdr_line =~ /Data Type/){
          $data_type = &remove_spaces(split('Data Type:',$hdr_line));
          chop $data_type;
        }elsif($hdr_line =~ /System Operator/){
          $operator = &remove_spaces(split('System Operator:',$hdr_line));
          chop $operator;
        }elsif($hdr_line =~ /Comments/){
          $comment = &remove_spaces(split('Comments:',$hdr_line));
          chop $comment;
        }elsif($hdr_line =~ /Aircraft Type/){
          $site = &remove_spaces(split('ID:',$hdr_line));chop $site;
        }elsif($hdr_line =~ /Launch Time/){
          $GMT = &remove_spaces(split(':   ',$hdr_line));chop $GMT;
          if($GMT ne ""){
            ($date,$time) = split(',',$GMT);
            ($year,$mth,$day) = split('/',$date);
            $GMT = "$year, $mth, $day, $time";
          }else{
            $GMT = "$19yy, $MM, $dd, $hh:$mm:$ss";
          }# end if-else
        }# end if-elsif(4)
      }# end foreach header
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
      $GMT = "19$yy, $MM, $dd, $hh:$mm:$ss";
      $stn_loc="999 99.99'W, 99 99.99'N, -99.99999,  99.99999, 99999.9";
    }# end if-else
    return reverse(@_);   
}# end sub getheader



sub remove_spaces{
    while(substr($_[1],0,1) eq ' '){
      $_[1] = substr($_[1],1,length($_[1])-1);
    }# end while
    return $_[1];
}# end sub remove_spaces

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
    if(($rh<=0.0)||($temp==999.0)){
      return 999.0;
    }else{
      $emb = $ESO*($rh/100)* exp(17.67 * $temp/($temp+243.5));
      $dewpt = (243.5 * log($emb)-440.8)/ (19.48 - log($emb));
      if($dewpt < -99.9){
        $dewpt = -99.9;
      }# end if
    }# end if-else
    return($dewpt);
}# end sub calc_dewpt

sub calc_UV{
    local ($spd,$dir) = @_;
    $mult = 1/$radian;
    if($spd < 999.0 && $dir <= 360.0 && $spd >= 0 && $dir >= 0.0){
      $outline[5] = sin(($dir+180.0)*$mult)*$spd;# U wind component
      $outline[6] = cos(($dir+180.0)*$mult)*$spd;# V wind component
      $outline[18] = 99.0;# Unchecked Qu
      $outline[19] = 99.0;# Unchecked Qv
    }# end if-else
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
    $press2 = $output[1]; # setting current pressure
    $T2 = $output[2] + $K;#converting current Celsius temp to Kelvin
    $DT2 = $output[3] + $K;#converting curent Celsius dewpt to Kevin
    #case 1 valid temp,last_temp,dewpt,last_dewpt
    if($output[3] != 999.0 && $last_dewpt != 999.0){
	$Q1 = &calc_q($last_press,$T1,$DT1);
	$Q2 = &calc_q($press2,$T2,$DT2);
	$TV1 = &calc_virT($T1,$Q1);
	$TV2 = &calc_virT($T2,$Q2);
	$dens1 = &calc_density($last_press,$TV1);
	$dens2 = &calc_density($press2,$TV2);
	$Tbar = &calc_Tbar($dens1,$dens2,$TV1,$TV2);
	$output[14]=$last_alt + ((($R*$Tbar)/$G) * (log($last_press/$press2)));
	$output[9] = &calc_w($output[14],$last_alt,$output[0],$last_time);
	$last_time = $output[0];
	$last_alt = $output[14];
	$last_press = $press2;
	$T1 = $T2;
	$DT1 = $DT2;
    #case 2 valid temp,last_temp,dewpt
    }elsif($outline[3] != 999.0 && $last_dewpt == 999.0){
        $Q2 = &calc_q($press2,$T2,$DT2);
        $TV2 = &calc_virT($T2,$Q2);
        $dens1 = &calc_density($last_press,$TV2);
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
    # case 3 vaild temp,last_temp,last_dewpt
    }elsif($outline[3] == 999.0 && $last_dewpt != 999.0){
        $Q1 = &calc_q($last_press,$T1,$DT1);
        $TV1 = &calc_virT($T1,$Q1);
        $dens1 = &calc_density($last_press,$TV1);
        $dens2 = &calc_density($press2,$T2);
        $Tbar = &calc_Tbar($dens1,$dens2,$TV1,$T2);
        $output[14] =$last_alt+((($R*$Tbar)/$G) * (log($last_press/$press2)));
        $output[9] = &calc_w($output[14],$last_alt,$output[0],$last_time);
        $last_time = $output[0];
        $last_alt = $output[14];
        $last_press = $press2;
        $T1 = $T2;
        $DT1 = $DT2;
    # case 4 vaild temp,last_temp
    }else{
	$dens1 = &calc_density($last_press,$T1);
	$dens2 = &calc_density($press2,$T2);
	$Tbar = &calc_Tbar($dens1,$dens2,$T1,$T2);
	$output[14]=$last_alt + ((($R*$Tbar)/$G) * (log($last_press/$press2)));
	$output[9] = &calc_w($output[14],$last_alt,$output[0],$last_time);
	$last_time = $output[0];
	$last_alt = $output[14];
	$last_press = $press2;
	$T1 = $T2;
	$DT1 = $DT2;
    }# end if-elsif(2)-else
    if($output[9] != 999.0){$output[20] = 99.0;}# Qdz unchecked
    if($output[1] != 9999.0){$output[15] = 99.0;}# Qp unchecked
    $last_time = $output[0];
    $last_alt = $output[14];
    $last_press = $press2;
    $T1 = $T2;
    $DT1 = $DT2;
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
   local($alt,$l_alt,$time,$l_time) = @_;
   local $w = 0;
   if($time == $l_time){return 999.0;}
   $w = ($alt - $l_alt)/($time - $l_time);
   if($w < -99.9){
     return -99.9;
   }elsif($w > 999.9){
     return 999.9;
   }else{
     return $w;
   }# end if-elsif-else
}# end sub calc_w










