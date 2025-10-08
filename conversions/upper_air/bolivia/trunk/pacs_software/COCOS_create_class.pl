#!/bin/perl -w
# program COCOS_create_class.pl April 22th ,1998
# Darren R. Gallant JOSS
# program takes Cocos Island sounding file MMDDHHmm
# time,pressure,temp,calculated dew point,calculated u and v winds,
# calculated altitiude, calculated flight lat and lon, and missing 
# azimuth,elevation,qc values
use POSIX;
$stn_name = "Bolivia";$stn_id = "BOL";
$data_type = "5sec High Resolution Sounding";
$project_id = "PACS";
$site = "$stn_name $stn_id";
# constants
$pi = 3.14159265;$radian = 180.0/$pi;$degree = 1/$radian;
$prg = "/raid/7/PACS/cocos_island/stereographic_pos.pl";
$header[0] = "Data Type:                         ";
$header[1] = "Project ID:                        ";
$header[2] = "Launch Site Type/Site ID:          ";
$header[3] = "Launch Location (lon,lat,alt):     ";
$header[4] = "GMT Launch Time (y,m,d,h,m,s):     ";
$header[5] = "Sounding No:                       ";
$header[6] = "Nominal Launch Time (y,m,d,h,m,s): ";
$header[7] = "System Operator/Comments:          ";
$header[8] = "Additional comments:               ";
%months = ("july"=>"07","august"=>"08","september"=>"09","october"=>"10");
#%field = (0,"time",1,"pressure",2,"temperature",3,"dewpoint",4,"RH",
#5,"Ucmp",6,"Vcmp",7,"Spd",8,"Dir",9,"Wcmp",10,"Lon",11,"Lat",12,"Rng",
#13,"Azi",14,"Alt",15,"Qp",16,"Qt",17,"Qrh",18,"Qu",19,"Qv",20,"Qdz"); 
$line[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$line[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code";
$line[2] ="------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----";
$main_directory = "/net/torrent/raid/7/PACS/cocos_island/raw_files";
if(@ARGV < 1){
  print "\nUsage is COCOS_create_class.pl sounding\n\n";
  exit;
}
$file = shift(@ARGV);
open(FILE,"gzcat $file|") || die "Can't open $file file\n";
@infile = <FILE>;close(FILE);
$l_time = 9999.0;$l_alt = 99999.0;
$outfile = "$stn_id".(substr($file,1,7)).".cls";
open(OUT,">$outfile") || die "Can't open $outfile\n";
print "class file $outfile\n";@metdata = ();
foreach $line(@infile){
    $wspd = 999.0;$wdir = 999.0;
    if($line =~ /Location/){
      @input = split(' ',$line);
      $lat = abs($input[1]);
      $lat_dir = $input[2];if($lat_dir eq "S"){$lat = -(abs($lat));}
      $lon = abs($input[3]);
      $lon_dir = $input[4];if($lon_dir eq "W"){ $lon = -(abs($lon));}
      $altitude = $input[5];
    }# end if
    if($line =~ /Sounding Number/){
	@input = split(' ',$line);$snd_number = $input[2]; 
    }# end if
    if($line =~ /Launch Time/){
	@input = split(' ',$line);
        $day = $input[2];$year = $input[4];$launch_time = $input[6];
        $mth = substr($file,0,2);
    }# end if
    if($line =~ /\s+\d+\s+\d+\s+/ && $line !~ /:/){
      @input = split(' ',$line);
      @outline = ();@outline = &init_line(@outline);
      $outline[0] = $input[0]*60.0 + $input[1]; #time in seconds
      if($input[3] =~ /\d+\.\d/){
        $outline[1] = $input[3]; #pressure in mb
        $outline[15] = 99; # unchecked Qp
      }# end if
      if($input[4] =~ /(|\-)\d+\.\d/){
        $outline[2] = $input[4]; # temperature in Celsius
        $outline[16] = 99.0;# unchecked Qt
      }# end if
      if($input[5] =~ /\d+/){
	$outline[4] = $input[5]; # relative humidity in percent
        $outline[17] = 99;# unchecked Qrh           
      }# end if
      if($input[6] =~ /(|\-)\d+\.\d/){
	$outline[3] = $input[6]; # DewPt in Celsius
      }# end if
      if($outline[4] < 0){
	  $outline[4] = 999.0;$outline[3] = 999.0;
      }# end if
      if($input[2] =~ /\d+/){ 
	$outline[14] = $input[2]; # geopotential altitude in gpm
      }# end if
      if($outline[0] != 9990.0 && $outline[14] != 99999.0){
	  $outline[9] = &calc_w($outline[14],$l_alt,$outline[0],$l_time);
          if($outline[9] != 999.0){ $outline[20] = 99.0;} # unchecked Qdz
          $l_time = $outline[0];$l_alt = $outline[14];
      }# end if
      if($outline[0] == 0.0){
          $outline[11] = $lat; # initial latitude
          $outline[10] = $lon; # initial longitude
      }# end if
      if($input[8] =~ /\d+\.\d/){
	  $wspd = $input[8]; # wind speed in meters/second
      }# end if
      if($input[7] =~ /\d{1,3}/){
	  $wdir = $input[7]; #wind direction in degrees
      }# end if
      if($wspd != 999.0 && $wdir != 999.0){
	  $outline[7] = $wspd; # wind speed
	  $outline[8] = $wdir; # wind direction
	  &calc_UV($wspd,$wdir);
      }# end if
	  if($outline[1] != 9999.0){push(@metdata,&line_printer(@outline));}
    }# end if
}# end foreach loop
&writeheader;
foreach $line(@metdata){  
    &printer(split(' ',$line));
}# end foreach 
close(OUT);&calc_lat_lon;
$num_lines = scalar(@metdata);
print "outfile contains $num_lines lines of data\n";    
print "FINI\n";

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
   $GMT = "$year, $mth, $day, $launch_time:00";
   printf OUT ("%s%s\n",$header[4],$GMT);
   printf OUT ("%s%s\n",$header[5],$snd_number);
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
      $return = ($alt - $l_alt)/($time - $l_time);
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










