#!/bin/perl -w
# program DROPATDcls2QCF.pl October 7th,1999
# Perl script takes INDOEX Dropsonde qc_class files and converts them to QCF
# Darren R. Gallant JOSS
use POSIX;
$TRUE = 1;$FALSE = 0;
$data_type = "High Resolution Dropsonde Sounding";
$project_id = "IHOP 2002";
$wind_tol = 0.0001;$pi = 3.14159265;$radian = $pi/180.;$degree = 1/$radian;
$header[0] = "Data Type:                         ";
$header[1] = "Project ID:                        ";
$header[2] = "Release Site Type/Site ID:         ";
$header[3] = "Release Location (lon,lat,alt):    ";
$header[4] = "UTC Release Time (y,m,d,h,m,s):    ";
$header[5] = "Input File:                        ";
$header[6] = "Nominal Release Time (y,m,d,h,m,s):";
$header[7] = "System Operator/Comments:          ";
$header[8] = "Additional comments:               ";
$header[9] = "Sonde Type/ID/Sensor ID/Tx Freq:   ";
$header[10] = "Pre-launch Obs (p,t,d,h):          ";
$header[11] = "Pre-launch Obs (wd,ws):            ";
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
@MONTH_NAMES = keys %MONTHS;
if(@ARGV < 1){
  print "Usage is DROPATDcls2QCF.pl file(s)\n";
  exit;
}
@files = grep(/^D\d{8}(_|)(\d{4}|\d{6})_QC\.\w{3}(|\.gz)$/i,@ARGV);
@headers = grep(/^D\d{8}(_|)(\d{4}|\d{6}).*\.header(|\.gz)$/i,@ARGV);
foreach $file(@files){
    if($file =~ /\.gz/){
	#open(INFILE,"gzcat $file|") || die "Can't open $file\n";
    }else{
	#open(INFILE,$file) || die "Can't open $file\n";
    }# end if-else
    print "Opening infile: $file\n";
    &filename($file);
    &writefile($file);
    close(INFILE);
    
}# end foreach file
print "FINI\n";

sub findmth{
    my $i = 0;
    while($i < scalar(@MONTH_NAMES)-1){
	if($MONTHS{$MONTH_NAMES[$i]}{"MM"} == $_[0]){
	    return $MONTH_NAMES[$i];
        }
	$i++;
    }
    return "NULL";
}# end sub findmth

sub writeheader{
   printf OUT ("%s%s\n",$header[0],$data_type);
   printf OUT ("%s%s\n",$header[1],$project_id);
   $site = "LEAR,$launchtype";
   printf OUT ("%s%s\n",$header[2],$site);
   $stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %7.2f, %6.2f, %7.1f",&calc_pos($LON,$LAT),$ALT);
   printf OUT ("%s%s\n",$header[3],$stn_loc);
   printf OUT ("%s%s\n",$header[4],$UTC);
   printf OUT ("%s%s\n",$header[9],$sondetype);
   printf OUT ("%s%s\n",$header[10],$pre_launch_obs[0]);
   printf OUT ("%s%s\n",$header[11],$pre_launch_obs[1]);
   #printf OUT ("%s%s\n",$header[7],$comments);
   if(length($comments) > 94){
       @input = split(' ',$comments);$last = scalar(@input)-1;$j = $last - 2;
       $comment1 = join(' ',@input[0..$j]);
       $comment2 = join(' ',@input[$j+1..$last]);
       while(length($comment1) > 94){
	   $j--;
	   $comment1 = join(' ',@input[0..$j]);
	   $comment2 = join(' ',@input[$j+1..$last]);
       }# end while
       printf OUT ("%s%s\n",$header[7],$comment1);
       printf OUT ("%s%s\n",$header[8],$comment2);
       printf OUT ("%s%s\n",$header[5],$_[0]);
       #for $i(0..1){ printf OUT ("%s\n","/");}
   }else{
       printf OUT ("%s%s\n",$header[7],$comments);
       printf OUT ("%s%s\n",$header[5],$_[0]);
       printf OUT ("%s\n","/");#for $i(0..2){ printf OUT ("%s\n","/");}
   }
   printf OUT ("%s%s\n",$header[6],$UTC);
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

sub calc_w{
    local ($alt,$l_alt,$time,$l_time) = @_;
    if($time != 9999.0 && $time - $l_time > 0 && $time >= 0 && $l_time >= 0){
      $w = ($alt - $l_alt)/($time - $l_time);
      if($w > 999.9){
	  $w = 999.9;
      }elsif($w < -99.9){
	  $w = -99.9;
      }# end if-elseif
      return $w;
    }else{
      return 999.0;
    }# end if-else
}# end sub calc_w

sub by_number{$a<=>$b};

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
    if($_[0] =~ /D(\d{4})(\d{2})(\d{2})_??(\d{2})(\d{2})(\d{2})/){
	($year,$mth,$day,$hour,$min,$sec) = ($1,$2,$3,$4,$5,$6);
	$stn_id = "UCAR";
    }# end if-elsif
    $month = &findmth($mth);
    if($month ne "NULL"){
	if($day <= $MONTHS{$month}{"DAYS"}){
	    $outfile = $stn_id.$MONTHS{$month}{"FILE"}.$day.$hour.$min.".cls";
	    if($hour == 0){
		$nominal = "00:00:00";
	    }elsif($hour > 0 && $hour <= 3){
		$nominal = "03:00:00";
	    }elsif($hour > 3 && $hour <= 6){
		$nominal = "06:00:00";
	    }elsif($hour > 6 && $hour <= 9){
		$nominal = "09:00:00";
	    }elsif($hour > 9 && $hour <= 12){
		$nominal = "12:00:00";
	    }elsif($hour > 12 && $hour <= 15){
		$nominal = "15:00:00";
	    }elsif($hour > 15 && $hour <= 18){
		$nominal = "18:00:00";
	    }elsif($hour > 18 && $hour <= 21){
		$nominal = "21:00:00";
	    }else{
		$nominal = "00:00:00";$day++;
		if($day > $MONTHS{$month}{"DAYS"}){
		    $day = 1;$month = $MONTHS{$month}{"NEXT"};
		    if($MONTHS{$month}{"MM"} eq "01"){$year++;}#end if
		}#end if
	    }#end if-elsif(6)-else
            $mm = $MONTHS{$month}{"FILE"};
            $NOMINAL = "$year, $mth, $day, $nominal";    
	    open(OUT,">$outfile")||die "Can't open $outfile\n";
	    print "Class file:$outfile\n";
            &getheader;
	}# end if
   }# end if
}#end sub filename

sub getheader{
    @OUTFILE = ();
    if($_[0] =~ /D(\d{4})(\d{2})(\d{2})_??(\d{2})(\d{2})(\d{2})/){
	($year,$mth,$day,$hour,$min,$sec) = ($1,$2,$3,$4,$5,$6);
	@headerfile = grep(/^D$year$mth$day\_$hour$min$sec/,@headers);
    }# end if
    #print "header file:$headerfile[0]\n";
    open(HEAD,$headerfile[0]) || die "Can't open $headerfile[0]\n";
    foreach $line(<HEAD>){
	if($line =~ /Sonde Type.+:[ ]+(.+)/){
	    #print "$header[9]$1\n";
            $sondetype = $1;
	}elsif($line =~ /Pre-launch Obs \(p\,t\,d\,h\):[ ]+( .+)/){
	    #print "$header[10]$1\n";
            $pre_launch_obs[0] = $1;
        }elsif($line =~ /Pre-launch Obs \(wd\,ws\):[ ]+( .+)/){
	    #print "$header[11]$1\n";
            $pre_launch_obs[1] = $1;
	}elsif($line =~ /(\d{2}\.\d{5}) deg.+(.\d{1,2}\.\d{5}) deg.+(\d{4}\.\d) m/){
	    ($LON,$LAT,$ALT) = ($1,$2,$3);
        }elsif($line =~ /Operator Comments:[ ]+(.+)/){
	    #print "$header[7]$1\n";
            $comments = $1;
        }
    }# end foreach
    close(HEAD);
    open(INFILE,$_[0]) || die "Can't open $_[0]\n";
    $line_cnt = 0;
    foreach $line(<INFILE>){
	if($line =~ /Launch Site Type.+:[ ]+(.+)/){
           #print "$header[2]$1\n";
           $launchtype = $1;
        }elsif($line =~ /GMT Launch Time.+:[ ]+(.+)/){
	   #print "$header[4]$1\n";
           $UTC = $1;   
        }elsif($line =~ /System Operator.+:[ ]+(.+)/){
	   #print "$header[7]$1\n";
           #$comments = $1;
        }# end if-elsif(3)
        if($line =~ /(\-|)\d+\.\d+/ && $line !~ /[a-zA-Z\/]/){
            @input = split(' ',$line);
	    unless($input[1] == 9999.0 || $input[2] == 999.0){
		push(@OUTFILE,&check_line($line));
            }
        }# end if
	$line_cnt++;
    }# end foreach
    close(INFILE);
    
}# end sub get_header

sub writefile{
    &writeheader;$line_cnt = 0;
    my @input = ();my @OUTPUT = ();my ($time,$alt) = (99999.0,9999.0);
    foreach $line(reverse @OUTFILE){
	@input = split(' ',$line);
	if($input[0] >= 0 && $input[0] != 9999.0){
	    if($input[14] != 99999.0){
                if($time != 9999.0 && $alt != 99999.0){
		    $input[9] = &calc_w($input[14],$alt,$input[0],$time);
		    unless ($input[9] == 999.0){$input[20] = 99.0;}
                }# end if
		$time = $input[0];$alt = $input[14];
	    }# end if
        }# end if
	push(@OUTPUT,&line_printer(@input));
	$line_cnt++;
    }# end foreach
    foreach $line (reverse @OUTPUT){print OUT $line;}
    close(OUT);
}# end sub writefile

sub check_line{
    my @input = split(' ',$_[0]);my @outline = ();
    @outline = &init_line(@outline);
    foreach $i (2..14){
	if($input[$i] !~ /9{3,5}\.0/ && $i != 9){
	    $outline[$i] = $input[$i];
            #if($i == 1){$outline[15] = 99.0;}# unchecked Qp
	    if($i == 2){$outline[16] = 99.0;}# unchecked Qt
            if($i == 4){$outline[17] = 99.0;}# unchecked Qrh
            if($i == 5){$outline[18] = 99.0;}# unchecked Qu
            if($i == 6){$outline[19] = 99.0;}# unchecked Qv  
            if($i == 3){
		if($outline[$i] < -99.9){$outline[$i] = -99.9;}
            }# end if   
	}# end if
    }# end foreach
    foreach $i (0..1){
	unless($input[$i] == 9999.0){$outline[$i] = $input[$i];}
	if($i == 1){$outline[15] = 99.0;}# unchecked Qp
    }# end foreach 
    return &line_printer(@outline);
}# end sub check_line


















