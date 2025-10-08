package Formats::Class;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
use FileHandle;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(%MONTHS %HEADER %INFO %MISS %field %CONSTANTS @line &init_line &writeheader &line_printer &printer &calc_w &calc_pos &trim);
@EXPORT_OK = qw(%mand_press &calc_UV &calc_RH &calc_dewpt &calc_alt &hydrostatic &calc_q &calc_Tbar &calc_density &calc_virT &windqc %WINDQC &remove_dropping &stereographic &check_obs &calc_WND &hydrostatic_press);
$VERSION = 1.0;
###########################################################################
# Module CLASS_FORMAT functions,associative arrays,header arrays, etc
###########################################################################
# Associative array containing required and optional header lines
###########################################################################
$HEADER{"Data Type"} = "Data Type:                         ";
$HEADER{"Project ID"} = "Project ID:                        ";
$HEADER{"Release Site"} = "Release Site Type/Site ID:         ";
$PROFILE{"Profile Site"} = "Profile Site Type/Site ID:         ";
$HEADER{"Release Loc"} = "Release Location (lon,lat,alt):    ";
$PROFILE{"Profile Loc"} = "Profile Location (lon,lat,alt):    ";
$HEADER{"UTC"} = "UTC Release Time (y,m,d,h,m,s):    ";
$PROFILE{"UTC"} = "UTC Profile Time (y,m,d,h,m,s):    ";
$HEADER{"Ascension No"} = "Ascension No:                      ";
$HEADER{"Nominal"} = "Nominal Release Time (y,m,d,h,m,s):";
$PROFILE{"Nominal"} = "Nominal Profile Time (y,m,d,h,m,s):"; 
$HEADER{"System Operator"} = "System Operator/Comments:          ";
$HEADER{"Comments"} = "Additional comments:               ";
$HEADER{"Serial Number"} = "Radiosonde Serial Number:          ";
$HEADER{"Manufacturer"} = "Radiosonde Manufacturer:           "; 
$HEADER{"Sonde Type"} = "Sonde Type/ID/Sensor ID/Tx Freq:   ";
$HEADER{"Met Processor"} = "Met Processor/Met Smoothing:       ";
$HEADER{"Winds Type"} = "Winds Type/Processor/Smoothing:    ";
$HEADER{"Input File"} = "Input File:                        ";           
###########################################################################
# Initializing associative array which contains header fields
# Program using module fills only elements to be used in header 
###########################################################################
$INFO{"Data Type"} = "";
$INFO{"Project ID"} = "";
$INFO{"Release Site"} = "";
$INFO{"Release Loc"} = "";
$INFO{"UTC"} = "";
$INFO{"Ascension No"} = "";
$INFO{"Nominal"} = "";
$INFO{"System Operator"} = "";
$INFO{"Comments"} = "";
$INFO{"Serial Number"} = "";
$INFO{"Manufacturer"} = "";
$INFO{"Sonde Type"} = "";
$INFO{"Met Processor"} = "";
$INFO{"Winds Type"} = "";
$INFO{"Input File"} = "";
##############################################################################
# Constants
# creating array of hashes holding global constants  
##############################################################################
$CONSTANTS{"Wind Tolerance"} = 0.0001; # minimum change in wind speed
$CONSTANTS{"Pi"} = 3.14159265;
$CONSTANTS{"Radian"} = $CONSTANTS{"Pi"}/180.0;
$CONSTANTS{"Degree"} = 1.0/$CONSTANTS{"Radian"};
$CONSTANTS{"Earth Radius"} = 6371220.0;# Earth's radius in meters
$CONSTANTS{"G"} = 9.80665; # m/s*s
$CONSTANTS{"ESO"} = 6.1121;# Saturation vapor pressure at 273 Kelvins in mb
$CONSTANTS{"K"} = 273.16;# 0 degrees Celsius = 273.16 degrees Kelvin
$CONSTANTS{"Rdry"} = 287.04;# Gas constant for dry air in J/(kgK)
$CONSTANTS{"Rv"} = 461.1;# Gas constant for water vapor in J/(kgK)
$CONSTANTS{"EPS"} = 0.622; # water vapor (18g/mol)/dry air (29g/mol)
$CONSTANTS{"TRUE"} = 1;
$CONSTANTS{"FALSE"} = 0;
###############################################################################
# Constants used in windqc
###############################################################################
$WINDQC{"INTERVAL"} = 120;# seconds contained in windqc interval
$WINDQC{"Questionable"} = 1.0;#flag Questionable points greater than 1.0
$WINDQC{"Bad"} = 1.5;#flag Bad points greater than 1.5
$WINDQC{"Ratio"} = 0.75;#An interval must contain at least 75% non-missing
$SHEAR{"Bad"} = 0.50;# Bad wind shear is 1.0m/s/s
$SHEAR{"Questionable"} = 0.25;# Questionable wind shear is 0.5 m/s/s
###############################################################################
# Associative array defining data and quality control character fields in JOSS Class
# Format 
##############################################################################
%field = ("time"=>0,"press"=>1,"temp"=>2,"dewpt"=>3,"RH"=>4,"Ucmp"=>5,
"Vcmp"=>6,"Spd"=>7,"Dir"=>8,"Wcmp"=>9,"Lon"=>10,"Lat"=>11,"Rng"=>12,"Azi"=>13,
"Alt"=>14,"Qp"=>15,"Qt"=>16,"Qrh"=>17,"Qu"=>18,"Qv"=>19,"Qdz"=>20);
##############################################################################
# Associative array defining missing values
##############################################################################
%MISS = ("time"=>9999.0,"press"=>9999.0,"temp"=>999.0,"dewpt"=>999.0,"RH"=>999.0,
"Ucmp"=>9999.0,"Vcmp"=>9999.0,"Spd"=>999.0,"Dir"=>999.0,"Wcmp"=>999.0,"Lon"=>9999.0,
"Lat"=>999.0,"Rng"=>999.0,"Azi"=>999.0,"Alt"=>99999.0);
##############################################################################
# Associative array defining WMO codes for mandatory levels
##############################################################################
%mand_press = ( "1000", 1, "925", 2, "850", 3, "700", 4, "500", 5, "400", 6, 
	"300", 7, "250", 8, "200", 9, "150", 10, "100", 11 , "70", 12, 
	"50", 13, "30", 14, "20", 15, "10", 16, "07", 17, "05", 18, "03", 19, 
	"02", 20, "01", 21 );  
##############################################################################
# Header lines delimiting each field, that field's units and a line of dashes 
##############################################################################
$line[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azim   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$line[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code";
$line[2] ="------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----";
$mixrat[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat MixRat  Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$mixrat[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg  g/kg   deg     m    code code code code code code";
$mixrat[2] = $line[2];
$virt[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat  VirT   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$virt[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg    K    deg     m    code code code code code code";
$virt[2] = $line[2];
$rho[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Rho  Azim    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$rho[1] = "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg  g/m^3  deg     m    code code code code code code";
$rho[2] = $line[2];
###############################################################################
# Hash of Associative arrays containing month names and aliases
############################################################################## 
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
               LEAP => "29",
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
##############################################################################
# Function trim
# function removes leading and trailing whitespace from an array or a string
##############################################################################
sub trim{
    my @out = @_;
    for (@out){
	s/^\s+//;
        s/\s+$//;
    }
    return wantarray ? @out : $out[0];
}
##############################################################################
# Subroutine writeheader
# writes header lines to class file whenever corresponding %INFO value not 
#equal to the null string
##############################################################################
sub writeheader{
    my ($LAT,$LON,$ALT) = ($INFO{"LAT"},$INFO{"LON"},$INFO{"ALT"});
    my ($TRUE,$FALSE) = ($CONSTANTS{"TRUE"},$CONSTANTS{"FALSE"});
    my ($MIX_RAT,$VIRT,$PROFILE,$RHO) = ($FALSE,$FALSE,$FALSE,$FALSE);
    my @tmp = ();my @input = ();my ($last,$comment1,$comment2) = ("","","");
    my $backslashes = 5;
    push(@tmp,sprintf("%s%s\n",$HEADER{"Data Type"},$INFO{"Data Type"}));
    push(@tmp,sprintf("%s%s\n",$HEADER{"Project ID"},$INFO{"Project ID"}));
    if($INFO{"Data Type"} =~ /Profile/i){$PROFILE = $TRUE;}# end if
    $stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %8.3f, %7.3f, %7.1f",&calc_pos($LON,$LAT),$ALT);
    if($PROFILE){
	push(@tmp,sprintf("%s%s\n",$PROFILE{"Profile Site"},$INFO{"Release Site"}));
	push(@tmp,sprintf("%s%s\n",$PROFILE{"Profile Loc"},$stn_loc));
	push(@tmp,sprintf("%s%s\n",$PROFILE{"UTC"},$INFO{"UTC"}));
    }else{
	push(@tmp,sprintf("%s%s\n",$HEADER{"Release Site"},$INFO{"Release Site"}));
	push(@tmp,sprintf("%s%s\n",$HEADER{"Release Loc"},$stn_loc));
	push(@tmp,sprintf("%s%s\n",$HEADER{"UTC"},$INFO{"UTC"}));
    }# end if-else
    if($INFO{"Manufacturer"} ne ""){
	push(@tmp,sprintf("%s%s\n",$HEADER{"Manufacturer"},$INFO{"Manufacturer"}));
	$backslashes--;
    }# end if
    if($INFO{"Sonde Type"} ne ""){
	push(@tmp,sprintf("%s%s\n",$HEADER{"Sonde Type"},$INFO{"Sonde Type"}));
	$backslashes--;
    }# end if
    if($INFO{"Winds Type"} ne ""){
	push(@tmp,sprintf("%s%s\n",$HEADER{"Winds Type"},$INFO{"Winds Type"}));
	$backslashes--;
    }# end if
    if($INFO{"Met Processor"} ne ""){
	push(@tmp,sprintf("%s%s\n",$HEADER{"Met Processor"},$INFO{"Met Processor"}));
	$backslashes--;
    }# end if
    if($INFO{"System Operator"} ne ""){
	if(length($INFO{"System Operator"}) > 94){
	    @input = split(' ',$INFO{"System Operator"});
	    $last = scalar(@input)-1;$j = $last - 2;
	    $comment1 = join(' ',@input[0..$j]);
	    $comment2 = join(' ',@input[$j+1..$last]);
	    while(length($comment1) > 94){
		$j--;
		$comment1 = join(' ',@input[0..$j]);
		$comment2 = join(' ',@input[$j+1..$last]);
	    }# end while
	    push(@tmp,sprintf("%s%s\n",$HEADER{"System Operator"},$comment1));
	    $backslashes--;
	    push(@tmp,sprintf("%s%s\n",$HEADER{"Comments"},$comment2));
            $backslashes--;
	}else{
	    push(@tmp,sprintf("%s%s\n",$HEADER{"System Operator"},$INFO{"System Operator"}));
	    $backslashes--;
	}# end if-else
    }# end if
    if($INFO{"Comments"} ne "" && $backslashes){
	push(@tmp,sprintf("%s%s\n",$HEADER{"Comments"},$INFO{"Comments"}));
	if($INFO{"Comments"} =~ /Water Vapor Mixing Ratio/i){
	    $MIX_RAT = $TRUE;
        }elsif($INFO{"Comments"} =~ /Virtual Temperature/i){
	    $VIRT = $TRUE;
        }elsif($INFO{"Comments"} =~ /Water Vapor Density/i){
	    $RHO = $TRUE;
        }# end if-elsif

	$backslashes--;
    }# end if
    if($INFO{"Serial Number"} ne "" && $backslashes){
	push(@tmp,sprintf("%s%s\n",$HEADER{"Serial Number"},$INFO{"Serial Number"}));
	$backslashes--;
    }# end if
    if($INFO{"Ascension No"} ne "" && $backslashes){
	push(@tmp,sprintf("%s%s\n",$HEADER{"Ascension No"},$INFO{"Ascension No"}));
	$backslashes--;
    }# end if
    if($INFO{"Input File"} ne "" && $backslashes){
	push(@tmp,sprintf("%s%s\n",$HEADER{"Input File"},$INFO{"Input File"}));
	$backslashes--;
    }# end if
    for $i(0..$backslashes){ push(@tmp,sprintf("%s\n","/"));}
    if($INFO{"Nominal"} ne ""){
        if($PROFILE){
	    push(@tmp,sprintf("%s%s\n",$PROFILE{"Nominal"},$INFO{"Nominal"}));
        }else{
	    push(@tmp,sprintf("%s%s\n",$HEADER{"Nominal"},$INFO{"Nominal"}));
        }# end if-else
    }else{
        if($PROFILE){
	    push(@tmp,sprintf("%s%s\n",$PROFILE{"UTC"},$INFO{"UTC"}));
	}else{
	    push(@tmp,sprintf("%s%s\n",$HEADER{"UTC"},$INFO{"UTC"}));
        }# end if-else
    }# end if
    for $i(0..2){
        if($MIX_RAT){
            push(@tmp,sprintf("%s\n",$mixrat[$i]));
        }elsif($VIRT){
	    push(@tmp,sprintf("%s\n",$virt[$i]));
        }elsif($RHO){
            push(@tmp,sprintf("%s\n",$rho[$i]));
	}else{
	    push(@tmp,sprintf("%s\n",$line[$i]));
        }# end if-else
    }# end for loop
    return @tmp;
}# end sub writeheader
##############################################################################
# Function init_line
# initializes an array representing a class file line with correct missing 
# value codes
##############################################################################
sub init_line{
    my @tmp = ();
    for $i("time","press","Ucmp","Vcmp","Lon"){
	$tmp[$field{$i}] = $MISS{$i};
    }
    for $i("temp","dewpt","RH","Spd","Dir","Wcmp","Lat","Rng","Azi"){
	$tmp[$field{$i}] = $MISS{$i};
    }
    for $i("Qp","Qt","Qrh","Qu","Qv","Qdz"){
	$tmp[$field{$i}] = 9.0;
    }
    $tmp[$field{"Alt"}] = $MISS{"Alt"};
    return @tmp;
}# end sub init_line
############################################################################
# Function line_printer: usage &line_printer(@array);
# prints out a class file line to a string
############################################################################
sub line_printer{
    $outline = sprintf ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]); 
    return $outline;
}# end sub line_printer
############################################################################
# Function printer: usage &printer(\@array,\$fh);
# prints out a class file line to file handle
############################################################################
sub printer{
    my ($array_ref,$fh_ref) = @_;
    my $fh = $$fh_ref;
    printf $fh("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@{$array_ref}[0..20]); 
    return $outline;
}# end sub line_printer
############################################################################
# Function check_obs: usage BOOLEAN = &check_obs(\@array,\@array)
# function checks that time and altitude are increasing and that pressure
# is decreasing. Returns TRUE if conditions are met and FALSE otherwise
############################################################################
sub check_obs{
    my($current_ref,$previous_ref) = @_;
    my @current = @{$current_ref};
    my @previous = @{$previous_ref};
    my($TRUE,$FALSE) = ($CONSTANTS{"TRUE"},$CONSTANTS{"FALSE"});
    my $BOOLEAN = $TRUE;
    foreach $elem("time","press","Alt"){
	$BOOLEAN*=&compare($elem,$current[$field{$elem}],$previous[$field{$elem}]);
	#print "$BOOLEAN $elem\n";
    }# end foreach
    return $BOOLEAN;
    
}
###########################################################################
# Function compare: usage BOOLEAN = &compare(\$elem,\$current,\$previous)
# function compares $current and $previous observations of type $elem
# and returns TRUE or FALSE 
###########################################################################
sub compare{
    my($elem,$current,$previous) = @_;
    my($TRUE,$FALSE) = ($CONSTANTS{"TRUE"},$CONSTANTS{"FALSE"});
    my %MISSING;
    $MISSING{"time"} = $MISSING{"press"} = 9999.0;
    $MISSING{"Alt"} = 99999.0;
    if($current != $MISSING{$elem} && $previous != $MISSING{$elem}){
	if($elem eq "press"){
	    if($current <= $previous){
		return $TRUE;
            }else{
		return $FALSE;
            }# if-else
        }else{
	    if($current >= $previous){
		return $TRUE;
            }else{
		return $FALSE;
            }# if-else
	}# end if-else
    }else{
	return $TRUE;
    }# end if-else
}# end sub compare
############################################################################
# Function calc_dewpt: usage @outline = &calc_dewpt(@outline)
# calculates dew point in Celsius using temperature in celsius and relative 
# humidity in whole percent
############################################################################
sub calc_dewpt{
    my $ESO = $CONSTANTS{"ESO"};my $emb;my $dewpt = 999.0;
    my @outline = @_;
    my ($temp,$rh) = ($outline[$field{"temp"}],$outline[$field{"RH"}]);
    if($rh<0.0||$temp==$MISS{"temp"}||$rh==$MISS{"RH"}){
	$outline[$field{"dewpt"}] = $dewpt;
    }else{
	if($rh == 0){ $rh = 0.005;}
	$emb = $ESO*($rh/100)* exp(17.67 * $temp/($temp+243.5));
	$dewpt = (243.5 * log($emb)-440.8)/(19.48 - log($emb));
	if($dewpt < -99.9){
	    $dewpt = -99.9;$outline[$field{"Qrh"}] = 4.0;# estimated Qrh
	}# end if
        $outline[$field{"dewpt"}] = $dewpt;
    }# end if-else
    return @outline;
}# end sub calc_dewpt
############################################################################
#Function calc_RH usage @outline = &calc_RH(@outline)
# function calculates relative humidity using dew point and temperature in
# celsius
#############################################################################
sub calc_RH{
    my @outline = @_;
    my($temp,$dewpt) = ($outline[$field{"temp"}],$outline[$field{"dewpt"}]);
    my $ESO = $CONSTANTS{"ESO"};my $emb;
    if($temp != 999.0 && $dewpt != 999.0 && $temp >= $dewpt){
	if($dewpt < -99.9){
	    $outline[$field{"dewpt"}] = -99.9;
            $outline[$field{"Qrh"}] = 4.0;# estimated Qrh
        }# end if
	$emb = exp(($dewpt*19.4817+440.8)/($dewpt+243.5));
        $outline[$field{"RH"}] = (100.0*$emb)/((exp(17.67*$temp/($temp+243.4))*$ESO));
        $outline[$field{"Qrh"}] = 99.0 unless $outline[$field{"Qrh"}] == 4.0;
    }# end if
    return @outline;
}# end sub calc_RH
#############################################################################
# Function calc_w: 
# usage ascentrate = &calc_w(Alt,previous Alt,Time,previous time)
# function calculates sonde's ascent/descent rate using time and altitude
############################################################################# 
sub calc_w{
    my ($alt,$l_alt,$time,$l_time) = @_;my $Wcmp;
    if($time != 9999.0 && $time - $l_time > 0 && $time >= 0 && $l_time >= 0){
      $Wcmp = ($alt - $l_alt)/($time - $l_time);
      if($Wcmp > 999.9){
	  return 999.9;
      }elsif($Wcmp < -99.9){
	  return -99.9;
      }else{
	  return $Wcmp;
      }# end if
    }else{
      return 999.0;
    }# end if-else
}# end sub calc_w
#############################################################################
# Subroutine calc_UV usage &calc_UV($spd,$dir)
# subroutine calculates u and v wind components using the wind speed and 
# direction
#############################################################################
sub calc_UV{
    my @outline = @_;
    my ($spd,$dir) = ($outline[$field{"Spd"}],$outline[$field{"Dir"}]);
    my $pi = $CONSTANTS{"Pi"};my $radian = $CONSTANTS{"Radian"};
   if($spd < 999.0 && $dir <= 360.0 && $spd >= 0 && $dir >= 0.0){
	$outline[$field{"Ucmp"}]=sin(($dir+180.0)*$radian)*$spd;# Ucmp
	$outline[$field{"Vcmp"}] = cos(($dir+180.0)*$radian)*$spd;# Vcmp
        $outline[$field{"Qu"}] = 99.0;# Unchecked Qu
        $outline[$field{"Qv"}] = 99.0;# Unchecked Qv
    }else{
        $outline[$field{"Ucmp"}] = 9999.0; #missing U wind component
        $outline[$field{"Vcmp"}] = 9999.0; #missing V wind component
        $outline[$field{"Dir"}] = 999.0;# missing Wind Direction
        $outline[$field{"Qu"}] = 9.0;# missing Qu
        $outline[$field{"Qv"}] = 9.0;# missing Qv
    }# end if-else
    return @outline;
}# end sub calc_UV
#############################################################################
# Subroutine calc_WND usage &calc_WND($spd,$dir)
# subroutine calculates total wind speed and direction using U and V wind 
# components
#############################################################################
sub calc_WND{
    my @outline = @_;
    my ($U,$V) = ($outline[$field{"Ucmp"}],$outline[$field{"Vcmp"}]);
    my $degree = $CONSTANTS{"Degree"};
    use POSIX;
    if($U != $MISS{"Ucmp"} && $V != $MISS{"Vcmp"}){
	$outline[$field{"Spd"}] = sqrt($U**2 + $V**2); # total wnd speed
	if($U == 0){$U = 0.000001;}
	$outline[$field{"Dir"}] = abs(POSIX::atan($V/$U))*$degree; # wnd direction
        SWITCH:{
	    if($U > 0 && $V > 0){
		$outline[$field{"Dir"}] = 90.0 - $outline[$field{"Dir"}];
		last SWITCH;
            }# end if
	    if($U < 0 && $V > 0){
		$outline[$field{"Dir"}] = 270.0 + $outline[$field{"Dir"}];
		last SWITCH;
            }# end if
            if($U < 0 && $V < 0){
		$outline[$field{"Dir"}] = 270 - $outline[$field{"Dir"}];
		last SWITCH;
            }# end if
	    if($U > 0 && $V < 0){
		$outline[$field{"Dir"}] = 90.0 + $outline[$field{"Dir"}];
		last SWITCH;
            }# end if
	}# end SWITCH
	$outline[$field{"Dir"}] +=180.0;
	if($outline[$field{"Dir"}] > 360.0){$outline[$field{"Dir"}]-=360.0;}
	if($outline[$field{"Spd"}] > 999.9){
            $outline[$field{"Spd"}] = 999.0; #missing total wind speed
	    $outline[$field{"Ucmp"}] = 9999.0; #missing U wind component
	    $outline[$field{"Vcmp"}] = 9999.0; #missing V wind component
	    $outline[$field{"Dir"}] = 999.0;# missing Wind Direction
	    $outline[$field{"Qu"}] = 9.0;# missing Qu
	    $outline[$field{"Qv"}] = 9.0;# missing Qv
        }# end if
    }# end if
    return @outline;
}# end sub calc_WND
#############################################################################
# Subroutine calc_pos: usage $string = calc_pos(Lon,Lat)
# subroutine creates a decimal and whole degree,minutes and seconds equivalent
# for longitude and latitude
#############################################################################
sub calc_pos{
    my @pos_list = ();my ($lon,$lat) = @_;
    $lon_min = (abs($lon) - int(abs($lon)))*60.0;
    $lat_min = (abs($lat) - int(abs($lat)))*60.0;
    $lon_dir = "'W";if($lon > 0){$lon_dir = "'E";}
    $lat_dir = "'N";if($lat < 0){$lat_dir = "'S";}
    @pos_list = (abs(int($lon)),$lon_min,$lon_dir,abs(int($lat)),$lat_min,$lat_dir,$lon,$lat);
    return @pos_list;
}# end sub calc_pos
#############################################################################
# Subroutine stereographic: usage @OUTFILE = &stereographic(@OUTFILE)
# Subroutine calculates a decimal latitude and longitude using U and V wind
# components, time, and altitude. Routine performs a stereographic projection
# centered on the beginning Lat and Lon and calculates the change in position
# as the sonde ascends/descends
#############################################################################  
sub stereographic{
    my($wind_tol,$radius) = ($CONSTANTS{"Wind Tolerance"},$CONSTANTS{"Earth Radius"});
    my($pi,$radian,$degree) = ($CONSTANTS{"Pi"},$CONSTANTS{"Radian"},$CONSTANTS{"Degree"});
    my ($time,$uwind,$vwind);my @input = ();my ($array_ref,$fh_ref) = @_;;
    my $fh = $$fh_ref;
    my %LAST;my($difft,$diffu,$diffv,$rho,$c,$elem);
    foreach $elem("time","Ucmp","Vcmp","Lat","Lon","Alt"){$LAST{$elem}=0;}
    use POSIX;
    my $first = 0;
    foreach $line(@{$array_ref}){
	@input = split(' ',$line);
	if($first == 0){
	    print "Calculating longitude and latitude\n";
            foreach $elem("time","Ucmp","Vcmp","Lat","Lon","Alt"){$LAST{$elem} = $input[$field{$elem}];}
	    $radius = $radius + $input[$field{"Alt"}];
	    $first++;
	}else{
	    $time = $input[$field{"time"}];$uwind = $input[$field{"Ucmp"}];
            $vwind = $input[$field{"Vcmp"}];
	    if(($LAST{"Lon"} == 9999.000)||($uwind == 9999.0)||($LAST{"Ucmp"} == 9999.0)){
		$input[$field{"Lon"}] = 9999.000;
		$input[$field{"Lat"}] = 999.000;
	    }else{
		if(abs($uwind) <= $wind_tol && abs($vwind) <= $wind_tol){
		    $input[$field{"Lat"}] = $LAST{"Lat"};
		    $input[$field{"Lon"}] = $LAST{"Lon"};
		}else{
		    $difft=abs($LAST{"time"}-$time);$diffu=($uwind+$LAST{"Ucmp"})/2.0;
		    $diffv = ($vwind + $LAST{"Vcmp"})/2.0;
		    $x = $difft*$diffu;
		    $y = $difft*$diffv;
		    $rho = sqrt($x*$x + $y*$y);
		    #printf("%8.3f %8.3f %8.3f\n",$difft,$difft,$rho);
		    $c = 2.0*POSIX::atan($rho/(2.0*$radius*1.0));
		    $input[$field{"Lat"}] = POSIX::asin(cos($c)*sin($LAST{"Lat"}*$radian)+$y*sin($c)*cos($LAST{"Lat"}*$radian)/$rho);$input[$field{"Lat"}] = $input[$field{"Lat"}]*$degree;# latitude
		    $input[$field{"Lon"}] = $LAST{"Lon"} + POSIX::atan($x*sin($c)/($rho*cos($LAST{"Lat"}*$radian)*cos($c)-($y*sin($LAST{"Lat"}*$radian)*sin($c))))*$degree;# longitude
		}# end if-else
                $radius=$radius+$input[$field{"Alt"}]-$LAST{"Alt"};#increasing alt
                foreach $elem("time","Ucmp","Vcmp","Lat","Lon","Alt"){$LAST{$elem} = $input[$field{$elem}];}
	    }#end if-else
	}# end if
        &printer(\@input,\$fh);
    }# end foreach $line
}# end sub stereographic
##############################################################################
# Subroutine calc_alt usage @outfile = &calc_alt(@outfile);
# subroutine calculates altitude using hydrostatic equation using virtual when
# valid dew point available and when pressure is 400mb or more
##############################################################################
sub calc_alt {
    my @OUTFILE = @_;my @ALTFILE = ();
    my ($TRUE,$FALSE,$K) = ($CONSTANTS{"TRUE"},$CONSTANTS{"FALSE"},$CONSTANTS{"K"});
    my $FIRST = $TRUE;
    my ($Tbar,@pressure,$line,%DATA,%LAST,%CURRENT);
    foreach $elem ("time","press","temp","dewpt","alt","mixing ratio","density","virtual t"){
	$LAST{$elem} = 0.0;$CURRENT{$elem} = 0.0;
    }# end foreach $elem
    
    foreach $line(@OUTFILE){
	@pressure = split(' ',$line);
	if($FIRST){
	    if($pressure[$field{Alt}] != 99999.0){
		if($pressure[$field{"press"}] != 9999.0 && $pressure[$field{"temp"}] != 999.0){
		    $LAST{"alt"} = $pressure[$field{"Alt"}];
		    $LAST{"press"} = $pressure[$field{"press"}];
		    $LAST{"temp"} = $pressure[$field{"temp"}]+$K;
		    $LAST{"dewpt"} = $pressure[$field{"dewpt"}]+$K;
                    $LAST{"time"}  = $pressure[$field{"time"}];
		    if($FIRST){$FIRST = $FALSE;}
		}# end if
	    }# end if 
        }else{
            if($pressure[$field{"temp"}] != 999.0 && $pressure[$field{"press"}] != 9999.0){
                $CURRENT{"press"} = $pressure[$field{"press"}];
                $CURRENT{"temp"} = $pressure[$field{"temp"}]+$K;
                $CURRENT{"dewpt"} = $pressure[$field{"dewpt"}]+$K;
		$CURRENT{"time"}  = $pressure[$field{"time"}];
    #case 1 valid temp,last_temp,dewpt,last_dewpt
                #foreach $elem ("temp","press","dewpt"){ print "last $elem ",$LAST{$elem}," current $elem ",$CURRENT{$elem},"\n";}
		if($CURRENT{"dewpt"} != 999.0 && $LAST{"dewpt"} != 999.0){
		    $LAST{"mixing ratio"} = &calc_q($LAST{"press"},$LAST{"temp"},$LAST{"dewpt"});
		    $CURRENT{"mixing ratio"} = &calc_q($CURRENT{"press"},$CURRENT{"temp"},$CURRENT{"dewpt"});
		    $LAST{"virtual t"} = &calc_virT($LAST{"temp"},$LAST{"mixing ratio"});
		    $CURRENT{"virtual t"} = &calc_virT($CURRENT{"temp"},$CURRENT{"mixing ratio"});
		    $LAST{"density"} = &calc_density($LAST{"press"},$LAST{"virtual t"});
		    $CURRENT{"density"} = &calc_density($CURRENT{"press"},$CURRENT{"virtual t"});
		    $Tbar = &calc_Tbar($LAST{"density"},$CURRENT{"density"},$LAST{"virtual t"},$CURRENT{"virtual t"});
		    $pressure[$field{"Alt"}]=&hydrostatic($CURRENT{"press"},$LAST{"press"},$LAST{"alt"},$Tbar);
		    $pressure[$field{Wcmp}] = &calc_w($pressure[$field{"Alt"}],$LAST{"alt"},$CURRENT{time},$LAST{time});
		    $LAST{"alt"} = $pressure[$field{"Alt"}];
                    foreach $elem("temp","dewpt","press","time"){$LAST{$elem} = $CURRENT{$elem};}
    #case 2 valid temp,last_temp,dewpt
		}elsif($CURRENT{"dewpt"} != 999.0 && $LAST{"dewpt"} == 999.0){
		    $CURRENT{"mixing ratio"} = &calc_q($CURRENT{"press"},$CURRENT{"temp"},$CURRENT{"dewpt"});
		    $CURRENT{"virtual t"} = &calc_virT($CURRENT{"temp"},$CURRENT{"mixing ratio"});
		    $LAST{"density"} = &calc_density($LAST{"press"},$CURRENT{"virtual t"});
		    $CURRENT{"density"} = &calc_density($CURRENT{"press"},$CURRENT{"temp"});
		    $Tbar = &calc_Tbar($LAST{"density"},$CURRENT{"density"},$LAST{"temp"},$CURRENT{"temp"});
                    $pressure[$field{"Alt"}]=&hydrostatic($CURRENT{"press"},$LAST{"press"},$LAST{"alt"},$Tbar);
                    $pressure[$field{Wcmp}] = &calc_w($pressure[$field{"Alt"}],$LAST{"alt"},$CURRENT{time},$LAST{time});
		    $LAST{"alt"} = $pressure[$field{"Alt"}];
                    foreach $elem("temp","dewpt","press","time"){$LAST{$elem} = $CURRENT{$elem};}
    # case 3 vaild temp,last_temp,last_dewpt
		}elsif($pressure[$field{"dewpt"}] == 999.0 && $LAST{"dewpt"} != 999.0){
		    $LAST{"mixing ratio"} = &calc_q($LAST{"press"},$LAST{"temp"},$LAST{"dewpt"});
		    $LAST{"virtual t"} = &calc_virT($LAST{"temp"},$LAST{"mxing ratio"});
		    $LAST{"density"} = &calc_density(PRESS=>$LAST{"press"},VIRT=>$LAST{"virtual t"});
		    $CURRENT{"density"} = &calc_density($CURRENT{"press"},$CURRENT{"temp"});
		    $Tbar = &calc_Tbar($LAST{"density"},$CURRENT{"density"},$LAST{"virtual t"},$CURRENT{"temp"});
                    $pressure[$field{"Alt"}]=&hydrostatic($CURRENT{"press"},$LAST{"press"},$LAST{"alt"},$Tbar);
		    $pressure[$field{Wcmp}] = &calc_w($pressure[$field{"Alt"}],$LAST{"alt"},$CURRENT{time},$LAST{time});
		    $LAST{"alt"} = $pressure[$field{"Alt"}];
                    foreach $elem("temp","dewpt","press","time"){$LAST{$elem} = $CURRENT{$elem};}
    # case 3 vaild temp,last_temp,last_dewpt
		}elsif($pressure[$field{"dewpt"}] == 999.0 && $LAST{"dewpt"} != 999.0){
		    $LAST{"mixing ratio"} = &calc_q($LAST{"press"},$LAST{"temp"},$LAST{"dewpt"});
		    $LAST{"virtual t"} = &calc_virT($LAST{"temp"},$LAST{"mxing ratio"});
		    $LAST{"density"} = &calc_density($LAST{"press"},$LAST{"virtual t"});
		    $CURRENT{"density"} = &calc_density($CURRENT{"press"},$CURRENT{"temp"});
		    $Tbar = &calc_Tbar($LAST{"density"},$CURRENT{"density"},$LAST{"virtual t"},$CURRENT{"temp"});
                    $pressure[$field{"Alt"}]=&hydrostatic($CURRENT{"press"},$LAST{"press"},$LAST{"alt"},$Tbar);
                    $pressure[$field{Wcmp}] = &calc_w($pressure[$field{"Alt"}],$LAST{"alt"},$CURRENT{time},$LAST{time});
		    $LAST{"alt"} = $pressure[$field{"Alt"}];
                    foreach $elem("temp","dewpt","press","time"){$LAST{$elem} = $CURRENT{$elem};}
    # case 4 vaild temp,last_temp
		}else{
                    $LAST{"density"} = &calc_density($LAST{"press"},$LAST{"temp"});
		    $CURRENT{"density"} = &calc_density($CURRENT{"press"},$CURRENT{"temp"});
	            $Tbar = &calc_Tbar($LAST{"density"},$CURRENT{"density"},$LAST{"temp"},$CURRENT{"temp"});
		    $pressure[$field{"Alt"}]=&hydrostatic($CURRENT{"press"},$LAST{"press"},$LAST{"alt"},$Tbar);
                    $pressure[$field{Wcmp}] = &calc_w($pressure[$field{"Alt"}],$LAST{"alt"},$CURRENT{time},$LAST{time});
		    $LAST{"alt"} = $pressure[$field{"Alt"}];
                    foreach $elem("temp","dewpt","press","time"){$LAST{$elem} = $CURRENT{$elem};}
		}# end if-elsif(2)-else
                $LAST{"alt"} = $pressure[$field{"Alt"}];
                foreach $elem("temp","dewpt","press","time"){$LAST{$elem} = $CURRENT{$elem};}
            }# end if
            #push(@ALTFILE,&line_printer(@pressure));
	}# end if-else
        push(@ALTFILE,&line_printer(@pressure));
    }# end foreach $line
    return @ALTFILE;
} # end sub calc_alt
##############################################################################
# function hydrostatic usage "altitude" = &hydrostatic(altitude,average T,last press,current press)
# function calculates altitude using hydrostatic equation
##############################################################################
sub hydrostatic{
    my ($p1,$p0,$alt,$tbar) = @_;
    my ($G,$R) = ($CONSTANTS{"G"},$CONSTANTS{"Rdry"});
    return $alt+((($R*$tbar)/9.8)*(log($p0/$p1)));
}# end sub hydrostatic
##############################################################################
# function hydrostatic_press 
# usage "pressure" = &hydrostatic(pressure,average T,last alt,current alt)
# function calculates pressure using hydrostatic equation
##############################################################################
sub hydrostatic_press{
    my ($h1,$h0,$press,$tbar) = @_;
    my ($G,$R) = ($CONSTANTS{"G"},$CONSTANTS{"Rdry"});
    return $press * exp((9.8/($R*$tbar))*($h0-$h1))
}# end sub hydrostatic_press
##############################################################################
# function calq_q usage "mixing ratio" = &calc_q(press,temp,dewpt)
# function calculates mixing ratio using pressure, temperature, and dew point
############################################################################## 
sub calc_q{
    my ($K,$Rv,$ESO,$EPS) = ($CONSTANTS{"K"},$CONSTANTS{"Rv"},$CONSTANTS{"ESO"},$CONSTANTS{"EPS"});
    my ($L,$X,$expo,$es);
    my($press,$temp,$dewpt) = @_;
    if($temp){
	$L = 2500000 - 2369 * ($temp-$K);
	$X = (1/$K) - (1/$temp);
	$expo = ($L/$Rv)*$X;
	$es = ($ESO*100.0)*exp($expo);
	return (($es*$EPS)/($press*100.0)*(1-($es/($press*100.0))));
    }else{
	return $CONSTANTS{"FALSE"};
    }# end if
}# end sub calc_q
#############################################################################
# function calc_Tbar usage "average Temp" = &calc_Tbar(density1,density2,
# temperature1,temperature2) function calculates density weighted average 
# temperature within a layer
#############################################################################
sub calc_Tbar{
    my($d1,$d2,$t1,$t2) = @_;
    if($d1+$d2){
	return ((($d1*$t1)+($d2*$t2))/($d1+$d2));
    }else{
	return $CONSTANTS{"FALSE"};
    }# end if
}# end sub calc_Tbar
###############################################################################
# function calc_density usage density = &calc_density(press,temp)
# function calculates density using the ideal gas law and pressure and 
# temperature
###############################################################################
sub calc_density{
    my($press,$temp) = @_;
    if($temp){
	return (($press*100)/($CONSTANTS{"Rdry"}*$temp));
    }else{
	return $CONSTANTS{"FALSE"};
    }# end if
}# end sub calc_density
###############################################################################
# function calc_virT usage "virtual temp" = &calc_virT(temp,"mixing ratio")
# function calculates virtual temperature using temperature and mixing ratio
###############################################################################
sub calc_virT{
    my($temp,$mix_rat) = @_;
    return ($temp*(1 + (.61 * $mix_rat)));
}# end calc_virT
###############################################################################
# subroutine windqc usage @OUTFILE = &windqc(@OUTFILE)
# subroutine uses a running mean and variance centered on $CONSTANT{"INTERVAL"}
# seconds to flaq wind speed questionable or bad based on standard deviations
###############################################################################
sub windqc{
    my ($time,@OUTFILE,%TIME,@times,@input,$begin,$end,$LIMIT,$interval,$line,$wind_shear);
    my ($increment,$N,$delta,$FACTOR);my($value,$num,$Z,$press,$denum,$qcflag);my $string;
    my ($BAD,$QUEST,$RATIO) = ($WINDQC{"Bad"},$WINDQC{"Questionable"},$WINDQC{"Ratio"});
    my $miss_count = 0;my ($FLAG,$qc);
    my ($array_ref,$file_ref,$file_ref2) = @_;my @ARRAY = @{$array_ref};my $fh = ${$file_ref};
    my $sh = ${$file_ref2};
    my($TRUE,$FALSE,$CALCVAR,$NOCHECK) = ($CONSTANTS{"TRUE"},$CONSTANTS{"FALSE"},$CONSTANTS{"FALSE"},$CONSTANTS{"FALSE"});
    my (%QC,%VAR,%MEAN,%MISS,%MISS_COUNT,%K,$elem,$qcval,$diff,$index);
    $MISS{"Spd"} = 999.0;$MISS{"Vcmp"} = 9999.0;$MISS{"Ucmp"} = 9999.0;
    #$K{"Spd"} = 0.95;$K{"Ucmp"} = 1.0;$K{"Vcmp"} = 1.0;
    #$K{"Spd"} = 0.85;$K{"Ucmp"} = 1.0;$K{"Vcmp"} = 1.0;
    #$K{"Spd"} = 0.85;$K{"Ucmp"} = 0.85;$K{"Vcmp"} = 1.0;
    #$K{"Spd"} = 0.85;$K{"Ucmp"} = 0.85;$K{"Vcmp"} = 1.00;
    #$K{"Spd"} = 0.85;$K{"Ucmp"} = 0.85;$K{"Vcmp"} = 0.85;
    $K{"Spd"} = 0.80;$K{"Ucmp"} = 0.80;$K{"Vcmp"} = 0.85;
    $QC{2.0} = "QUEST";
    $QC{3.0} = "BAD";
    printf $fh ("%s %6.1f %s\n","Avg. period is:",$WINDQC{INTERVAL}," secs");
    foreach $line(@ARRAY){
	@input = split(' ',$line);
	$TIME{$input[$field{"time"}]} = &line_printer(@input);
    }# end foreach $line
    @times = sort{$a <=> $b} keys (%TIME);
    $LIMIT = scalar(@times)-1;
    for ($i = 1;$i <= $LIMIT;$i++){
        @current = split(' ',$TIME{$times[$i]});
        if($current[$field{"time"}] != 9999.0){
	    $qc = "NULL";
	    foreach $elem ("Spd","Ucmp","Vcmp"){
		if($current[$field{$elem}] != $MISS{$elem} && $qc eq "NULL"){
		    $diff = 0;$index = $i-1;
		    @previous = split(' ',$TIME{$times[$index]});
		    while((($previous[$field{"time"}] == 9999.0 || $previous[$field{$elem}] == $MISS{$elem}) || ($previous[$field{"Qu"}] == 3.0 || $previous[$field{"Qu"}] == 2.0)) && ($index > 0 && $index < $i)){
			$index--;
			@previous = split(' ',$TIME{$times[$index]});
		    }# end while loop 
		    if(($previous[$field{"time"}] != 9999.0 && $previous[$field{$elem}] != $MISS{$elem}) && ($previous[$field{"Qu"}] != 3.0 || $previous[$field{"Qu"}] != 2.0)){
			$diff = abs($current[$field{"time"}]-$previous[$field{"time"}]);
			if($diff <= 3*$WINDQC{"INTERVAL"} && $diff){
			    $wind_shear = abs($current[$field{$elem}]-$previous[$field{$elem}])/$diff;
			    if($wind_shear >= $SHEAR{"Questionable"} && $wind_shear > 0){
				$qc = 2.0;# Questionable flag
				if($wind_shear >= $SHEAR{"Bad"}){
				    $qc = 3.0;# Bad flag
				}# end if
				foreach $elem ("Qu","Qv"){$current[$field{$elem}] = $qc;}
				
				$TIME{$times[$i]} = &line_printer(@current);
			        printf $fh ("%4s %s %6.1f %s %6.1f %s %7.3f %s %3.1f\n",$elem,"Windshear between pressures:",$previous[$field{"press"}]," mb and ",$current[$field{"press"}]," mb is:",$wind_shear,"m/s/s QC FLAG=",$qc);
			    }# end if
			    printf $sh ("%4s %s %6.1f %s %6.1f %s %7.3f %s %4s\n",$elem,"Windshear between pressures:",$previous[$field{"press"}]," mb and ",$current[$field{"press"}]," mb is:",$wind_shear,"m/s/s QC FLAG=",$qc);
		       }# end if
		    }# end if

	         }# end if loop
            }# end for each $elem loop
        }# end if
    }# end for loop
    $num = int(rand $LIMIT);@times = sort{$a <=> $b} keys (%TIME);
    if(defined($times[$num-1]) && defined($times[$num])){
	$increment = abs($times[$num-1]-$times[$num]);
	if($increment){
	    $interval = int($WINDQC{"INTERVAL"}/$increment);
        }else{
	    $interval = 0;
        }
    }
    if($interval){
	for ($i = 0;$i <= $LIMIT;$i++){
	    $CALCVAR = $FALSE;
	    if($i+int($interval/2) <= $LIMIT && $i-int($interval/2) > 0){
		$begin = $i - int($interval/2);
		$end = $i + int($interval/2);
		$CALCVAR = $TRUE;
	    }elsif($i+int($interval/4) <= $LIMIT && $i-int((3/4)*$interval) > 0){
		$begin = $i - int((3/4)*$interval);
		$end = $i + int($interval/4);
		$CALCVAR = $TRUE;
	    }elsif($i+int((3/4)*$interval) <= $LIMIT && $i-int($interval/4) > 0){
		$begin = $i - int($interval/4);
		$end = $i + int((3/4)*$interval);
		$CALCVAR = $TRUE;
	    }# end if-elsif(2)
	    if($CALCVAR){
		foreach $elem("Spd","Ucmp","Vcmp"){
		    $VAR{$elem} = 0;$MEAN{$elem} = 0;$MISS_COUNT{$elem}=0;$Z=0;
		    for($j = $begin;$j <= $end;$j++){
			@input = split(' ',$TIME{$times[$j]});
			$value = $input[$field{$elem}];
                        $qcflag = $input[$field{"Qu"}];
			if($value == $MISS{$elem} || $qcflag == 3.0 || $qcflag == 2.0){$MISS_COUNT{$elem}++;}
		    }# end for loop
		    for($j = $begin;$j <= $end;$j++){
			@input = split(' ',$TIME{$times[$j]});
			$value = $input[$field{$elem}];
                        $qcflag = $input[$field{"Qu"}];
			$denum = ($interval+1)-$MISS_COUNT{$elem};
			unless ($value == $MISS{$elem} || $qcflag == 3.0 || $qcflag == 2.0){
			    $delta = ($value-$MEAN{$elem})/($denum);
			    $MEAN{$elem}+=$delta;
			    $VAR{$elem}+=(($denum-1)*$delta*$delta)-($VAR{$elem}/($denum));
			}# end unless
		    }# end for loop
		    @input = split(' ',$TIME{$times[$i]});
		    $value = $input[$field{$elem}];
		    $press = $input[$field{"press"}];
		    $FLAG = 99.0;#Unchecked QC flag
		    unless ($value == $MISS{$elem} || $input[$field{"Spd"}] <= 5.0){
			if($denum/$interval >= $RATIO){
			    if($VAR{$elem} > 0){$Z = &calc_Z(\$value,\$MEAN{$elem},\$VAR{$elem});}
			    if($elem eq "Spd" || $elem eq "Ucmp"){
			    if($Z >= $QUEST*$K{$elem}){
				$FLAG = 2.0;# Questionable QC flag
				if($Z >= $BAD*$K{$elem}){$FLAG = 3.0;}# Bad QC flag
			    }# end if
                            }else{
			    if(abs($Z) >= $QUEST*$K{$elem}){
			    
				$FLAG = 2.0;# Questionable QC flag
				if(abs($Z) >= $BAD*$K{$elem}){$FLAG = 3.0;}# Bad QC flag
				
			    }# end if
			    }# end if
			}elsif($denum/$interval >= 0.5){
			    if($VAR{$elem} > 0){$Z = &calc_Z(\$value,\$MEAN{$elem},\$VAR{$elem});}
			    if($elem eq "Spd" || $elem eq "Ucmp"){
			    if($Z >= $QUEST*$K{$elem}){
				$FLAG = 2.0;# Questionable QC flag
				
				if($Z >= $BAD*$K{$elem}){$FLAG = 3.0;}# Bad QC flag
			    }# end if
                            }else{
			    if(abs($Z) >= $QUEST*$K{$elem}){
			    
				$FLAG = 2.0;# Questionable QC flag
				if(abs($Z) >= $BAD*$K{$elem}){$FLAG = 3.0;}# Bad QC flag
				
			    }# end if
                            }# end if
			}# end if
		    }# end unless
		    if($FLAG == 3.0){
			#unless($input[$field{"Qu"}] == 3.0 && $input[$field{"Qv"}] == 3.0){
			    if($elem eq "Spd"){
				printf $fh ("%s %6.1f %s %6.1f %s %10.4f %s\n","Windspd at press:",$press,"is:",$value,"m/s Z=",$Z,"BAD");
			    }else{
				printf $fh ("%s %6.1f %s %6.1f %s %10.4f %s\n","   $elem at press:",$press,"is:",$value,"m/s Z=",$Z,"BAD");
			    }# end if
                            unless($input[$field{"Qu"}] == 3.0 && $input[$field{"Qv"}] == 3.0){
				foreach $qcval("Qu","Qv"){$input[$field{$qcval}] = 3.0;}
				$TIME{$times[$i]} = &line_printer(@input);
                            }
			#}# end unless
		    }elsif($FLAG == 2.0){
			#unless(($input[$field{"Qu"}] == 3.0 && $input[$field{"Qv"}] == 3.0)||($input[$field{"Qu"}] == #2.0 && $input[$fie#ld{"Qv"}] == 2.0)){
			    if($elem eq "Spd"){
				printf $fh ("%s %6.1f %s %6.1f %s %10.4f %s\n","Windspd at press:",$press,"is:",$value,"m/s Z=",$Z,"QUEST");
			    }else{
				printf $fh ("%s %6.1f %s %6.1f %s %10.4f %s\n","   $elem at press:",$press,"is:",$value,"m/s Z=",$Z,"QUEST");
			    }# end if
                            unless(($input[$field{"Qu"}] == 3.0 && $input[$field{"Qv"}] == 3.0)||($input[$field{"Qu"}] == 2.0 && $input[$field{"Qv"}] == 2.0)){
				foreach $qcval("Qu","Qv"){$input[$field{$qcval}] = 2.0;}
			        $TIME{$times[$i]} = &line_printer(@input);
                            }
			#}# end unless
		    }# end if-elsif
		    if($elem eq "Spd"){
			$string = "Windspd";
                    }else{
			$string = "$elem   ";
                    }
                    #if(undef $MEAN{$elem} && undef $VAR{$elem}){$MEAN{$elem} = 0;$VAR{$elem}=0;}
                    printf $sh ("%s %6.1f %s %6.1f %s %10.4f %s %6.1f %s %10.4f \n","$string at press:",$press,"is:",$value,"m/s Z=",$Z,"Mean=",$MEAN{$elem},"m/s Var=",$VAR{$elem})
	       }#end foreach $elem loop
               
	    }# end if $CALCVAR loop
	    #push(@OUTFILE,$TIME{$times[$i]});
	}# end for loop
        for ($i = 0;$i <= $LIMIT;$i++){
	    @input = split(' ',$TIME{$times[$i]});
            $NOCHECK = $FALSE;
	    if(($input[$field{Qu}] == 2.0 || $input[$field{Qu}] == 3.0) && ($input[$field{Qv}] == 2.0 || $input[$field{Qv}] == 3.0)){
		$NOCHECK = $TRUE;
            }# end if  
	    foreach $elem("Spd","Ucmp","Vcmp"){
		$value = $input[$field{$elem}];
		$press = $input[$field{press}];
		$time = $input[$field{time}];
		unless($value==$MISS{$elem} || $input[$field{"Spd"}] <= 5.0 || $NOCHECK){
		    if($i+1 <= $LIMIT && $i-1 >= 0){
			#print &line_printer(@input);
			@after = split(' ',$TIME{$times[$i+1]});
                        @before = split(' ',$TIME{$times[$i-1]});
			unless($after[$field{"Qu"}] == 99.0 || $before[$field{"Qu"}] == 99.0){
			if($before[$field{"Qu"}]>=2.0&&$after[$field{"Qu"}]>=2.0){
			    $FLAG = 2.0;# Questionable QC flag
			    if($before[$field{"Qu"}]  == 3.0 && $after[$field{"Qu"}] == 3.0){
				$FLAG = 3.0;# Bad QC flag
			     }# end if
			     foreach $qcval("Qu","Qv"){
				 $input[$field{$qcval}] = $FLAG;
                             }# end foreach
			     if($elem eq "Spd"){
				 printf $fh ("%s %6.1f %s %6.1f %s %s\n","Windspd at press:",$press,"is:",$value,"m/s",$QC{$FLAG});
			     }else{
				 printf $fh ("%s %6.1f %s %6.1f %s %s\n","   $elem at press:",$press,"is:",$value,"m/s",$QC{$FLAG});
			     }# end if
			     $TIME{$times[$i]} = &line_printer(@input);
                        }# end if
                        }# end unless     
		    }# end if
		}# end unless
            }# end foreach
            push(@OUTFILE,$TIME{$times[$i]});
        }# end for loop
        return @OUTFILE;
    }else{
	printf $fh ("%s %5d %s\n","File contains only",$LIMIT,"datalines");
        return @ARRAY;
    }# end if
}# end sub windqc
#############################################################################
# Function calc_Z
# function calulates standarized anomoly using sample mean and variance
#############################################################################
sub calc_Z{
    my($X,$mean,$var) = @_;
    return ($$X-$$mean)/sqrt($$var);
}#end sub calc_Z
#############################################################################
# subroutine remove_dropping  usage @OUTFILE = &remove_dropping(@OUTFILE)
# subroutine removes dropping observations from end of upsonde sounding
#############################################################################
sub remove_dropping{
    my @tmp = @_;my @input;my $max_alt = 0;
    my $count = scalar(@_)-1;
    foreach $line(@_){
	@input = split(' ',$line);
	unless($input[$field{"Alt"}] == $MISS{"Alt"}){
	    if($input[$field{"Alt"}] >= $max_alt){
		$max_alt = $input[$field{"Alt"}];
	    }# end if
	}# end unless
    }# endif
    @input = split(' ',$_[$count]);
    while($input[$field{"Alt"}] < $max_alt || $input[$field{"Alt"}] == $MISS{"Alt"}){
	pop @tmp;$count--;
        @input = split(' ',$_[$count]);
    }# end while loop
    return @tmp
}# end sub remove_dropping
#############################################################################
# End of Module
#############################################################################

1;











