#!/bin/perl -w
# program pibal2class.pl
# converts pibal sounding file to wind only class format file
# 
use Time::Local;
# Constants use in calc_UV
$pi = 3.14159265;$radian = 180.0/$pi;
#$K = 273.15;$R = 287.04;$Rv = 461.5;$ESO = 6.1121;$EPS = .622;$G = 9.80665;
# define LOGICAL variables 
$TRUE = 1;$FALSE = 0;
# setting directory variable
$main_dir = "/work/PACS/upper_air/pibal";
# setting name of logfile

$OUTDIR = "../output/";
$LOGDIR = "../logs/";
mkdir($OUTDIR) unless(-e $OUTDIR);
mkdir($LOGDIR) unless(-e $LOGDIR);
$logfile = sprintf("%sPIBAL.log",$LOGDIR);

# CLass FIle HEADER
$data_type = "profile";
$project_id = "PACS pibal";
$header[0] = "Data Type:                         ";
$header[1] = "Project ID:                        ";
$header[2] = "Release Site Type/Site ID:         ";
$header[3] = "Release Location (lon,lat,alt):    ";
$header[4] = "UTC Release Time (y,m,d,h,m,s):    ";
$header[5] = "Ascension No:                      ";
$header[6] = "Nominal Release Time (y,m,d,h,m,s):";
$header[7] = "System Operator/Comments:          ";
$header[8] = "Additional comments:               ";
$header[9] = "Input file:                        ";
$line[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$line[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code";
$line[2] ="------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----";
%FIELDS = (
	  "t[seg]   z[m]     U[m/s]   V[m/s]  mag[m/s]  dir[grados]" => {
	      TIME => "0",
              Ucmp => "2",
              Vcmp => "3",
              Spd  => "4",
              Dir  => "5",
              Alt  => "1",
          },
	  "t[s]   zt[m]   zm[m]    x[m]    y[m]   dir[o] mag[m/s] U[m/s]  V[m/s]" => {
	      TIME => "0",
	      Ucmp => "7",
	      Vcmp => "8",
	      Spd  => "6",
	      Dir  => "5",
	      Alt  => "2",
	  },
	   "t[s]     zt[m]     zm[m]      x[m]      y[m]    dir[o]  mag[m/s]    U[m/s]    V[m/s]" => {
	      TIME => "0",
	      Ucmp => "7",
	      Vcmp => "8",
	      Spd  => "6",
	      Dir  => "5",
	      Alt  => "2",
	   },
           "t[s]	zt[m]	zm[m]	x[m]	y[m]	dir[o]	mag[m/s] U[m/s]V[m/s]" => {
	      TIME => "0",
	      Ucmp => "7",
	      Vcmp => "8",
	      Spd  => "6",
	      Dir  => "5",
	      Alt  => "2",
	  },
		   "t[s]    zt(m)   zm(m)   x[m]    y[m]    dir[o] mag[m/s] U[m/s]  V[m/s]" => {
		  TIME => "0",
		  Ucmp => "7",
		  Vcmp => "8",
		  Spd  => "6",
		  Dir  => "5",
		  Alt  => "2",
	  }, 
		   "t[s]    zt(m)   zm(m)   x[m]    y[m]    dir[o]  mag[m/s] U[m/s]  V[m/s]" => {
		  TIME => "0",
          Ucmp => "7",
          Vcmp => "8",
          Spd  => "6",
          Dir  => "5",
          Alt  => "2",
	  },
           "t[s]    zt(m)   zm(m)   x[m]    y[m]    dir[o]  mag[m/s]U[m/s]  V[m/s]" => {
          TIME => "0",
          Ucmp => "7",
          Vcmp => "8",
          Spd  => "6",
          Dir  => "5",
          Alt  => "2",

	  },
	   " t[s]     zt[m]     zm[m]      x[m]      y[m]    dir[o]  mag[m/s]    U[m/s]    V[m/s]" => {
	  TIME => "0",
	  Ucmp => "7",
	  Vcmp => "8",
	  Spd  => "6",
	  Dir  => "5",
	  Alt  => "2",
          },
          );
%station_name = (
		 Mexico => {"01"=>"Salina Cruz","02"=>"Puerto Madero",
                    "03"=>"Frontera","55"=>"Puerto Penasco", 
		    "56"=>"Topolobampo","57"=>"Tampico","58"=>"Cd del Carmen"},
		 Nicaragua => {"04"=>"Managua"},
		 "Costa Rica"=> {"05"=>"Liberia","06"=>"Isla del Coco"},
		 Panama => {"07"=>"Los Santos"},
		 Columbia => {"08"=>"Cartagena"},
		 Ecuador => {"00"=>"PortoViejo","10"=>"Esmeraldas",
			     "11"=>"Guayaquil","12"=>"San Cristobal",
			     "18"=>"Ancon"},
		 Peru => {"13"=>"Piura","14"=>"Tumbes","15"=>"Trujillo",
			  "16"=>"Ancon2","17"=>"Iquitos","21"=>"Chiclayo"},
		 Bolivia => {"20"=>"Santa Cruz","70"=>"Cobija",
			     "71"=>"Trinidad",
			     "72"=>"La Paz",
			     "73"=>"Robore","74"=>"Uyuni"},
		 Paraguay => {"30"=>"Asuncion","32"=>"Estigarribia"},
		 );
%station_info = (
	    "Salina Cruz" => {
		ID => "SALI",
		SITE => "SALI Salina Cruz, Mexico",
		LAT  => "16.16",
		LON  => "-95.18",
		ALT  => "32.0",
        ALIAS => "Salina Crux",
	    },
            "Puerto Madero" => {
		ID => "PUMD",
		SITE => "PUMD Puerto Madero, Mexico",
		LAT  => "14.7",
		LON  => "-92.4",
		ALT  => "3.0", 
            },
            "Frontera" => {
		ID => "FRTA",
		SITE => "FRTA Frontera, Mexico",
		LAT  => "18.52",
		LON  => "-92.65",
		ALT  => "4.0",
                ALIAS => "Fronte",
	    },
		"Puerto Penasco" => {
		ID => "PUPN",
		SITE => "PUPN Puerto Penasco, Mexico",
		LAT => "31.31",
		LON => "-113.55",
		ALT => "3.0",
		},
		"Topolobampo" => {
		ID => "TPLB",
		SITE => "TPLB Topolobampo, Mexico",
		LAT => "25.59",
		LON => "-109.06",
		ALT => "12.0",
		},
		"Tampico" => {
		ID => "TMPC",
		SITE => "TMPC Tampico, Mexico",
		LAT => "22.27",
		LON => "-97.78",
		ALT => "15.0",
		},
		"Cd del Carmen" => {
		ID => "CDCM",
		SITE => "CDCM Cd del Carmen, Mexico",
		LAT => "18.63",
		LON => "-91.83",
		ALT => "2.0",
		},
	    "Managua" => {
		ID => "MNGA",
		SITE => "MNGA Managua, Nicaragua",
		LAT  => "12.13",
		LON  => "-86.16",
		ALT  => "56.0",
            },
	    "Liberia" => {
		ID => "LBRA",
		SITE => "LBRA Liberia, Costa Rica",
		LAT  => "10.6",
		LON  => "-85.53",
		ALT  => "70.0", # inserted 10/1/99
		ALIAS => "Lberia",
            },
            "Isla del Coco" => {
		ID => "COCO",
		SITE => "COCO Isla del Coco, Costa Rica",
		LAT  => "5.54",
		LON  => "-87.05",
		ALT  => "99999.0", # unknown
            },
	    "Los Santos" => {
		ID => "LOST",
		SITE =>"LOST Los Santos, Panama",
		LAT  => "7.95",
		LON  => "-80.42",
		ALT  => "99999.0", # unknown
		ALIAS => "David",
	    },
	    "David" => {
		ID => "DAVD",
		SITE => "DAVD David, Panama",
		LAT  => "8.93",
		LON  => "-82.43",
		ALT  => "27.0",
	    },
	    "Cartagena" => {
		ID => "CART",
		SITE => "CART Cartagena, Colombia",
		LAT  => "10.43",
		LON  => "-75.57",
		ALT  => "3.0", # inserted 9/28/99
		ALIAS => "cartagen",
            },
            "Ancon" => {
		ID => "ANCO",
		SITE => "ANCO Ancon, Ecuador",
		LAT  => "999.0", # unknown lat/lon and altitude
		LON  => "9999.0",
		ALT  => "99999.0",
		ALIAS => "Ancon1",
	    },
	    "Esmeraldas" => {
		ID => "EMAS",
                SITE => "EMAS Esmeraldas, Ecuador",
		LAT  => "1.0",
		LON  => "-79.63",
		ALT  => "80.0",
		ALIAS => "Esmeralda",
            },
	    "Guayaquil"  => {
		ID => "GUAY",
		SITE => "GUAY Guayaquil, Ecuador",
		LAT  => "-2.14",
		LON  => "-79.96",
		ALT  => "97.0",
                ALIAS => "Espol",
            },
            "PortoViejo"  => {
		ID => "PORT",
		SITE => "PORT PortoViejo, Ecuador",
		LAT  => "999.0", # missing lat/lon and altitude
		LON  => "9999.0",
		ALT  => "99999.0",
            },
	    "San Cristobal" => {
		ID => "SNCR",
		SITE => "SNCR San Cristobal, Ecuador",
		LAT  => "-0.9",
		LON  => "-87.62",
		ALT  => "8.0", #inserted 10/1
		ALIAS => "Galapagos",
            },
	    "Piura" => {
		ID => "PIUA",
		SITE => "PIUA Piura, Peru",
		LAT  => "-5.2",
		LON  => "-80.63",
		ALT  => "40.0",
            },
	    "Tumbes" => {
		ID => "TUMB",
		SITE => "TUMB Tumbles, Peru",
		LAT  => "-3.57",
		LON  => "-80.45",
		ALT  => "7.0",
            },
	    "Trujillo" => {
		ID => "TRUJ",
		SITE => "TRUJ Trujillo, Peru",
		LAT  => "-8.12",
		LON  => "-80.45",
		ALT  => "34.0",
            },
	    "Ancon2" => {
		ID => "ANCN",
		SITE => "ANCO Ancon2, Peru",
		LAT  => "-11.77",
		LON  => "-77.17",
		ALT  => "30.0",
            },
	    "Chiclayo" => {
		ID => "CHYO",
		SITE => "CHYO Chiclayo, Peru",
		LAT  => "-6.77",
		LON  => "-79.85",
		ALT  => "29.0",
            },
	    "Iquitos" => {
		ID => "IQTS",
		SITE => "IQTS Iquitos, Peru",
		LAT  => "-3.75",
		LON  => "-73.25",
		ALT  => "104.0",
            },
	    "Santa Cruz" => {
		ID => "SACZ",
		SITE => "SACZ Santa Cruz, Bolivia",
		#LAT  => "-17.75",
		LAT => "-17.76", #1998
		#LAT => "-17.66",  #for 1999 and later
		#LON  => "-73.15",
		LON => "-63.15", #1998
		#LON => "-63.12",  #for 1999 and later
		ALT  => "410.0", #1998
		#ALT => "373.0", #1999 and later 
	    },
	    "Cobija" => {
		ID => "CBJA",
		SITE => "CBJA Cobija, Bolivia",
		LAT => "-11.04",
		LON => "-68.78",
		ALT => "271.0",
            },
		"Trinidad" => {
		ID => "TRND",
		SITE => "TRND Trinidad, Bolivia",
		LAT => "-14.82",
		LON => "-64.92",
		ALT => "156.0",
		},
		"La Paz" => {
		ID => "LAPZ",
		SITE => "LAPZ La Paz, Bolivia",
		LAT => "-16.51",
		LON => "-68,20",
		ALT => "4024.0",
		},
		"Robore" => {
		ID => "RBRE",
		SITE => "RBRE Robore, Bolivia",
		LAT => "-18.33",
		LON => "-59.76",
		ALT => "277.0",
		},
		"Uyuni" => {
		ID => "UYNI",
		SITE => "UYNI Uyuni, Bolivia",
		LAT => "-20.46",
		LON => "-66.83",
		ALT => "3669.0",
		},
		"Asuncion" => {
		ID => "ASNC",
		SITE => "ASNC Asuncion, Paraguay",
		LAT => "-25.24",
		LON => "-57.52",
		ALT => "83.0",
		},
		"Estigarribia" => {
		ID => "ESTG",
		SITE => "ESTG Estigarribia, Paraguay",
		LAT => "-22.05",
		LON => "-60.63",
		ALT => "155.0",
		},
	    );
%MONTHS = (
	   january => {
	       FILE => "1",
	       MM   => "01",
	       DAYS => "31",
	       SPANISH => "enero",
	       ABREV   => "feb",
           NEXT    => "february",
		   MISSPELL => "ener",
	   },
	   february => {
	       FILE => "2",
	       MM   => "02",
	       DAYS => "28",
	       SPANISH => "febrero",
	       ABREV   => "feb",
	       NEXT    => "march",
               MISSPELL =>"febrer0",
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
               MISSPELL =>"unio",
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
	       MISSPELL => "dicembre",
	   },
	   );
if(@ARGV < 1){
    print "Usage is pibal2class.pl file(s)\n";
    exit;
}# end if
foreach $file(@ARGV){
#$file = shift(@ARGV);
$code = substr($file,13,2);

# $file_dir = "$dir/class_files";
$site = "NULL";
$year_from_dir = "2000";
@arrays = keys %FIELDS;
foreach $country (keys %station_name){
    if(grep(/$code/,keys %{$station_name{$country}})){
	$site = $station_name{$country}{$code};
    }# end if
}# end foreach $country
@months = keys %MONTHS;
foreach $mth (@months){
    push(@spanish,$MONTHS{$mth}{"SPANISH"});
}# end mths
open(LOG,">$logfile") || die "Can't open $logfile\n";
#&timestamp;
print "processing file $file\n";print LOG "processing file $file ";
# OPENING input file, check to see if input file has just one line of data, 
# if so, split lines on carriage returns. 
open(FILE,"$file") || die "Can't open $file\n";
$line_cnt = 0;
while (defined($line = <FILE>)){
    $line_cnt++;
}
close (FILE);
if ($line_cnt == 1) {    
    open(OUTFILE,">$file.out") || die "Can't open $file.out\n";    
    open(FILE,"$file") || die "Can't open $file\n";
    $line = <FILE>; 
    @array = split (/\r/, $line);
    $length = scalar(@array);
    print LOG "new line count = $length ";
       foreach $ele (@array) {
           print OUTFILE "$ele\n";
       }  
    close (OUTFILE);
    close (FILE); 
    rename ("$file.out", "$file") || die "can't rename $file.out";
} 
open(FILE,"$file") || die "Can't open $file\n";
$line_cnt = 0;$FILE_OPEN = $FALSE;$i = 0;$ALIAS = $FALSE;
while (defined($line = <FILE>)){
    @name = split(' ',$site);
    if(!$FILE_OPEN){
	if($line =~ /$site /i && !$FILE_OPEN){
	    chop($line);
	    #print "Calling filename 1 $line\n";
            &filename($line);
	}# end if
	if(scalar(@name) == 2 && !$FILE_OPEN){
	    if($line =~ /$name[0]/i && $line =~ /$name[1]/i){
		chop($line);#print "Calling filename 2 $line\n";
		&filename($line);
	    }# end if
	}# end if
        if($i == 0 && $site ne "NULL" && !$FILE_OPEN){
	    foreach $stn (keys %station_info){
		if($line =~ /$stn/i){
		    $site = $stn;
		}# end if
		if(defined($station_info{$stn}{"ALIAS"})){
		    if($line =~ /$station_info{$stn}{"ALIAS"}/i){
			$alias_site = $station_info{$stn}{"ALIAS"};
			if(grep(/^$alias_site$/,keys %station_info)){
			    $site = $alias_site;
			}# end if
			$ALIAS = $TRUE; 
		    }# end if
		}# end if
	    }# end foreach $stn
	    chop($line);#print "Calling filename 3 $line\n";
	    &filename($line);
	}# end if
	if(defined($station_info{$site}{"ALIAS"}) && !$FILE_OPEN){
	    if($line =~ /$station_info{$site}{"ALIAS"}/i){
		chop($line);#print $line;
		$alias_site = $station_info{$site}{"ALIAS"};
		$line =~ s/$alias_site/ /ig;
		if(grep(/^$alias_site$/,keys %station_info)){
		    $site=$alias_site;
                }
		$ALIAS = $TRUE;#print "Calling filename 4 $line\n";
		&filename($line); 
	    }# end if
	 }# end if
    }else{
	if($line =~ /[A-Za-z]/ && $line !~ /[0-9]/){
	    #print "Calling findarray \n";
	    chop($line);&findarray($line);
	}# end if
	if($line =~ /[0-9]/ && $line !~ /[A-Za-z]/){
	    #print "GODDAMN if this isn't data\n";
	    if($WRITE && $FILE_OPEN){
		#print "Calling writedata\n";
		&writedata($line);
	    }# end if
	}# end if
    }# end if-else
    $i++;
}# end while loop
close(FILE);
if(!$FILE_OPEN){
    print LOG " outfile not opened ";
    #$cmd = "mv -f $dir/$file $main_dir/not_proc_$year_from_dir";system($cmd);
}else{
    printf LOG ("%s %3d %s","contains",$line_cnt,"lines of data ");
    printf ("%s %3d %s\n","contains",$line_cnt,"lines of data\n");
    #$cmd = "mv -f $dir/$file $main_dir/proc_$year_from_dir";system($cmd);
    #print LOG "$cmd\n";
}
print LOG "\n";#&timestamp;
}# end foreach $file
close(LOG);

sub by_number {$a <=> $b;}

sub init_line{
    for $i(0,1,5,6,10){$_[$i] = 9999.0;}
    for $i(2,3,4,7,8,9,11,12,13){$_[$i] = 999.0;}
    for $i(15..20){$_[$i] = 9.0;}
    $_[14] = 99999.0;
    return @_;
}# end sub init_line
    
sub printer{
    printf CLS ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]);
}# end sub printer
 
sub line_printer{
     $outline = sprintf ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]);
     return $outline;
}# end sub line_printer

sub calc_UV{
    local ($spd,$dir) = @_;
    $mult = 1/$radian;
    if($spd < 999.0 && $dir <= 360.0 && $spd >= 0 && $dir >= 0.0){
      $outline[5] = sin(($dir+180.0)*$mult)*$spd;# U wind component
      $outline[6] = cos(($dir+180.0)*$mult)*$spd;# V wind component
      $outline[7] = $spd;$outline[8] = $dir;
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
    $lat_dir = "'N";if($lat < 0){$lat_dir = "'S";}
    @pos_list = (abs(int($lon)),$lon_min,$lon_dir,abs(int($lat)),$lat_min,$lat_dir,$lon,$lat);
    return @pos_list;
}# end sub calc_pos

sub writeheader{
   printf CLS ("%s%s\n",$header[0],$data_type);
   printf CLS ("%s%s\n",$header[1],$project_id);
   printf CLS ("%s%s\n",$header[2],$station_info{$site}{"SITE"});
   $lat = $station_info{$site}{"LAT"};
   $lon = $station_info{$site}{"LON"};
   $alt = $station_info{$site}{"ALT"};
   printf CLS ("%s%s\n",$header[3],&find_pos($lat,$lon,$alt));
   printf CLS  ("%s%s\n",$header[4],$actual);
   if($LOCAL && $BALLOON){
       printf CLS ("%s%s%3d%s%5.2f%s\n",$header[7],"Balloon size: ",$size," grams Assumed ascension rate: ",$w," m/s");
       printf CLS ("%s%s\n",$header[8],"UTC might be local time");
       printf CLS ("%s%s\n",$header[9],substr($file,12));
       for $i(0..2){ printf CLS ("%s\n","/");}
   }elsif($BALLOON){
       printf CLS ("%s%s%3d%s%5.2f%s\n",$header[7],"Balloon size: ",$size," grams Assumed ascension rate: ",$w," m/s");
       printf CLS ("%s%s\n",$header[9],substr($file,12));
       for $i(0..3){ printf CLS ("%s\n","/");}
   }elsif($LOCAL){
       printf CLS ("%s%s\n",$header[8],"GMT might be local time");
       printf CLS ("%s%s\n",$header[9],substr($file,12));
       for $i(0..3){ printf CLS ("%s\n","/");}
   }else{
       printf CLS ("%s%s\n",$header[9],substr($file,12));
       for $i(0..4){ printf CLS ("%s\n","/");}
   }
   printf CLS ("%s%s\n",$header[6],$GMT);
   for $i(0..2){printf CLS ("%s\n",$line[$i]);}
}# end sub write_header

sub find_pos{
    local ($lat,$lon,$alt) = @_;
    if($lon != 9999.0){
	$stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %8.3f, %7.3f, %8.2f",&calc_pos($lon,$lat),$alt);
	return $stn_loc;
    }else{
	$string = "999 99.00'W, 99 99.00'N, 9999.000, 999.000,";
	$stn_loc = sprintf("%s %8.2f",$string,$alt);
	return $stn_loc;
    }# end if-else
}# sub find_pos

sub filename{
    #print "line = $_[0]\n";
    my $line = $_[0];local $not_found = $TRUE;local $i = 0;
    my $mth = "";local $year = -1;local $dd = -1;local $time = -1;#$FILE_OPEN = $FALSE;
    local $dir = &find_file_dir;
    $LOCAL = $FALSE;$BALLOON = $FALSE;$size = 99;$w = 999.0;
    $found = $FALSE;
    if($ALIAS){
		$line =~ s/$alias_site//ig;
    }else{
		$line =~ s/$site//ig;
    }
    $line =~ s/(\bdel\b|de)//gi;
	$numbers = $line;$numbers =~ s/[A-Za-z;]/ /ig;
    $numbers =~ s/[\/\(\)\=\-]/ /sgi;
    #print LOG "number $numbers\n";
    while(!$found && defined($months[$i])){
	$mth = $months[$i];#print "$mth $not_found $i\n";
	$spanish = $MONTHS{$mth}{"SPANISH"};
	$abrev = substr($spanish,0,3);#print $line,"\n";
	#print "$site $mth $not_found $spanish\n";    
	if($line=~/(\d{1,2})\s*$spanish\s*(\d{4}|\d{2})[\.;]*\s+(?:[a-z]*|):*(\d{2}|\d{1})[\.:]*(\d{2})/i){
            ($dd,$year,$hh,$mm) = ($1,$2,$3,$4);
	    $found = $TRUE;
            if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
        }elsif($line=~/(?:\.|\-)$spanish\.(\d{2})\-(\d{4})\- Hora (\d{2})(?:\.|:)(\d{2})/i){
	    ($dd,$year,$hh,$mm) = ($1,$2,$3,$4);
	    $found = $TRUE;
            if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}   
	}elsif($line=~/(\d{2})\s+$abrev\s+(\d{2})\s+(\d{2}):(\d{2})/i){
	    ($dd,$year,$hh,$mm) = ($1,$2,$3,$4);
	    $found = $TRUE;
	    if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
        }elsif($line=~/(\d{1,2})\-*\s*$abrev\-*\s*(\d{2})\s+(\d{1,2})[\.:]*(\d{2})/i){
	    $dd = $1;($hh,$mm) = ($3,$4);
            $found = $TRUE;
            if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
        }elsif($line=~/$spanish\s+(\d{2})\s+(\d{4})\. HORA:(\d{2})(?:\.|:)(\d{2})/i){
	    ($dd,$year,$hh,$mm) = ($1,$2,$3,$4);
	    $found = $TRUE;
	    if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
	}elsif ($line=~/(\d{1,2})\s+(?:$abrev|$spanish)\s+(\d{4})\s+(\d{1,2})[\.:]*(\d{1,2})/i){
	    ($dd,$year,$hh,$mm) = ($1,$2,$3,$4);
	    $found = $TRUE;
            if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
 	}elsif($line =~ /.*$spanish\s+(\d{1,2})\s+(\d{4})\s+(\d{1,2})[\.:]*(\d{2})/i){
	    ($dd,$year,$hh,$mm) = ($1,$2,$3,$4);   
            $found = $TRUE;
	    if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
 	}elsif($line =~ /(\d{4})\s+(\d{1,2})\s+$spanish\s+(\d{1,2})[\.:]*(\d{2})/i){
	    ($year,$dd,$hh,$mm) = ($1,$2,$3,$4);   
            $found = $TRUE;
	    if(($hh>0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
	}elsif($line=~ /(?:$abrev|$spanish)-(\d{2})-(\d{4}|\d{2})-.*(\d{2}):(\d{2})/i){
	    ($dd,$year,$hh,$mm) = ($1,$2,$3,$4);
	    $found = $TRUE;
	    if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
	}elsif($line=~/(?:$abrev|$spanish)\s+(?:\d{2})*-*(\d{2})-(\d{4}|\d{2}).*(\d{2}):(\d{2})/i){
	    ($dd,$year,$hh,$mm) = ($1,$2,$3,$4);
	    $found = $TRUE;
            if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
        }elsif(exists($MONTHS{$mth}{"MISSPELL"})){
            $misspell = $MONTHS{$mth}{"MISSPELL"};
	    if($line=~/(\d{1,2}).*$misspell.*(\d{4})\s+(\d{1,2})[\.:]*(\d{2})/i){
		($dd,$year,$hh,$mm) = ($1,$2,$3,$4);
		$found = $TRUE;
		if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
	    }elsif($line=~/.*$misspell\s+(\d{1,2})\s+(\d{4})\s+(\d{1,2})[\.:]*(\d{2})/i){
		($dd,$year,$hh,$mm) = ($1,$2,$3,$4);
		$found = $TRUE;
	        if(($hh>=0 && $hh<24)&&($mm>=0 && $mm<60)){$time=$TRUE;}
            }# end if-elsif{2}
	}# end if-elsif{5}
	$i++;
    }# end while loop
    #print "$dd $year $hh $mm \n";
	unless(length($year) == 4){ 
	if($year<=0 && $year<=95){
	   
		$year=2000+$year;
        }else{
	    $year = 1900+$year;
        }# end if-else
        if(($year%4==0) && (($year%100)||($year%400==0))){
	    $MONTHS{february}{DAYS} = 29;
        };
	}# end unless
	if($dd > 0 && $year > 0 && $time > 0){
		$yy = $year;$MM = $MONTHS{$mth}{"MM"};$day = $dd;
	if($line =~ /pm/i && $hh < 13){$hh = $hh + 12;} 
	if($line =~ /12....am/){$hh = 1;}
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
	    if($dd > $MONTHS{$mth}{"DAYS"}){
		$dd = 1;$mth = $MONTHS{$mth}{"NEXT"};
	    if($MONTHS{$mth}{"MM"} eq "01"){$year++;}#end if
	    }#end if
        }#end if-elsif(6)-else
	if($line =~ /(pm|am)/i){$LOCAL = $TRUE;}
	if($line =~ /GLOBO|BLANCO|ROJO/i){
	    $BALLOON = $TRUE;$numbers =~ tr/\d\.//s;
            $numbers =~ s/[\/\(\)\=:]//gi;
	    @input = split(' ',$numbers);#print $numbers,"\n";
	    $w = pop(@input);$size = pop(@input);
	    while($size eq "." || $size < 30){
		$size = pop(@input);
	    }# end while
	    if($size > 200){$size = 30;}
            #print "w=$w size=$size\n";
	}# end if
	foreach $elem($day,$dd,$hh,$mm){$elem = sprintf("%02d",$elem);}
        $actual=sprintf("%4d, %s, %s, %s",$yy,$MM,$day,"$hh:$mm:00");
        $mon = $MONTHS{$mth}{"MM"};
	$GMT=sprintf("%4d, %s, %s, %s",$year,$mon,$dd,$nominal);
        $outfile = $station_info{$site}{"ID"}.$MONTHS{$mth}{"FILE"}.$dd;
	$outfile = $outfile.(substr($nominal,0,2)).".cls";
		
		$outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls",$station_info{$site}{"ID"},
				   $year,$MM,$day,$hh,$mm);

	print LOG " $outfile ";print "outfile: $outfile\n";
	#open(CLS,">$dir/$outfile") || die "Can't open $outfile\n";
        open(CLS,">$OUTDIR/$outfile") || die "Can't open $outfile\n";
	&writeheader;$FILE_OPEN = $TRUE;
    }# end if
    #print "$dd $mth $year $time $numbers\n";
}# sub filename

sub findarray{
    local $line = $_[0];local $not_found = $TRUE;local $i = 0;
    $WRITE = $FALSE;
    while($not_found && defined($arrays[$i])){
        $pos = index($line,$arrays[$i]); 
        if($pos >= 0){
	    $not_found = $FALSE;$WRITE = $TRUE;
	    $ARRAY = $arrays[$i];#print LOG $ARRAY;
	}# end if
	$i++;
    }# end while loop
}# sub findarray

sub writedata{
    local @outline = ();@outline = &init_line(@outline);
    local @input = split(' ',$_[0]);
    # time
    #print $input[$FIELDS{$ARRAY}{"TIME"}],"\n";
    if($input[$FIELDS{$ARRAY}{"TIME"}] ne " "){
	if(length($FIELDS{$ARRAY}{"TIME"}) == 1){
	    if($input[$FIELDS{$ARRAY}{"TIME"}] =~ /\d+\.\d+/){
		$outline[0] = $input[$FIELDS{$ARRAY}{"TIME"}]*60;
	    }else{
		$outline[0] = $input[$FIELDS{$ARRAY}{"TIME"}];
	    }#end if-else
	}else{
	    ($min,$sec) = split(' ',$FIELDS{$ARRAY}{"TIME"});
	    $outline[0] = 60*$input[$min]+$input[$sec];
        }# end if
    # U wind component
	if(defined($input[$FIELDS{$ARRAY}{"Ucmp"}])){
	    if(abs($input[$FIELDS{$ARRAY}{"Ucmp"}]) < 998.0){
		$outline[5] = $input[$FIELDS{$ARRAY}{"Ucmp"}];
		$outline[18] = 99.0;# unchecked Qu
	    }# end if
	}# end if
    # V wind component
	if(defined($input[$FIELDS{$ARRAY}{"Vcmp"}])){
	    if($outline[5] != 999.0){
		$outline[6] = $input[$FIELDS{$ARRAY}{"Vcmp"}];
		$outline[19] = 99.0;# unchecked Qv
    # Magnitude
		$outline[7] = $input[$FIELDS{$ARRAY}{"Spd"}];
    # Direction
		$outline[8] = $input[$FIELDS{$ARRAY}{"Dir"}];
	     }# end if
	 }# end if
    # Altitude
	 if(defined($input[$FIELDS{$ARRAY}{"Ucmp"}])){
	     $outline[14] = $input[$FIELDS{$ARRAY}{"Alt"}];
	 }# end if
	 &printer(@outline);$line_cnt++;
    }# end if
}# end sub writedata

sub find_file_dir{
    my @array = keys %station_name;
    my $count = 0;my $NOT_FOUND = $TRUE;
    my $station = "";my $class_dir = "";my $country = "";my $cmd = "";
    while($NOT_FOUND && defined($array[$count])){
        $country = $array[$count];
	if(grep(/$code/,keys %{$station_name{$country}})){
	    $station = lc $station_name{$country}{$code};
	    $country =~ s/ /\_/g;$country = lc $country;
	    $station =~ s/ /\_/g;
	    $class_dir="$main_dir/$country/$station/class_files/$year_from_dir";
            $NOT_FOUND = $FALSE;
	    if(!opendir(ETC,"$main_dir/$country")){
		#$cmd="mkdir $main_dir/$country";print $cmd,"\n";system($cmd);
            }# end if
            if(!opendir(ETC,"$main_dir/$country/$station")){
		#$cmd =  "mkdir $main_dir/$country/$station";
                print $cmd,"\n";system($cmd);
            }# end if
            if(!opendir(ETC,"$main_dir/$country/$station/class_files")){
		#$cmd = "mkdir $main_dir/$country/$station/class_files";
		print $cmd,"\n";system($cmd);
            }# end if
            if(!opendir(ETC,$class_dir)){
		#$cmd = "mkdir $class_dir";print $cmd,"\n";system($cmd);
            }# end if
        }# end if
	$count++;
   }# end while
   return $class_dir;
}# end sub find_file_dir

sub timestamp{
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$julian,$isdst) = gmtime(time);
    $mon+=1;$year+=1900;$year = substr($year,2,2);
    $TIME = sprintf("%02d%s%02d%s%02d",$hour,":",$min,":",$sec);
    $DATE = sprintf("%02d%s%02d%s%02d",$mon,"/",$mday,"/",$year);
    print LOG "GMT time and day $TIME $DATE\n";  
}# end sub timestamp







