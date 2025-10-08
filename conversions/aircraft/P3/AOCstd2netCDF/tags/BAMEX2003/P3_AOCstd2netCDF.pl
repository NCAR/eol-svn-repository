#!/bin/perl
################################################################################
#
# BAMEX_P3_AOCstd2netCDF.pl
#
# written by Janine Goldstein
# Sept 2003
# Version 1 taken from Version 1 of SALLJEX_P3_AOCstd2netCDF.pl
#
# Note: This code attemps to follow the 
# <a href="http://raf.atd.ucar.edu/Software/netCDF.html">NCAR-RAF netCDF Conventions</a>
#  However, when these conventions contradict the more recent 
# <a href="http://www.unidata.ucar.edu/staff/russ/netcdf/BestPractices.html"> Writing NetCDF Files: Best Practices</a> 
# document, the Best Practices document takes precedent.
#
#       Version 1.1, Last Updated Jan 27, 2003.
#
# Changes to this version from Version 1 of SALLJEX s/w.
# Changed name of code from SALLJEX* to BAMEX* Changed usage statement to
# get project name from constants.  Moved constants to beginning of code.
# Changed missing block to be within put_var_5.  Before, date was off by one
# second.  Now it is correct.
# If the flight rolls over midnight, then the hour,min,sec reset to
# zero, but the year, month, day is not incremented in the AOC standard data.
# Fix this here.
# Added handing of missing values.
################################################################################

use NetCDF;
use Time::Local;

$debug = 0;
%records = ();
%transformation = ();
@timeoffset = "";

#-------------------------------------------------------------
# THESE CONSTANTS MUST BE SET FOR EACH PROJECT
#-------------------------------------------------------------
$project = "BAMEX";
$project_number = "";
$century=2000;

#-------------------------------------------------------------
# usage
#-------------------------------------------------------------
if ($#ARGV <0)
  {
  print "Usage: ${project}_P3_AOCstd2netCDF.pl [raw_file]\n";
  exit 1;
  }

#-------------------------------------------------------------
# Print WARNINGS to user about hardcoded values
#-------------------------------------------------------------
print "WARNING: The following values are hardcoded.\n";
print "\t The project is $project\n";
print "\t The project number is $project_number\n";
print "\t This code converts the 1-digit year in the AOC standard file to a\n";
print "\t 4-digit year by adding $century\n";

#-------------------------------------------------------------
# Let's time how long it takes to convert a std file to NetCDF
#-------------------------------------------------------------
$begin_conversion = (times)[0];

print "Processing file $ARGV[0]\n";

#-------------------------------------------------------------
# Open an AOC standard tape format file (the input file)
#-------------------------------------------------------------
open AOC_STD, $ARGV[0] or die "Can't open $ARGV[0]:$!";

#-------------------------------------------------------------
# Read in the first record (type 1)
#-------------------------------------------------------------
$type = 1;
@{$records{$type}} = &get_rec(2,"s");
&check_rec($type);
$aircraft_number = ${$records{$type}}[3];

# Convert the 1-digit year to a 4-digit year
if (${$records{$type}}[4] < 5) {
   ${$records{$type}}[4]=${$records{$type}}[4] + $century;
} else {
   print "ERROR: Don't know what to add to year $year to get 4-digit year\n";
   exit(1);
}

# All times in std file are UTC
$year      = sprintf("%04d",${$records{$type}}[4]);
$month     = sprintf("%02d",${$records{$type}}[5]);
$day       = sprintf("%02d",${$records{$type}}[6]);
$starthour = sprintf("%02d",${$records{$type}}[7]);
$startmin  = sprintf("%02d",${$records{$type}}[8]);
$startsec  = sprintf("%02d",${$records{$type}}[9]);
$endhour   = sprintf("%02d",${$records{$type}}[10]);
$endmin    = sprintf("%02d",${$records{$type}}[11]);
$endsec    = sprintf("%02d",${$records{$type}}[12]);
$tapenum   = ${$records{$type}}[17];


#-------------------------------------------------------------
# Open a netCDF file for writing (the output file)
#  The extension .cdf is obsolete.  Use .nc instead.
#-------------------------------------------------------------
use File::Basename;
$filename = basename($ARGV[0],".std");
$ncid = NetCDF::create("$project.$filename.$starthour$startmin${startsec}_$endhour$endmin$endsec.$tapenum.nc",0);

#-------------------------------------------------------------
# Define Dimensions
#-------------------------------------------------------------
$timedimid = NetCDF::dimdef($ncid, "Time", NC_UNLIMITED);
die "Couldn't define time dimension\n" if $timedimid < 0;

#-------------------------------------------------------------
# Define Variables and associated attributes
#-------------------------------------------------------------

# base_time
$basetimeid = NetCDF::vardef($ncid, "base_time", NetCDF::LONG,[]);
die "Couldn't define base_time\n" if $basetimeid < 0;

$attid = NetCDF::attput($ncid, $basetimeid, "long_name", 
         NetCDF::CHAR,"Seconds since Jan 1, 1970.");
die "Couldn't define base_time longname\n" if $attid < 0;

$attid = NetCDF::attput($ncid, $basetimeid, "units",NetCDF::CHAR,"s");
die "Couldn't define base_time units\n" if $attid < 0;

# time_offset
$timeoffsetid = NetCDF::vardef($ncid, "time_offset", NetCDF::FLOAT,$timedimid);
die "Couldn't define time_offset\n" if $timeoffsetid < 0;

$attid = NetCDF::attput($ncid, $timeoffsetid, "long_name", 
         NetCDF::CHAR,"Seconds since base_time");
die "Couldn't define time_offset longname\n" if $attid < 0;

$attid = NetCDF::attput($ncid, $timeoffsetid, "units",NetCDF::CHAR,"s");
die "Couldn't define time_offset units\n" if $attid < 0;

# rest
&define_remaining_variables;

#Set global attributes
&set_global_attributes;

NetCDF::endef($ncid);

#---------------------------------------------------------
# Read in AOC standard tape record types.  Ignore type
# 2 and 4.  Read in 3 and 5.
# Put parameter values into variables in the netCDF file.
#---------------------------------------------------------
&calc_basetime;
if (NetCDF::varput($ncid, $basetimeid, 0,0,$basetime) < 0)
  {die "Couldn't put basetime value into data\n";}

#--------------------------------------------------------------------------
# read in the second record (type 2)
# Note that this record is strange - I don't yet understand how to parse it.
# Ignore type 2 record.
#--------------------------------------------------------------------------
print "\n";
$bytes_read = read AOC_STD, $rec_type, 4;
$rectype = int(unpack "f", $rec_type);
if ($debug) {print "Record type = $rectype\n";}
$bytes_read = read AOC_STD, $rec_type, 804;

#--------------------------------------------------------------------------
# Read in the rest of the records (type 3, repeat {type 4, type 5}, type 6)
#--------------------------------------------------------------------------
# Keep track of number of time values we have found so can use them to index
# timeoffset into NetCDF file.
$time_index = 0;

while ($bytes_read != 0) {
   # Words are 16 bits long
   @param = &get_rec(2,"s");

   if ($param[1] == 3) {
      $type = 3;

      # The record length should be 192 words
      if ($param[2] != 192) {
         print "ERROR: Wrong number of words in type $type record\n";
         exit(1);
      }

      for ($i=3;$i<$param[2];$i+=2) {
         if ($param[$i] < -30000 || $param[$i+1] < -30000) {
            print "Found a missing transformation value at word $i:${i+1}\n";
            print "Need to update code to put out -32767 in it's place\n";
            exit(1);
         }
         $transformation{$param[$i]} = $param[$i+1];
      }
   }
   elsif ($param[1] == 4) {
      # Ignore input from type 4 record.
      $type = 4;
      @{$records{$type}} = @param;
   }
   elsif ($param[1] == 5) {
      $type = 5;
      @{$records{$type}} = @param;

      # The record length is 106 words
      # THE BLOCK LENGTH IS 10 RECORDS...10 x 106 = 1060 WORDS
      # Ignore block length, just read one record at a time
      if (${$records{$type}}[2] != 106) {
         print "ERROR: Wrong number of words in type $type record\n";
         exit(1);
      }


      # Put all the AOC record type 5 variables into the NetCDF file.
      # The subroutine varput applies the transformation (if any)
      # from AOC record type 3.
      &put_type_5;

   }
   elsif ($param[1] == 6) {
      $type = 6;
      @{$records{$type}} = @param;

      # The record length should be 15 words
      if (${$records{$type}}[2] != 15) {
         print "ERROR: Wrong number of words in type $type record\n";
         exit(1);
      }

      $flight_length  = ${$records{$type}}[4] + ${$records{$type}}[5];
      $takeoff_hour   = sprintf("%02d",${$records{$type}}[6]);
      $takeoff_min    = sprintf("%02d",${$records{$type}}[7]);
      $takeoff_sec    = sprintf("%02d",${$records{$type}}[8]);
      $beginreel_hour = sprintf("%02d",${$records{$type}}[9]);
      $beginreel_min  = sprintf("%02d",${$records{$type}}[10]);
      $beginreel_sec  = sprintf("%02d",${$records{$type}}[11]);
      $endreel_hour   = sprintf("%02d",${$records{$type}}[12]);
      $endreel_min    = sprintf("%02d",${$records{$type}}[13]);
      $endreel_sec    = sprintf("%02d",${$records{$type}}[14]);
      if (${$records{$type}}[15] == 1) {
         $last_reel = "YES";
      } else {
         $last_reel = "NO";
      }
      #Set more global attributes
      NetCDF::redef($ncid);
      &set_global_att("Number_of_Seconds_in_Flight", "$flight_length");
      &set_global_att("TakeOffTime", "$takeoff_hour:$takeoff_min:$takeoff_sec");
      &set_global_att("ReelTimeInterval",
      "$beginreel_hour:$beginreel_min:$beginreel_sec-$endreel_hour:$endreel_min:$endreel_sec");
      &set_global_att("LastReel","$last_reel");
      NetCDF::endef($ncid);
   }
   else {
      print "ERROR: Unknown record type $param[1]\n"; 
      exit(1);
   }
}

#---------------------------------------------------------
# Close the input AOC standard file and output NetCDF file
#---------------------------------------------------------
NetCDF::close($ncid);
close(AOC_STD);

#---------------------------------------------
# Print the time it took to do the conversion.
#---------------------------------------------
$end_conversion = (times)[0];
printf "Done.  Code took %.2f CPU seconds to convert %d seconds of data\n",
$end_conversion - $begin_conversion, $time_index;
printf "\n*****************************************************************\n";

#---------------
# And we're done.
#---------------
exit(1);

######################################################################
# Subroutines
######################################################################
sub check_rec {
   # Check to make sure that the record type given in the first word 
   # of the record is the type we expect.
   my $type = shift;

   if ($records{$type}[1] != $type) {
      print "ERROR: Found record in file of type $records{$type}[1].";
      print " Expecting type $type\n";
      exit(1);
   }
}
#---------------------------------------------------------------------
sub get_rec {
   # Read in an AOC standard tape record

   my $num_bytes = shift;
   my $data_type = shift;
   my @params = ();

   if ($debug) {print "\n";}

   $bytes_read = read AOC_STD, $rec_type, $num_bytes;
   if ($bytes_read == 0) {last;}
   $rectype = unpack $data_type, $rec_type;
   $params[1] = $rectype;
   if ($debug) {print "Record type = $rectype\n";}

   $bytes_read = read AOC_STD, $num_words, $num_bytes;
   $numwords = unpack $data_type, $num_words;
   $params[2] = $numwords;
   if ($debug) {print "Number of words in record type $rectype = $numwords\n";}

   for ($word = 3;$word <= $numwords; $word++) {
      $bytes_read = read AOC_STD, $param, $num_bytes;
      $parameter = unpack $data_type, $param;
      $params[$word] = $parameter;
      if ($debug) {print "Parameter $word = $parameter\n";}
   }

   return(@params);
}
#---------------------------------------------------------------------
sub calc_basetime {
  # Calculate basetime from the year, month, day, hour, min, sec taken
  # from the AOC standard tape.
  # Basetime is seconds since Jan 1, 1970
  # Perl routines expect day to be in the range 1-31 and month
  # to be in the range 0-11.

  $basetime = timegm($startsec, $startmin, $starthour, $day, $month-1, $year);
  die "ERROR: Couldn't calculate basetime\n" if ($basetime < 0);
  print "Flight began on ".gmtime($basetime)." UTC\n";
}
#---------------------------------------------------------------------
sub set_global_att {
   # Set a global attribute
   my $name = shift;
   my $value = shift;
   if (NetCDF::attput($ncid, NetCDF::GLOBAL, $name, NetCDF::CHAR, $value) <0 )
      {die "Couldn't define Global attribute $name \n";}
}

#---------------------------------------------------------------------
sub define_var {

# Subroutine to define a variable.  Use UNDEF for attributes you do
# not wish to define.
#
# Usage: $id = 
# &define_var($name, $units, $long_name, $category, $rate);
#
   my $name = shift;
   my $units = shift;
   my $long_name = shift;
   my $category = shift;
   my $rate = shift;
   my $quality = shift;

   # Name - required
   if (!defined($name)) {
      print "ERROR: Variable name must be defined\n"; 
      exit(1);
   }
   $varid = NetCDF::vardef($ncid, $name, NetCDF::FLOAT,$timedimid);
   die "Couldn't define $name \n" if $varid <0;

   # Units - required
   if (!defined($units)) {
      print "ERROR: Variable units must be defined\n"; 
      exit(1);
   }
   $attid = NetCDF::attput($ncid, $varid, "units",NetCDF::CHAR,$units);
   die "Couldn't define $name units\n" if $attid < 0;

   # long_name - required
   if (!defined($long_name)) {
      print "ERROR: Variable long_name must be defined\n"; 
      exit(1);
   }
   $attid = NetCDF::attput($ncid, $varid, "long_name",NetCDF::CHAR,$long_name);
   die "Couldn't define $name long_name\n" if $attid < 0;

   #_FillValue - required
   # The P-3 fill value is -1e+38, and the C-130 uses -32767.  Since ncplot 
   # expects -32767, use that here.
   $attid = NetCDF::attput($ncid, $varid, "_FillValue",NetCDF::FLOAT, -32767);
   die "Couldn't define $name _FillValue\n" if $attid < 0;

   # Category - optional
   if (defined($category)) {
   $attid = NetCDF::attput($ncid, $varid, "Category",NetCDF::CHAR, $category);
   die "Couldn't define $name Position\n" if $attid < 0;
   }

   # Sampled Rate - optional
   if (defined($rate)) {
   $attid = NetCDF::attput($ncid, $varid, "SampledRate",NetCDF::CHAR, $rate);
   die "Couldn't define $name SampledRate\n" if $attid < 0;
   }

   return $varid;
}
#---------------------------------------------------------------------
sub put_vars {
#
# Put actual data into a previously defined variable
#
   my $id = shift;
   my $name = shift;
   my $word = shift;

      # Don't perform transformation on missing values (-32767)
      if (${$records{$type}}[$word] > -32766) {
          $val = ${$records{$type}}[$word]/$transformation{$word};
      } else {
          $val = ${$records{$type}}[$word];
      }
 
      if (NetCDF::varput($ncid, $id, $time_index, 1, $val))
         {die "Couldn't put $name data into NetCDF file\n";}
}                                                           
#---------------------------------------------------------------------

sub define_remaining_variables {
   # Define the rest using the subroutine &define_var
   # Usage: $id = 
   # &define_var($name, $units, $long_name, $category, $rate);
   #
   # For conventions on naming units, see
   # <a href="http://my.unidata.ucar.edu/content/software/udunits/">UDUNITS</a>
   #
$yearid = &define_var("YEAR","common_year","UTC Raw Tape Date Component","None",1);
$monid  = &define_var("MONTH","month","UTC Raw Tape Date Component","None",1);
$dayid  = &define_var("DAY","day","UTC Raw Tape Date Component","None",1);
$hourid = &define_var("HOUR","hours","UTC Raw Tape Time Component","None",1);
$minid  = &define_var("MINUTE","minutes","UTC Raw Tape Date Component","None",1);
$secid  = &define_var("SECOND","s","UTC Raw Tape Date Component","None",1);
$timeerrid = &define_var("TIME_ERR",UNDEF,"TIME ERROR FLAG","None",1);
$latid  = &define_var("LAT","degrees_north","LATITUDE Xla1+Xla2/60","Position",1);
$laterrid  = &define_var("LAT_ERR",UNDEF,"LATITUDE ERROR FLAG","Position",1);
$lonid  = &define_var("LON","degrees_east","LONGITUDE Xlo1+Xlo2/60","Position",1);
$lonerrid  = &define_var("LON_ERR",UNDEF,"LONGITUDE ERROR FLAG","Position",1);
$altid  = &define_var("Ralt","meters","RADAR ALTITUDE","Position",1);
$alterrid  = &define_var("Ralt_ERR",UNDEF,"RADAR ALTITUDE ERROR FLAG","Position",1);
$presid = &define_var("PS","mbar","PRESSURE","Atmos. State",1);
$preserrid = &define_var("PS_ERR",UNDEF,"PRESSURE ERROR FLAG","Atmos. State",1);
$tempid = &define_var("TA","degC","AMBIENT TEMPERATURE","Atmos. State",1);
$temperrid = &define_var("TA_ERR",UNDEF,"AMBIENT TEMPERATURE ERROR FLAG","Atmos. State",1);
$dewptid= &define_var("TW1","degC","DEWPOINT SENSOR","Atmos. State",1);
$dewpterrid= &define_var("TW1_ERR",UNDEF,"DEWPOINT SENSOR ERROR FLAG","Atmos. State",1);
$rdid   = &define_var("RD","degC","RADIOMETER DOWN",UNDEF,1);
$rderrid = &define_var("RD_ERR",UNDEF,"RADIOMETER DOWN ERROR FLAG",UNDEF,1);
$rsid   = &define_var("RS","degC","RADIOMETER SIDE",UNDEF,1);
$rserrid = &define_var("RS_ERR",UNDEF,"RADIOMETER SIDE ERROR FLAG",UNDEF,1);
$gsid   = &define_var("GS","m/s","GROUND SPEED","Aircraft State",1);
$gserrid = &define_var("GS_ERR",UNDEF,"GROUND SPEED ERROR FLAG","Aircraft State",1);
$tsid   = &define_var("TS","m/s","TRUE AIRSPEED","Aircraft State",1);
$tserrid = &define_var("TS_ERR",UNDEF,"TRUE AIRSPEED ERROR FLAG","Aircraft State",1);
$wgsid  = &define_var("WGS","m/s","VERT. GROUND SPEED","Aircraft State",1);
$wgserrid = &define_var("WGS_ERR",UNDEF,"VERT. GROUND SPEED ERROR FLAG","Aircraft State",1);
$tkid   = &define_var("TK","degrees","TRACK","Aircraft State",1);
$tkerrid = &define_var("TK_ERR",UNDEF,"TRACK ERROR FLAG","Aircraft State",1);
$hdid   = &define_var("HD","degrees","HEADING (TRUE)","Aircraft State",1);
$hderrid = &define_var("HD_ERR",UNDEF,"HEADING (TRUE) ERROR FLAG","Aircraft State",1);
$pcid   = &define_var("PC","degrees","PITCH","Aircraft State",1);
$pcerrid = &define_var("PC_ERR",UNDEF,"PITCH ERROR FLAG","Aircraft State",1);
$rlid   = &define_var("RL","degrees","ROLL","Aircraft State",1);
$rlerrid = &define_var("RL_ERR",UNDEF,"ROLL ERROR FLAG","Aircraft State",1);
$aaid   = &define_var("AA","degrees","ATTACK ANGLE","Aircraft State",1);
$aaerrid = &define_var("AA_ERR",UNDEF,"ATTACK ANGLE ERROR FLAG","Aircraft State",1);
$said   = &define_var("SA","degrees","SLIP ANGLE","Aircraft State",1);
$saerrid = &define_var("SA_ERR",UNDEF,"SLIP ANGLE ERROR FLAG","Aircraft State",1);
$lwid   = &define_var("LW","gram/m3","J-W LIQ. WATER","Liquid Water",1);
$lwerrid = &define_var("LW_ERR",UNDEF,"J-W LIQ. WATER ERROR FLAG","Liquid Water",1);
$pqid   = &define_var("PQ","mbar","DYNAMIC PRESSURE","Aircraft State",1);
$pqerrid = &define_var("PQ_ERR",UNDEF,"DYNAMIC PRESSURE ERROR FLAG","Aircraft State",1);
$tdid   = &define_var("TD","degC","DEWPOINT TEMPERATURE","Atmos. State",1);
$ruid   = &define_var("RU","degC","RADIOMETER UP (IF AVAIL)","Radiation",1);
$sw1id  = &define_var("SW1",UNDEF,"SWITCHES",UNDEF,1);
$sw2id  = &define_var("SW2",UNDEF,"SWITCHES",UNDEF,1);
$sw3id  = &define_var("SW3",UNDEF,"SWITCHES",UNDEF,1);
$utailid= &define_var("UTAIL",UNDEF,"E/W VELOCITY OF TAIL","Aircraft State",1);
$vtailid= &define_var("VTAIL",UNDEF,"N/S VELOCITY OF TAIL","Aircraft State",1);
$wtailid= &define_var("WTAIL",UNDEF,"VERT VELOCITY OF TAIL","Aircraft State",1);
$gaid   = &define_var("GA","m","GEOPOTENTIAL ALT. (METER)","Position",1);
$paltid = &define_var("PALT","m","PRESSURE ALT. (METER)","Position",1);
$dvid   = &define_var("DV","m","D-VALUE","Position",1);
$htid   = &define_var("HT","m","HEIGHT STANDARD",UNDEF,1);
$spid   = &define_var("SP","mbar","SURFACE PRESSURE","Thermodynamic",1);
$rhid   = &define_var("RH","%","RELATIVE HUMIDITY","Atmos. State",1);
$tvid   = &define_var("TV","degK","VIRTUAL TEMPERATURE","Thermodynamic",1);
$wasid  = &define_var("WAS","m/s","VERTICAL AIRSPEED","Aircraft State",1);
$gmid   = &define_var("GM",UNDEF,"RATIO SPECIFIC HEATS",UNDEF,1);
$amaid  = &define_var("AMA",UNDEF,"MACH NUMBER","Thermodynamic",1);
$daid   = &define_var("DA","degrees","DRIFT",UNDEF,1);
$gsxid  = &define_var("GSX","m/s","E/W GROUND SPEED","Aircraft State",1);
$gsyid  = &define_var("GSY","m/s","N/S GROUND SPEED","Aircraft State",1);
$txid   = &define_var("TX","m/s","E/W TRUE AIRSPEED","Aircraft State",1);
$tyid   = &define_var("TY","m/s","N/S TRUE AIRSPEED","Aircraft State",1);
$wxid   = &define_var("WX","m/s","E/W WIND SPEED","Wind",1);
$wyid   = &define_var("WY","m/s","N/S WIND SPEED","Wind",1);
$wzid   = &define_var("WZ","m/s","VERTICAL WIND SPEED","Wind",1);
$wsid   = &define_var("WS","m/s","WIND SPEED","Wind",1);
$wdid   = &define_var("WD","degrees","WIND DIRECTION","Wind",1);
$ewid   = &define_var("EW","mbar","SAT. VAPOR PRES","Thermodynamic",1);
$eeid   = &define_var("EE","mbar","VAPOR PRESSURE","Thermodynamic",1);
$mrid   = &define_var("MR","gram/kg","MIXING RATIO","Atmos. State",1);
$ptid   = &define_var("PT","degK","POTENTIAL TEMP","Thermodynamic",1);
$etid   = &define_var("ET","degK","EQUIVALENT POT.TEMP.","Thermodynamic",1);
$wxbid  = &define_var("WXB","m/s","E/W AVERAGE WIND","Wind",1);
$wybid  = &define_var("WYB","m/s","N/S AVERAGE WIND","Wind",1);
$wsbid  = &define_var("WSB","m/s","AVERAGE WIND SPEED","Wind",1);
$wdbid  = &define_var("WDB","degrees","AVERAGE WIND DIR","Wind",1);
$av1id  = &define_var("AV1","m/s2","VERTICAL ACCELEROMETER #1","Aircraft State",1);
$av2id  = &define_var("AV2","m/s2","VERTICAL ACCELEROMETER #2","Aircraft State",1);
$wacid  = &define_var("WAC","s","SEC. WIND AVERAGED OVER","Wind",1);
$bt1id  = &define_var("BT1","degC","AXBT1 Ch 1 TEMP",UNDEF,1);
   $attid = NetCDF::attput($ncid, $bt1id, "Comments",NetCDF::CHAR, 
    "AXBT DATA REPRESENTS OCEAN TEMPERATURES FROM EXPENDABLES LAUNCHED FROM THE AIRCRAFT");
   die "Couldn't define BT1 Comment\n" if $attid < 0;
$bt2id  = &define_var("BT2","degC","AXBT2 Ch 2 TEMP",UNDEF,1);
   $attid = NetCDF::attput($ncid, $bt2id, "Comments",NetCDF::CHAR, 
    "AXBT DATA REPRESENTS OCEAN TEMPERATURES FROM EXPENDABLES LAUNCHED FROM THE AIRCRAFT");
   die "Couldn't define BT2 Comment\n" if $attid < 0;
$bt3id  = &define_var("BT3","degC","AXBT3 Ch 3 TEMP",UNDEF,1);
   $attid = NetCDF::attput($ncid, $bt3id, "Comments",NetCDF::CHAR, 
    "AXBT DATA REPRESENTS OCEAN TEMPERATURES FROM EXPENDABLES LAUNCHED FROM THE AIRCRAFT");
   die "Couldn't define BT3 Comment\n" if $attid < 0;
$navid  = &define_var("NAV",UNDEF,"1 = INE 1, 2 = INE 2",UNDEF,1);
$itmpid = &define_var("ITMP",UNDEF,"TOTAL TEMP USED: 1 OR 2",UNDEF,1);
$kingid = &define_var("KING","volts","KING LIQ. WATER VOLTAGE",UNDEF,1);
$vgsid  = &define_var("VGS","m/s","DPJ's VERT GRND SPEED","Aircraft State",1);
$vasid  = &define_var("VAS","m/s","DPJ's VERT AIR SPEED","Aircraft State",1);
$vwid   = &define_var("VW","m/s","DPJ's VERT WIND","Wind",1);

}
#---------------------------------------------------------------------
sub set_global_attributes {
   &set_global_att("Source","UCAR Joint Office for Science Support");
   &set_global_att("Address","P.O. Box 3000, Boulder, CO 80307-3000");
   &set_global_att("Phone","(303) 497-1000");
   &set_global_att("Conventions","NCAR-RAF/nimbus");
   &set_global_att("Version","1");
   $curtime = gmtime; $curtime = $curtime." UTC";
   &set_global_att("DateProcessed",$curtime);
   &set_global_att("ProjectName",$project);
   &set_global_att("Aircraft","N${aircraft_number}RF");
   &set_global_att("ProjectNumber",$project_number);
   &set_global_att("FlightNumber",$filename);
   &set_global_att("TapeNumber","Tape Number $tapenum");
   &set_global_att("FlightDate","$month/$day/$year");
   &set_global_att("TimeInterval",
       "$starthour:$startmin:$startsec-$endhour:$endmin:$endsec");
   # The next 4 attributes are place holders.  There values will be
   # set after they are read from record type 6 at the end of the file.
   &set_global_att("Number_of_Seconds_in_Flight", "unknown");
   &set_global_att("TakeOffTime", "unknown");
   &set_global_att("ReelTimeInterval", "unknown");
   &set_global_att("LastReel","unknown");
   &set_global_att("Categories", 
       "Position,Thermodynamic,Aircraft State,Atmos. State,Liquid Water,Uncorr\'d Raw,Wind,PMS Probe,Housekeeping,Chemistry,Radiation,Non-Standard");
}
#---------------------------------------------------------------------
sub put_type_5 {
      $hour = ${$records{$type}}[3];
      $min = ${$records{$type}}[4];
      $sec = ${$records{$type}}[5];

      # If the flight rolls over midnight, then the hour,min,sec reset to
      # zero, but the year, month, day is not incremented. Fix this here.
      &increment_date if ($hour == 0 && $min == 0 && $ sec == 0);

      # Check for missing values
      for ($i=3;$i<${$records{$type}}[2];$i++) {
         if (${$records{$type}}[$i] == -32767) {
            #print "Found a missing value at record 5 word $i on ";
            #print "$year/$month/$day $hour:$min:$sec: ${$records{$type}}[$i]\n";         
         } elsif (${$records{$type}}[$i] == 32767) {
            #print "Found a missing value at record 5 word $i on ";
            #print "$year/$month/$day $hour:$min:$sec: ${$records{$type}}[$i]";
            #print " Set to -32767\n";
            ${$records{$type}}[$i] = -32767;
         } elsif ( $i != 6 && (${$records{$type}}[$i] < -30000 ||
                  ${$records{$type}}[$i] > 30000)) {
            # Word 6 is the record count.  It can be > 30000.
            print "Found a possible missing value at record 5 word $i on ";
            print "$year/$month/$day $hour:$min:$sec: ${$records{$type}}[$i]";
            print "/$transformation{$i}\n";
         }
      }  
      
      # Calculate unix time and take diff with Jan 1, 1970.  Result is time
      # offset. All times in std file are UTC
      die "ERROR: Time out of range $hour:$min:$sec\n"
         if ($hour > 23 || $min > 59 || $ sec > 59);
      $timeoffset[$time_index] = 
         timegm($sec, $min, $hour, $day, $month-1, $year)-$basetime;

      # Time should increment 1 second at a time.  If not, warn user.
      if ($time_index > 0 && 
          $timeoffset[$time_index] - $timeoffset[$time_index-1] != 1) {
         print "WARNING: Time increment was not 1 second: Previous time = ";
         print "$timeoffset[$time_index-1], Current time = ";
         print "$timeoffset[$time_index] ($year/$month/$day $hour:$min:$sec)";
         print " Time increment is calculated from year,mon,day,hr,min,sec\n";
      }

      if (NetCDF::varput($ncid, $timeoffsetid, $time_index, 1, $timeoffset[$time_index]))
         {die "Couldn't put time offset value into data\n";}

      if (NetCDF::varput($ncid, $yearid, $time_index, 1, $year))
         {die "Couldn't put year value into data\n";}
      if (NetCDF::varput($ncid, $monid, $time_index, 1, $month))
         {die "Couldn't put month value into data\n";}
      if (NetCDF::varput($ncid, $dayid, $time_index, 1, $day))
         {die "Couldn't put day value into data\n";}
      if (NetCDF::varput($ncid, $hourid, $time_index, 1, $hour))
         {die "Couldn't put hour value into data\n";}
      if (NetCDF::varput($ncid, $minid, $time_index, 1, $min))
         {die "Couldn't put min value into data\n";}
      if (NetCDF::varput($ncid, $secid, $time_index, 1, $sec))
         {die "Couldn't put sec value into data\n";}

      # WORDS 10 AND 11 ARE ERROR FLAGS
      # FLAG 1 IS IN HIGH ORDER BIT OF WORD 10.  IF THE BIT IS ON,
      # A 1 INSTEAD OF A 0, THE DATA MAY BE IN ERROR.
      #
      # THE FOLLOWING IS A LIST ASSOCIATING AN ERROR FLAG TO A
      # PARAMETER:
      #
      #      FLAG 1   TIME                    FLAG 11   TRUE AIRSPEED
      #           2   LATITUDE                     12   VERTICAL SPEED
      #           3   LONGITUDE                    13   TRACK
      #           4   RADAR ALT                    14   HEADING
      #           5   AMBIENT PRESS                15   PITCH
      #           6   AMBIENT TEMP                 16   ROLL
      #           7   DEWPOINT TEMP                17   ATTACK ANGLE
      #           8   DOWN RADIOMETER              18   SLIP ANGLE
      #           9   SIDE RADIOMETER              19   LIQUID WATER
      #          10   GROUND SPEED                 20   DYNAMIC PRESS
      #
      #  Word 10 contains flags 1-16 and word 11 contains flags 17-20.  
      #
      #  The high order bit is the most significant bit, the bit that 
      #  contributes the greatest value, i. e. 128 in an 8-bit byte.
      #  If word 10 contains the value 32768, then the high order bit 
      #  of word 10 is set, and flag 1 indicates that TIME may be in error.
      #
      #  The log base 2 of a value is equal to the natural log of the 
      #  value divided by the natural log of 2.  We can determine the bit
      #  that is set bu finding the log base 2 of the value stored in
      #  this word.
      #
      #  there are no transformations for words 10 and 11
      # 
      @id = ($timeerrid,$laterrid,$lonerrid,$alterrid,$preserrid,$temperrid,
             $dewpterrid,$rderrid,$rserrid,$gserrid,$tserrid,$wgserrid,$tkerrid,
             $hderrid,$pcerrid,$rlerrid,$aaerrid,$saerrid,$lwerrid,$pqerrid);
      @name = ("TIME_ERR","LAT_ERR","LON_ERR","Ralt_ERR", "PS_ERR","TA_ERR",
               "TW1_ERR","RD_ERR","RS_ERR","GS_ERR","TS_ERR","WGS_ERR","TK_ERR",
               "HD_ERR","PC_ERR","RL_ERR","AA_ERR","SA_ERR","LW_ERR","PQ_ERR");
      for ($i = 15; $i >=0; $i--) {
         $val = 2**$i;
         if (${$records{$type}}[10] != 0 && ${$records{$type}}[10] >= $val) {
             ${$records{$type}}[10] = ${$records{$type}}[10] - $val;
             $flag = 16 - int(((log $val)/(log 2))+0.5);
             #print "Word 10 (ERROR FLAGS): ";
             #print $val + ${$records{$type}}[10];
             #print "; flag = $flag\n";
             $errval = 1;
             #print "at time $time_index $name[$flag-1] = $errval\n";
         } else {
             $flag = 16 - $i;
             $errval = 0;
             #print "at time $time_index $name[$flag-1] = $errval\n";
         }
         if (NetCDF::varput($ncid, $id[$flag-1], $time_index, 1, $errval))
            {die "Couldn't put $name[$flag-1] data into NetCDF file\n";}
      }
      for ($i = 15; $i >=12; $i--) {
         $val = 2**$i;
         if (${$records{$type}}[11] != 0 && ${$records{$type}}[11] >= $val) {
             ${$records{$type}}[11] = ${$records{$type}}[11] - $val;
             $flag = 32 - int(((log $val)/(log 2))+0.5);
             #print "Word 11 (ERROR FLAGS): ";
             #print $val + ${$records{$type}}[11];
             #print "; flag = $flag\n";
             $errval = 1;
             #print "at time $time_index $name[$flag-1] = $errval\n";
         } else {
             $flag = 32 - $i;
             $errval = 0;
             #print "at time $time_index $name[$flag-1] = $errval\n";
         }
         if (NetCDF::varput($ncid, $id[$flag-1], $time_index, 1, $errval))
            {die "Couldn't put $name[$flag-1] data into NetCDF file\n";}

      }

print ${$records{$type}}[12]." ".$transformation{12}." ".
${$records{$type}}[13]." ".$transformation{13}."\n";
      $lat = ${$records{$type}}[12]/$transformation{12} + 
             (${$records{$type}}[13]/$transformation{13})/60;
      if (NetCDF::varput($ncid, $latid, $time_index, 1, $lat))
         {die "Couldn't put lat value into data\n";}

      $lon = ${$records{$type}}[14]/$transformation{14} + 
             (${$records{$type}}[15]/$transformation{15})/60;
      if (NetCDF::varput($ncid, $lonid, $time_index, 1, $lon))
         {die "Couldn't put lon value into data\n";}

      &put_vars($altid,"Ralt",16);
      &put_vars($presid,"PS",17);
      &put_vars($tempid,"TA",18);
      &put_vars($dewptid,"TW1",19);
      &put_vars($rdid,"RD",20);
      &put_vars($rsid,"RS",21);
      &put_vars($gsid,"GS",22);
      &put_vars($tsid,"TS",23);
      &put_vars($wgsid,"WGS",24);
      &put_vars($tkid,"TK",25);
      &put_vars($hdid,"HD",26);
      &put_vars($pcid,"PC",27);
      &put_vars($rlid,"RL",28);
      &put_vars($aaid,"AA",29);
      &put_vars($said,"SA",30);
      &put_vars($lwid,"LW",31);
      &put_vars($pqid,"PQ",32);
      &put_vars($tdid,"TD",33);
      &put_vars($ruid,"RU",34);

      # there are no transformations for words 8 and 9
      # However, by setting them to 32768, we set the value
      # of SW1 and SW2 to one if the high order bit is set,
      # else the value is zero.
      $transformation{8} = 32768;
      $transformation{9} = 32768;
      if ($transformation{35} == 1) {
         $transformation{35} == 32768;
      } else {
         print "ERROR: Found a non-unary transformation for word 35=";
         print " $transformation{35}\n";
         exit(1);
      }

      &put_vars($sw1id,"SW1",8);
      &put_vars($sw2id,"SW2",9);
      &put_vars($sw3id,"SW3",35);
      &put_vars($utailid,"UTAIL",36);
      &put_vars($vtailid,"VTAIL",37);
      &put_vars($wtailid,"WTAIL",38);
      &put_vars($gaid,"GA",40);
      &put_vars($paltid,"PALT",41);
      &put_vars($dvid,"DV",42);
      &put_vars($htid,"HT",43);
      &put_vars($spid,"SP",44);
      &put_vars($rhid,"RH",45);
      &put_vars($tvid,"TV",46);
      &put_vars($wasid,"WAS",47);
      &put_vars($gmid,"GM",48);
      &put_vars($amaid,"AMA",49);
      &put_vars($daid,"DA",50);
      &put_vars($gsxid,"GSX",51);
      &put_vars($gsyid,"GSY",52);
      &put_vars($txid,"TX",53);
      &put_vars($tyid,"TY",54);
      &put_vars($wxid,"WX",55);
      &put_vars($wyid,"WY",56);
      &put_vars($wzid,"WZ",57);
      &put_vars($wsid,"WS",58);
      &put_vars($wdid,"WD",59);
      &put_vars($ewid,"EW",60);
      &put_vars($eeid,"EE",61);
      &put_vars($mrid,"MR",62);
      &put_vars($ptid,"PT",63);
      &put_vars($etid,"ET",64);
      &put_vars($wxbid,"WXB",65);
      &put_vars($wybid,"WYB",66);
      &put_vars($wsbid,"WSB",67);
      &put_vars($wdbid,"WDB",68);
      &put_vars($av1id,"AV1",69);
      &put_vars($av2id,"AV2",70);
      &put_vars($wacid,"WAC",71);
      &put_vars($bt1id,"BT1",72);
      &put_vars($bt2id,"BT2",73);
      &put_vars($bt3id,"BT3",74);
      &put_vars($navid,"NAV",75);
      &put_vars($itmpid,"ITMP",76);
      &put_vars($kingid,"KING",79);
      &put_vars($vgsid,"VGS",80);
      &put_vars($vasid,"VAS",81);
      &put_vars($vwid,"VW",82);

      $time_index++;

      # Now perform some sanity checks

      # WORDS 6 AND 7 ARE RECORD COUNTS  TOTAL COUNT=RC1+RC2
      # At each record, check the the record count matches what I
      # think it should be, else exit.
      $total_count = ${$records{$type}}[6] + ${$records{$type}}[7];
      if ($total_count != $time_index) {
         print "Total record count: $total_count != time index: $time_index\n";
         exit(1);
      }

      # WORDS 8,9,35 ARE EVENT SWITCHES
      # HIGH ORDER BIT OF WORD 8 IS SWITCH 1
      if (${$records{$type}}[8] != 0) {
         print "Word 8 (EVENT SWITCH) is not zero: ${$records{$type}}[8]\n";
      }
      if (${$records{$type}}[9] != 0) {
         print "Word 9 (EVENT SWITCH) is not zero: ${$records{$type}}[9]\n";
      }
      if (${$records{$type}}[35] != 0) {
         print "Word 35 (EVENT SWITCH) is not zero: ${$records{$type}}[35]\n";
      }

      # WORDS 78 THRU 106 ARE BLANK EXCEPT FOR HURRICANE TAPES


      # For testing purposes, stop after 10 minutes of data
      #if ($time_index == 600) {last;} 
}
#---------------------------------------------------------------------
sub increment_date {
     print "Incrementing date from $year/$month/$day $hour:$min:$sec to ";

    #-------------------------------
    # Determine if it is a leap year
    #-------------------------------
    $leap_year = 0;
    if ( (($year % 4 == 0) && ($year % 100 != 0)) || ($year % 400 == 0) )
        {$leap_year = 1;}

    $day = $day + 1;

    if ($month == 2 && $day > 28 && $leap_year == 0)
        {
        $month = $month + 1;
        $day = 1;
        }
    elsif ($month == 2 && $day > 29 && $leap_year == 1)
        {
        $month = $month + 1;
        $day = 1; 
        }
    elsif (($month == 4 || $month == 6 || $month == 9 ||
             $month == 11) && ($day > 30) )
        {
        $month = $month + 1;
        $day = 1;
        }
    elsif ($day > 31)
        {
        if ($month != 12)
            {
            $month = $month + 1;
            $day = 1;
            }
        else
            {
            $year = $year + 1;
            $month = $day = 1;
            }
        }

    print "$year/$month/$day $hour:$min:$sec\n";

}
