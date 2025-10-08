#!/usr/bin/perl 

#-------------------------------------------------------
# create_DlyPcp_stnInfo.pl - This perl script/program
#   determines the begin and end dates for all the stns 
#   in the specified input files. Saves off stn_info and
#   creates the requested stn list files.
# 
# 2001 Nov lec
#   Created.
#
# WARNING: This s/w is ONLY set up to process
#     Daily Precip (dqcf) data files. See all "Daily pcp"
#     comments.
#-------------------------------------------------------*/
$debug = 0;
$debug2 = 0;

#-----------------------------
# First get and print the time
#-----------------------------
printf "\ncreate_DlyPcp_stnInfo.pl began on ";print scalar localtime; printf "\n"
;

print "ARGV[0], ARGV[1]: $ARGV[0], $ARGV[1]\n"; 

#-----------------------------------------------
# This subrountine reads in the default
# characteristics of the data being processed.
# This does NOT work well if parameters vary
# per stn such as elev or state. Works fine
# for many fixed networks such as NDARB. The
# Daily precip does NOT have elev in the output.
#-----------------------------------------------
sub read_network_defaults {
   my ($input_file_name) = @_;

   if ($debug2)
      {
      print "Begin read_network_defaults.\n";
      print "input_file_name: $input_file_name\n";
      }

   open (INFILE, "<$input_file_name") ||
         die "Can NOT open $input_file_name for reading";

   $line = <INFILE>; # skip first header line

   if ($debug2) { print "HEADER LINE: $line\n";}

   $line = <INFILE>;

   if ($debug2) { print "DEFAULT VALUES: $line\n";}

   @def_parts = split (' ', $line);  

   $project =   $def_parts[0];   $network =  $def_parts[1];
   $id_type =   $def_parts[2];   $platform = $def_parts[3];
   $country =   $def_parts[4];   $accuracy = $def_parts[5];
   $comm_code = $def_parts[6];   $state =    $def_parts[7];
   $county =    $def_parts[8];   $time_zone = $def_parts[9];
   $dst_switch = $def_parts[10]; $frequency = $def_parts[11];
   $elev =      $def_parts[12];  $fixed_mobile = $def_parts[13];

   if ($debug2)
      {
      print "def_parts: @def_parts\n";
      print "End read_network_defaults.\n";
      }  

   } # read_network_defaults



#---------------------------------------
# Open the input and output files.
# Process all the files (of the specified
# type that are in the current directory.
#----------------------------------------
opendir current_dir,  "$ARGV[0]" or die "Can't open current dir: $ARGV[0]";
@current_files = grep !/^\./, readdir current_dir;
closedir current_dir;

print "There are $#current_files files in current directory.\n";

open (OUTFILESTNCD, ">stationCD.out") || 
      die "Can NOT open stationCD.out file for output";
open (OUTFILESTN, ">station.out") ||
      die "Can NOT open station.out file for output"; 
open (OUTFILESTNID, ">stn_ID.out") ||
      die "Can NOT open stn_ID.out file for output";  


#---------------------------------------
# Process all the files in the directory
# that are of a valid type.
#---------------------------------------
%stnlat = (); %stnlon = ();
%stnBeg = (); %stnEnd = ();

foreach $currentfile (@current_files)
   {
   #-------------------------------------
   # Only process the Daily precip files
   #-------------------------------------
   if ($currentfile =~ m/dqcf/)
      {
      print "\n\n------------------------------------------\n";
      print "Processing $currentfile\n";

      open (INFILE, "<$ARGV[0]/$currentfile") || 
                    die "Can NOT open $ARGV[0] $currentfile file for reading";

      #---------------------------------
      # Fill the associative array with
      # all the uniq network, stnBegEnd and
      # initialize begin and end dates.
      # Add new stns as they appear. 
      #---------------------------------
      while ($line = <INFILE>)
         {
         chop ($line);

         @line_parts = split (' ', $line);

         $pcp_line = substr ($line, 62, 526); #Daily pcp
         @pcp_parts = split (' ', $pcp_line);

         $stnID = $line_parts[2]; # Daily pcp
         $lat = $line_parts[3]; # Daily pcp
         $lon = $line_parts[4]; # Daily pcp

         $date  = $line_parts[0]; # Daily pcp
         @parts = split ('\/', $date);
         $YYMM_date = $parts[0].$parts[1];   # Want YYYYMMDD

         if ($debug)
            {
            print "\nline:xxx", $line, "xxx\n";
            print "stnID:xxx", $stnID, "xxx\n";
            print "lat:xxx", $lat, "xxx\n";
            print "lon:xxx", $lon, "xxx\n";
            print "YYMM_date:xxx", $YYMM_date, "xxx\n";
            print "stnID: $stnID   stnBeg:$stnBeg{$stnID}  stnEnd:$stnEnd{$stnID}\n";
            }


         #---------------------------------------------------------
         # Go through all data values on the each line to determine
         # exact day begin end days.
         #---------------------------------------------------------
         for ($i=0; $i<=30; $i++) 
           {
           $pcp = $pcp_parts[$i*4];

           $YYMMDD_date = 0;
           $j= $i+1;

           if ($debug) { print "processing day = $j   pcp=$pcp\n"; }

           if (($pcp < -999.0) && ($pcp > -1000.0)) # missing found  - skip
              {                                         
              # Found missing - Do nothing.
              if ($debug){print "Found MISSING value at day = $j\n"; }
              }
           else # Not missing value
              {
              # Put the date into the YYMMDD final form.
              if ($j < 10)
                 {
                 $YYMMDD_date = $YYMM_date."0".$j;
                 }
              else
                 {
                 $YYMMDD_date = $YYMM_date.$j;
                 }   


              #--------------------------------------------------
              # Update the begin end dates, lat lons for each stn
              # and put them into hash tables (assoc. arrays).
              #--------------------------------------------------
              if (exists $stnlat{$stnID} )
                {
                # This stn already exists in the hash.
                # Compare dates already in hash.

                if ($debug) { print "This stn EXISTS in the hash.\n";}

                if ($YYMMDD_date < $stnBeg{ $stnID })
                  {
                  $stnBeg{ $stnID } = $YYMMDD_date;
                  }

                if ($YYMMDD_date > $stnEnd{ $stnID })
                  {
                  $stnEnd{ $stnID } = $YYMMDD_date;
                  }

                if ($debug)
                  {
                  print "EXISTS: stnID: $stnID   stnBeg:$stnBeg{$stnID}  stnEnd:$stnEnd{$stnID}\n";
                  }

                } # exists
              else
                {
                # This stn is not in the hash, so add it.
                if ($debug) { print "This stn is NEW.\n";}
  
                $stnlat{ $stnID } = $lat;
                $stnlon{ $stnID } = $lon;

                $stnBeg{ $stnID } = $YYMMDD_date;  #initialize value
                $stnEnd{ $stnID } = $YYMMDD_date;  #initialize value   

                if ($debug) {
                   print "NEW: stnID: $stnID  stnlat:$stnlat{$stnID} stnlon:$stnlon{$stnID}  stnBeg:$stnBeg{$stnID}  stnEnd:$stnEnd{$stnID}\n"; }
                }
              } # Not missing value

           } #for all 31 precip values

         } # While data in file

      } # Valid type of file

   close(INFILE);

   } # For each file in the directory.

if ($debug) { print "Write to station out files.\n";}

#--------------------------------------------------
# Print stnID with begin and End dates to output.
# The exact same stns should appear in both hashes.
#--------------------------------------------------
$count=0;
$occur = 0;

read_network_defaults($ARGV[1]);


while (($stnID, $lat) = each (%stnlat))
   {
   $count++;

   $name = $network.":".$stnID;

   printf OUTFILESTNCD
 "%-15s %4d %10.5f %11.5f %3d %5d %-46s %-3s %-8s %-8s %-2s %-2s %-3s %6.2f %-1s %4d %-15s %9.1f %1s\n",
            $stnID, $id_type, $stnlat{$stnID}, $stnlon{$stnID},
            $occur, $accuracy, $name, $comm_code,
            $stnBeg{$stnID}, $stnEnd{$stnID}, $country, $state,
            $county, $time_zone, $dst_switch, $platform,         
            $frequency, $elev, $fixed_mobile;        

   printf OUTFILESTN
"%-15s %10d %10.5f %11.5f %3d %5d %-46s %-3s %-8s %-8s %-2s %-2s %-3s %6.2f %-1s %4d %-15s %9.1f %1s\n",
      $project, $count, $stnlat{$stnID}, $stnlon{$stnID}, $occur, 
      $accuracy, $name, $comm_code,
      $stnBeg{$stnID}, $stnEnd{$stnID}, $country, $state,
      $county, $time_zone, $dst_switch, $platform,
      $frequency, $elev, $fixed_mobile;        

   printf OUTFILESTNID  "%-15s %10d %4d %-s\n",
      $project, $count, $id_type, $stnID; 
   }


close (OUTFILESTNCD);
close (OUTFILESTN);
close (OUTFILESTNID);

#----------------------------------------------------
# Copy final station out files to requested dir
# (same dir as where input *.dqcf files are located).
#----------------------------------------------------
system ("/bin/mv st*.out $ARGV[0]/.");

printf "\ncreate_DlyPcp_stnInfo.pl ended on ";
print scalar localtime; printf "\n";   
