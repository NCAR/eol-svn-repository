#!/usr/bin/perl 

#-------------------------------------------------------------------
# form_webPage_output.pl - This perl program creates a colorized
#   web page from the output of the det_closest_stn.c s/w.
#   Currently this code runs on the *stationCD.out form of the
#   station list. It's really just looking for the station name
#   in the first 15 chars followed by the station lat and lon
#   in the next columns. That's the minimum info required to
#   compute distances.
#
#   Execute: form_webPage_output.pl input_station_list [AOIs]
#
#   Required Inputs:
#
#
#   Output:
#      A web page containing colorized levels for the distances
#      comparing each station in the input station list.
#
# Execute: form_webPage_output.pl input_station_list AOI_distance
#--------------------------------------------------------------------
#
# May 2004 lec
#   Created Version 1.0.
#-------------------------------------------------------
$number_lines_in_file = 0;
$number_lines_processed = 0;

$NUMBER_STNS_TO_PRINT = 10;

#-----------
# Initialize
#-----------
%stnInfo = ();


##################################################
#----------------------------------------------
# create_web_page()
#
# This subroutine takes the output file generated
# by C program det_closest_stn and converts it
# into a nicer/color coded web page format for
# easy viewing.
#
# Assume input file name for each distance are
# of the form Closest_stns_LIST_xxxKM.out.
#----------------------------------------------
sub create_web_page {
   my ($station_info_file, $distanceKM) = @_;

   if ($debug) { print "Enter create_web_page with station_info_file = $station_info_file; distanceKM = $distanceKM\n";}

   #-----------
   # Initialize
   #-----------
   %stnInfo = ();

   $number_lines_processed = 0;

   open (DIST_INPUT_FILE, "<Closest_stns_LIST_$distanceKM.out") || die "Can NOT open Closest_stns_LIST_$distanceKM.htm for reading";
   open (DIST_OUTPUT_FILE, ">webPage_$distanceKM.htm") || die "Can NOT open webPage_$distanceKM.htm for writing";

   #-------------------------------------------------------
   # Read in detailed station info from stationCD.out file.
   #-------------------------------------------------------
   open (INFILE_STATIONCD, "<$station_info_file") ||
     die "Can NOT open $station_info_file for reading";

   @stationCD_lines = <INFILE_STATIONCD>; # Read all lines into an array.

   #-----------------------------------------
   # Output standard information to web page.
   #-----------------------------------------
   printf DIST_OUTPUT_FILE "<HTML>\n";
   printf DIST_OUTPUT_FILE "<!-- ================================================= -->\n";
   printf DIST_OUTPUT_FILE "<body bgcolor=white>\n";
   printf DIST_OUTPUT_FILE "<TITLE>Top Stations within $distanceKM KM AOI of Current_Station</TITLE>\n";
   printf DIST_OUTPUT_FILE "<center>\n";
   printf DIST_OUTPUT_FILE "<H1>Top Stations within $distanceKM KM AOI of Current_Station</H1>\n";
   printf DIST_OUTPUT_FILE "</center>\n";
   printf DIST_OUTPUT_FILE "<br>\n";
   printf DIST_OUTPUT_FILE "<b>Key:<br>\n";
   printf DIST_OUTPUT_FILE "<font color=red>&nbsp;&nbsp;&nbsp;Red = Closest Station;</font><br>\n";
   printf DIST_OUTPUT_FILE "<font color=orange>&nbsp;&nbsp;&nbsp;Orange</font> = Station within <font color=orange>10KM</font>;<br>\n";
   printf DIST_OUTPUT_FILE "<font color=yellowgreen>&nbsp;&nbsp;&nbsp;Yellow</font> = Station within <font color=yellowgreen>25KM</font>;<br>\n";
   printf DIST_OUTPUT_FILE "<font color=green>&nbsp;&nbsp;&nbsp;Green</font> = Station within <font color=green>50KM</font>;<br>\n";
   printf DIST_OUTPUT_FILE "<font color=blue>&nbsp;&nbsp;&nbsp;Blue</font> = Station within <font color=blue>75KM</font>;<br>\n";
   printf DIST_OUTPUT_FILE "<font color=purple>&nbsp;&nbsp;&nbsp;Purple</font> = Station within <font color=purple>100KM</font>;<br>\n";
   printf DIST_OUTPUT_FILE "&nbsp;&nbsp;&nbsp;Black = Station further than 100KM;</b><br>\n";
   printf DIST_OUTPUT_FILE "<b><font color=gray>&nbsp;&nbsp;&nbsp;Gray</font> = <font color=gray>Duplicate</font> Distance Station.</b><br>\n";
   printf DIST_OUTPUT_FILE "<br><br>\n";
   printf DIST_OUTPUT_FILE "<b>Station List Processed: <a href='./$station_info_file'>$station_info_file</a><br>\n";
   printf DIST_OUTPUT_FILE "<br><br>\n";
   printf DIST_OUTPUT_FILE "<hr>\n";

   $station_index = 0;
   while ($input_line = <DIST_INPUT_FILE>)
      {
      $number_lines_processed++;

      if ($debug) { print "Line: $number_lines_processed; Input_line=xxx $input_line xxx\n";}
       
      if ( $number_lines_processed != 1 && $number_lines_processed != 2 && $number_lines_processed != 3 &&
           $input_line ne "" && $input_line ne "\n" )
         {
         # Pick out current stn
         @line_parts = split /::/, $input_line;

         #----------------------------------------
         # Find Current stn Info from station list
         #----------------------------------------
         @current_stn_parts = split /\./, $line_parts[0];
         $stn_index = $current_stn_parts[0];
         @current_stn_next = split /\(/, $current_stn_parts[1]; 
         $current_stn = $current_stn_next[0];

         $CD_line = $stationCD_lines[$stn_index];

         if ($debug) { print "CD_line: $CD_line\n";}

         $CD_stnID = substr($CD_line, 0, 15);
         $CD_lat = substr($CD_line, 21, 10);
         $CD_lon = substr($CD_line, 32, 11);

         $CD_name = substr($CD_line,54,47);

         $CD_network = substr($CD_line, 142, 4);
         $CD_freq = substr($CD_line, 147, 10);
         $CD_elev = substr($CD_line, 165, 7);

         if ($debug) { print "CD_stnID, CD_lat, CD_lon, CD_name, CD_network, CD_freq, CD_elev:: $CD_stnID, $CD_lat, $CD_lon, $CD_network, xxx $CD_freq xxx, $CD_elev\n";}

###Next line put out StationCD file number preceeding name.
###was:  printf DIST_OUTPUT_FILE "<b><font color=blue>StnFileLineNum.</font>Current_Station (lat/lon location): &nbsp;&nbsp;&nbsp;<font color=blue>$current_stn</font></b><br>\n";

         printf DIST_OUTPUT_FILE "<b>Current_Station: &nbsp;&nbsp;&nbsp;<font color=blue>$current_stn ($CD_lat $CD_lon $CD_elev $CD_freq $CD_network $CD_name)</font></b><br>\n";

         #-------------------------
         # Handle compared stations
         #-------------------------
         @compared_stations = split /,/, $line_parts[1];

         if ($debug) { print "line_parts[0], line_parts[1]:: xxx $line_parts[0] xxx xxx $line_parts[1] xxx\n";}
         if ($debug) { print "current_stn ($current_stn), compared_stations:: @compared_stations\n";}

         undef %stnInfo; # Clean out hash.

         foreach $station_combo (@compared_stations)
            {
            @stn_parts = split /\(/, $station_combo;    # 0=stnid, 1=dist
            @stn_name_parts = split /\./, $stn_parts[0];
            $stn_index = $stn_name_parts[0];    
            $station_name = $stn_name_parts[1];    

            if ($debug) { print "stn_parts, stn_name_parts, stn_index, station_name:: xxx @stn_parts xxx @stn_name_parts xxx $stn_index xxx $station_name xxx\n";}

            #--------------------------------------------
            # Get other stn info from stationCD.out file.
            #--------------------------------------------
            $CD_line = $stationCD_lines[$stn_index];

            if ($debug) { print "stationCD_lines[0]: $stationCD_lines[0]\n";}
            if ($debug) { print "CD_line: $CD_line\n";}

            $CD_stnID = substr($CD_line, 0, 15);
            $CD_lat = substr($CD_line, 21, 10);
            $CD_lon = substr($CD_line, 32, 11);

            $CD_name = substr($CD_line,54,20); # Only take first 20 chars of name. Total is 47 chars.

            $CD_network = substr($CD_line, 142, 4);
            $CD_freq = substr($CD_line, 147, 10);
            $CD_elev = substr($CD_line, 165, 7);

            if ($debug) { print "CD_stnID, CD_lat, CD_lon, CD_name, CD_network, CD_freq, CD_elev:: $CD_stnID, $CD_lat, $CD_lon, $CD_network, $CD_freq, $CD_elev\n";}

            #-----------------
            # Station Distance
            #-----------------
            $station_dist = $stn_parts[1];
            chop($station_dist); chop($station_dist); # Chop off last 2 junk chars

            if ($stn_index[0] ne "[")
               {
               if (exists $stnInfo{$station_dist} )
                  {
                  #-------------------------------------------------------
                  # A station at this distance already exists in the hash.
                  # We have found a colocated station!
                  #-------------------------------------------------------
                  if ($debug) { print "$station_name EXISTS in the stnInfo Hash. DUPLICATE Distance Stns!\n";}

                  $stnInfo{$station_dist}{STNID}    = $stnInfo{$station_dist}{STNID}.
                          "<br><font color=gray>".$station_name." </font>";
                  $stnInfo{$station_dist}{NAME}     = $stnInfo{$station_dist}{NAME}.
                          "<br><font color=gray>".$CD_name." </font>";
                  $stnInfo{$station_dist}{LAT}      = $stnInfo{$station_dist}{LAT}.
                          "<br><font color=gray>".$CD_lat." </font>";
                  $stnInfo{$station_dist}{LON}      = $stnInfo{$station_dist}{LON}.
                          "<br><font color=gray>".$CD_lon." </font>";
                  $stnInfo{$station_dist}{ELEV}     = $stnInfo{$station_dist}{ELEV}.
                          "<br><font color=gray>".$CD_elev." </font>";
                  $stnInfo{$station_dist}{FREQ}     = $stnInfo{$station_dist}{FREQ}.
                          "<br><font color=gray>".$CD_freq." </font>";
                  $stnInfo{$station_dist}{NETWORK}  = $stnInfo{$station_dist}{NETWORK}.
                                                      "<br><font color=gray>".$CD_network." </font>";

                  $stnInfo{$station_dist}{DISTANCE} = $stnInfo{$station_dist}{DISTANCE}.
                                                      "<br><font color=gray>".$station_dist." </font>";
                  }
               else
                  {
                  #--------------------------------
                  # This is a new stn. Add to list.
                  #--------------------------------
                  if ($debug) { print "$station_name with distance $station_dist is NEW ID. Add to stnInfo Hash.\n";}
                  $stnInfo{$station_dist}{STNINDEX} = $stn_index; # Internal stationCD ID - just a count
                  $stnInfo{$station_dist}{STNID}    = $station_name; # True ID
                  $stnInfo{$station_dist}{NAME}     = $CD_name; # 47 char Name
                  $stnInfo{$station_dist}{LAT}      = $CD_lat;
                  $stnInfo{$station_dist}{LON}      = $CD_lon;
                  $stnInfo{$station_dist}{ELEV}     = $CD_elev;
                  $stnInfo{$station_dist}{FREQ}     = $CD_freq;
                  $stnInfo{$station_dist}{NETWORK}  = $CD_network;
                  $stnInfo{$station_dist}{DISTANCE} = $station_dist;

                  } # if exists
               } # if Found line

            } # foreach station

         #--------------------------------------------------
         # Print out this main station's compared stations
         # in "sorted by distance" order.
         #
         # ONLY PRINT OUT THE FIRST few stns, else web page
         # can become too large. See $NUMBER_STNS_TO_PRINT.
         #--------------------------------------------------
         $num = 0;
         $number_printed = 0;

         foreach $DIST (sort keys %stnInfo)
            {
            $num++;

            if ($debug)
               {
               print  "stnInfo:: $DIST - $stnInfo{STNID}  ";
               print "\n";
               }
 
            if ($num == 1) #First is always "Found xx stations within...."
               { 
               print "stnInfo{num==1}:: $stnInfo{$DIST}\n";

               printf DIST_OUTPUT_FILE "&nbsp;&nbsp;&nbsp;<font color=black> $stn_index KM.]</font><br><br>\n"; }
            else
               {
               if ($number_printed < $NUMBER_STNS_TO_PRINT)
                  {
                  $number_printed++;

                  if ($num ==2) 
                     {
                     printf DIST_OUTPUT_FILE "<table width='100%' border='0'>\n";
                     printf DIST_OUTPUT_FILE "<tr>\n";
                     printf DIST_OUTPUT_FILE "<th width=\"15\">StnID</th>\n";
                     printf DIST_OUTPUT_FILE "<th width=\"15\">Distance (KM)</th>\n";
                     printf DIST_OUTPUT_FILE "<th width=\"15\">Lat</th>\n";
                     printf DIST_OUTPUT_FILE "<th width=\"15\">Lon</th>\n";
                     printf DIST_OUTPUT_FILE "<th width=\"15\">Elev</th>\n";
###                     printf DIST_OUTPUT_FILE "<th>Frequency</th>\n";
###                     printf DIST_OUTPUT_FILE "<th>Platform</th>\n";
                     printf DIST_OUTPUT_FILE "<th width=\"20\">Name</th>\n";
                     printf DIST_OUTPUT_FILE "</tr> \n";

                     printf DIST_OUTPUT_FILE 
                       "<tr> <td><b><font color=red>$stnInfo{$DIST}{STNID} </font></b></td> 
                       <td><b><font color=red>$stnInfo{$DIST}{DISTANCE}</font></b></td> 
                       <td><b><font color=red>$stnInfo{$DIST}{LAT}</font></b></td> 
                       <td><b><font color=red>$stnInfo{$DIST}{LON}</font></td> 
                       <td><b><font color=red>$stnInfo{$DIST}{ELEV}</font></b></td> 
                       <td><b><font color=red>$stnInfo{$DIST}{NAME}</font></b> </td> </tr>\n";
                     }
                  elsif ($DIST<=10) 
                    {
                     printf DIST_OUTPUT_FILE "<tr> 
                       <td><b><font color=orange>$stnInfo{$DIST}{STNID}</font></b></td> 
                       <td><b><font color=orange>$stnInfo{$DIST}{DISTANCE}</font></b></td> 
                       <td><b><font color=orange>$stnInfo{$DIST}{LAT}</font></b></td> 
                       <td><b><font color=orange>$stnInfo{$DIST}{LON}</font></td> 
                       <td><b><font color=orange>$stnInfo{$DIST}{ELEV}</font></b></td> 
                       <td><b><font color=orange>$stnInfo{$DIST}{NAME}</font></b> </td> </tr>\n";
                    }
                  elsif ($DIST<=25) 
                    {
                     printf DIST_OUTPUT_FILE "<tr> 
                       <td><b><font color=yellowgreen>$stnInfo{$DIST}{STNID} </font></b></td> 
                       <td><b><font color=yellowgreen> $stnInfo{$DIST}{DISTANCE}</font></b></td> 
                       <td><b><font color=yellowgreen>$stnInfo{$DIST}{LAT}</font></b></td> 
                       <td><b><font color=yellowgreen>$stnInfo{$DIST}{LON}</font></td> 
                       <td><b><font color=yellowgreen>$stnInfo{$DIST}{ELEV}</font></b></td> 
                       <td><b><font color=yellowgreen>$stnInfo{$DIST}{NAME}</font></b> </td> </tr>\n";
                    }
                  elsif ($DIST<=50) 
                    {
                     printf DIST_OUTPUT_FILE "<tr> 
                       <td><b><font color=green>$stnInfo{$DIST}{STNID}</font></b></td> 
                       <td><b><font color=green> $stnInfo{$DIST}{DISTANCE}</font></b></td> 
                       <td><b><font color=green>$stnInfo{$DIST}{LAT}</font></b></td> 
                       <td><b><font color=green>$stnInfo{$DIST}{LON}</font></td> 
                       <td><b><font color=green>$stnInfo{$DIST}{ELEV}</font></b></td> 
                       <td><b><font color=green>$stnInfo{$DIST}{NAME}</font></b> </td> </tr>\n";
                    }
                  elsif ($DIST<=75) 
                    {
                     printf DIST_OUTPUT_FILE "<tr> 
                       <td><b><font color=blue>$stnInfo{$DIST}{STNID}</font></b></td> 
                       <td><b><font color=blue> $stnInfo{$DIST}{DISTANCE}</font></b></td> 
                       <td><b><font color=blue>$stnInfo{$DIST}{LAT}</font></b></td> 
                       <td><b><font color=blue>$stnInfo{$DIST}{LON}</font></td> 
                       <td><b><font color=blue>$stnInfo{$DIST}{ELEV}</font></b></td> 
                       <td><b><font color=blue>$stnInfo{$DIST}{NAME}</font></b> </td> </tr>\n";
                    }
                  elsif ($DIST<=100) 
                    {
                     printf DIST_OUTPUT_FILE "<tr> 
                       <td><b><font color=purple>$stnInfo{$DIST}{STNID} </font></b></td> 
                       <td><b><font color=purple> $stnInfo{$DIST}{DISTANCE}</font></b></td> 
                       <td><b><font color=purple>$stnInfo{$DIST}{LAT}</font></b></td> 
                       <td><b><font color=purple>$stnInfo{$DIST}{LON}</font></td> 
                       <td><b><font color=purple>$stnInfo{$DIST}{ELEV}</font></b></td> 
                       <td><b><font color=purple>$stnInfo{$DIST}{NAME}</font></b> </td> </tr>\n";
                    }
                  else 
                    {
                     printf DIST_OUTPUT_FILE "<tr> 
                       <td><b><font color=black>$stnInfo{$DIST}{STNID} </font></b></td> 
                       <td><b><font color=black>$stnInfo{$DIST}{DISTANCE}</font></b></td> 
                       <td><b><font color=black>$stnInfo{$DIST}{LAT}</font></b></td> 
                       <td><b><font color=black>$stnInfo{$DIST}{LON}</font></td> 
                       <td><b><font color=black>$stnInfo{$DIST}{ELEV}</font></b></td> 
                       <td><b><font color=black>$stnInfo{$DIST}{NAME}</font></b> </td> </tr>\n";
                    } #num or DIST check

                  } # number_printed < $NUMBER_STNS_TO_PRINT - only print first closest stns

               } # num == 1

            } # foreach  distance
   
            printf DIST_OUTPUT_FILE "</table>\n";
            printf DIST_OUTPUT_FILE "<hr><br>\n";

         } # Not a comment line
      } # while data in input file

   print "\nTotal number lines processed in file Closest_stns_LIST_$distanceKM.out: $number_lines_processed\n";

   #-----------------------------
   # Put in standard information.
   #-----------------------------
   printf DIST_OUTPUT_FILE "</HTML>\n";

   close (DIST_INPUT_FILE);
   close (DIST_OUTPUT_FILE);

   } # create_web_page()


##################################################
# form_webPage_output.pl - MAIN processing.
# Example : 
#  form_webPage_output.pl EAOP99EsfcNoShip.ID 20
##################################################
printf "\nform_webPage_output.pl of file $ARGV[0] began on ";print scalar localtime; printf "\n";

#----------------------------------
# Set the output information level.
#----------------------------------
$debug=0;

$station_info_file = $ARGV[0];
$distanceKM = $ARGV[1];

#-----------------------------------------------------
# Call Det_Closest_station for requested AOI km limit.
#-----------------------------------------------------
print "\nCall det_closest_stn $station_info_file $distanceKM.\n";
system ("det_closest_stn $station_info_file $distanceKM");

print "\n mv Closest_stns_LIST.out Closest_stns_LIST_$distanceKM.out\n";
system ("mv Closest_stns_LIST.out Closest_stns_LIST_$distanceKM.out");

#---------------------------------------------
# Create web page from det_closest_stn output.
# Assume form of input/output file names.
#---------------------------------------------
print "Call create_web_page( $station_info_file, $distanceKM ).\n";
create_web_page ($station_info_file, $distanceKM);

printf "\nform_webPage_output.pl of file $ARGV[0] ended on ";print scalar localtime;
printf "\n";
