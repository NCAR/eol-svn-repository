#!/usr/bin/perl 

#-------------------------------------------------------
# create_HQC_bystn_html.pl - This perl script/program reads
#   HQC records (including HQC Extended format) and
#   creates HTML (BY STN) output that can be viewed with a 
#   web browser. Only certain parameters are currently
#   displayed. Use the "TOP" file to navigate other pages.
#
#   Currently, this s/w assumes that a single file contains
#   data for only one day but for many hours. If some hours
#   are missing, the final TOP table may not look good.
# 
# 11 June lec
#   Created. Note that this version of the s/w opens
#   and closes each file as it is needed. Depending on 
#   the project, number of stns, etc., this may be too
#   time consumming. The original version s/w attempted
#   keep files open until no longer needed and ran into
#   the system limit for the NESOB97 project. A better
#   solution to opening and closing the files would be
#   to do "use FileCache; and cacheout ($path); everytime
#   a filehandle is used. The caching will handle wanting
#   to have more files open than the system allows.
#-------------------------------------------------------*/
$debug = 0;

if ($debug)
   {
   open (debug_OUTFILE, ">DEBUG.dbg") ||
     die "Can NOT open debug output file for writing DEBUG.dbg";
   }    

if ($debug) {$open_files = 1;}

#-------------------------------------
# USER MUST SET FOLLOWING ACCORDINGLY!
#-------------------------------------
$project = "ARM/GCIP/NESOB97  (Near Surface Observation Dataset 1997)";
$http_output_location = "http://www.joss.ucar.edu/dpg/NESOB97/HQC/QCFLAG_INFO_BYSTN/TEST2";

$printf, "WARNING: update project name: $project\n";
$printf, "WARNING: http_output_location: $http_output_location\n";

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncreate_HQC_bystn_html.pl began processing $ARGV[0] at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# First argument on the input line for
# this script should be the name of a 
# file containing a list of all the QCF
# day files to be processed.
#----------------------------------------
open (Files_to_Process, "$ARGV[0]") || 
     die "Can NOT open Files_to_Process input file for reading $ARGV[0].html";

if ($debug){$open_files++; printf debug_OUTFILE "First 1Number of open_files = $open_files\n";}

open (TOP_OUTFILE, ">TOP_QC_stn_table.html") || 
     die "Can NOT open TOP output file for writing TOP_QC_stn_table.html";

if ($debug){$open_files++; printf debug_OUTFILE "second 2Number of open_files = $open_files\n";}  


#---------------------------------------------------
# Maintain a list (hash) of all stns encountered in
# all input files (i.e, a project stn list). 
# This hash will contain Network:Stn as the
# key and lat, lon, elev as the content of the hash.
#---------------------------------------------------
%stnInfo = ();        

#------------------------------------------------------
# Maintain a list (hash) of all open Navigation files
# and lower level stn QC flag files, so can close files
# properly. Index both by stnID.
#------------------------------------------------------
%NavgFiles = ();
%QCFlagFiles = ();

#----------------------------------------------------
# Maintain a list (hash) of data seen for each stnID.
#----------------------------------------------------
%LastYYMM = ();
%LastDay = ();

#------------------------------------------------------
# NOTE: Only open lower level "stn" html files for stns
# not yet seen in this project/set of input files.
# Initialize top level variables.
#------------------------------------------------------
$number_files_processed = 0;

$current_year_month = "00/00"; #(YY/MM) Input files are generally day files, not monthly
$current_day = "00";           #(DD)


#-------------------------------------------
# Process all files in input list ($ARG[0]).
#-------------------------------------------
while ($input_file = <Files_to_Process>)
   {
   #-----------------------------------------------------------------
   # Get file name from input list and open that file for processing.
   #-----------------------------------------------------------------
   chop($input_file);

   open (INFILE, "$input_file") || 
        die "Can NOT open ($input_file) input file for reading input_file";

   if ($debug) {printf debug_OUTFILE "\nOPEN FILE: $input_file\n"; }
   if ($debug){$open_files++; printf debug_OUTFILE "3Number of open_files = $open_files\n"; }  

   #--------------------------------------------------------------
   # Initialize vars that generally change within each input file.
   #--------------------------------------------------------------
   $current_date_time = "00/00/00 00:00";
   $stnID = "xxxxxx";

   #----------------------------------
   # Process each record in that file.
   #----------------------------------
   while ($line = <INFILE>) 
      { 
      $number_lines_read++;

      if ($debug) {printf debug_OUTFILE "\nRead line: $line"; }

      #--------------------------------------------
      # Parse each line. Spaces in stn Name prevent 
      # splitting on space in first half of line.
      #--------------------------------------------
      $date_time = substr( $line, 0, 14); #Format: "YY/MM/DD HH:MM"
      $current_date_time = $date_time;    #Format: "YY/MM/DD HH:MM"
 
      $date = substr( $line, 0, 8);       #Format: "YY/MM/DD" 
      $year_month = substr( $line, 0, 5); #Format: "YY/MM"
      $day = substr( $line, 6, 2);        #Format: "DD"
      $hour = substr( $line, 9, 2);       #Format: "HH"

      $net_name = substr( $line, 30, 25); #Example: "ARMSFC1    E25: Seminole"
      @words = split (' ', $net_name);                       #Form a Network:Name w/o spaces
      $stnID = $words[0].":".$words[1].$words[2].$words[3];  #May NOT work in all cases!
      $stnQC_file_name =$input_file."_".$stnID.".html";      #Lowest lvl stn file w/ QC flag info

      $lat = substr( $line, 57, 10);      #Example:  35.24600
      $lon = substr( $line, 68, 11);      #Example: -96.73600
      $elev = substr( $line, 84, 7);      #Example: 277.00

      $stn_info_array[0] = $lat; #Fill the array to be stored in the stnInfo hash table
      $stn_info_array[1] = $lon;
      $stn_info_array[2] = $elev;

      $line_secondhalf =substr( $line, 85, 285 ); #Parameter values and QC flags
      @line_parts = split(' ', $line_secondhalf);


      #----------------------------------------------------------
      # If have NOT seen this stn before in this project (i.e.
      # all input files considered.
      #----------------------------------------------------------
      if (!(exists $stnInfo{$stnID}))
         {
         #-------------------------------------------
         # This is the first time in this input
         # QCF file this stn has been encountered, so
         # open the appropriate HTML output files and
         # write this QCF rec's info into those files.
         #-------------------------------------------

         if ($debug) {printf debug_OUTFILE "\n---->NEVER seen stn $stnID before.\n"; }

         # Save off initial date data seen for this stn, etc.
         $LastYYMM {$stnID} = "00/00";
         $LastDay {$stnID} = "00";

         #-------------------------------------------------------
         #Add this stn to the hash with it's lat/lon/elev info.
         #-------------------------------------------------------
         # At the end of processing this day file, output this
         # hash table as the top level list of all possible stns
         # in this project. THIS IS THE PROJECT'S STN LIST, NOT
         # JUST THE LIST FOR A DAY!!!!!
         #-------------------------------------------------------
         push (@{ $stnInfo{$stnID} }, @stn_info_array); 


         #----------------------------------------------------
         # Open this stn's Yr/Mo/Day navigation file and add
         # in header info. Put everything into a stnID subdir.
         #----------------------------------------------------
         mkdir $stnID, 0777;

         $navgArray[0] = $stnID."/".$stnID."_day_table.html";
         $navgArray[1] = $navgArray[0]."Ptr";

         if ($debug) {printf debug_OUTFILE "navgArray: $navgArray[0], $navgArray[1]\n";}

         open ($navgArray[1], ">$navgArray[0]") ||
              die "Can NOT open stn Navigation output file for writing: $navgArray[0]";   
         if ($debug){$open_files++; printf debug_OUTFILE "4Number of open_files = $open_files\n";}  

         push (@{ $NavgFiles{$stnID} }, @navgArray); 

         printf {$navgArray[1]}  "<HTML>\n <body bgcolor=white>\n
           <TITLE>JOSS Surface Quality Control</TITLE>\n
           <center><H1>JOSS Surface Quality Control</H1></center><hr><br>\n
           <center><H2><b>$project</b></H2></center>\n       

           <H4><b>Dates below contain data for complete project TOI for
           <font color=red>$net_name, Lat $lat, Lon $lon, Elev $elev</font>.
           <font color=black>Select a Date.</font></b></H4>\n 

           <table border=1>\n
           <tr>\n
           <td valign=\"top\" colspan=\"1\" bgcolor=\"lightblue\">
                  <center><b>YEAR/MONTH</b></center></td>\n
           <td valign=\"top\" colspan=\"31\" bgcolor=\"lightblue\"><center><b>DAY</b>\n
           </center> </td>\n </tr> \n\n";
         }
      else
         {
         #-----------------------------------------------------------
         # Pick out the right file ptr to write to for current stnID.
         #-----------------------------------------------------------
         @navgArray = @{ $NavgFiles{ $stnID } };
         open ($navgArray[1], ">>$navgArray[0]") ||
              die "Can NOT open stn Navigation output file for appending: $navgArray[0]"; 

         if ($debug){$open_files++; printf debug_OUTFILE "6Number of open_files = $open_files\n";}  
         }

      #----------------------------------------------------------
      # If we have NOT seen this stn OR the day changed (since
      # we create lowest level day html files.
      #----------------------------------------------------------
      if ( !(exists $stnInfo{$stnID}) || ($LastDay {$stnID} ne $day) )
         {
         #--------------------------------------------------
         # Open this stn's lowest level HTML page and add in
         # header info and first line of QC Flag info.
         #--------------------------------------------------
         $QCFlagFileArray[0] = $stnID."/".$stnQC_file_name;
         $QCFlagFileArray[1] = $QCFlagFileArray[0]."Ptr";

         if ($debug) {printf debug_OUTFILE "\nOPEN QCFLAG $QCFlagFileArray[1].\n"; }

         open ($QCFlagFileArray[1], ">$QCFlagFileArray[0]") ||
              die "Can NOT open stn info output file for writing $QCFlagFileArray[0]";  

         if ($debug)
            {
            $open_files++; 
            printf debug_OUTFILE "open $QCFlagFileArray[0] 7Number of open_files = $open_files\n";
            }

         delete ($QCFlagFiles{$stnID}); #remove then add in new elements.

         push (@{ $QCFlagFiles{$stnID} }, @QCFlagFileArray);

         if ($debug){
            @junk = @{ $QCFlagFiles{$stnID} };
            printf debug_OUTFILE "Current QCFlagFiles{stnID} elements: $junk[0], $junk[1]\n";
            }       


         printf {$QCFlagFileArray[1]} "<HTML>\n <body bgcolor=white>\n
           <TITLE>JOSS Surface Quality Control</TITLE>\n
           <center><H1>JOSS Surface Quality Control</H1></center><hr><br>\n
           <center><H2><b>$project</b></H2></center>\n
           <H3><b><u>KEY:</u> </H3>\n
           <table border cols=2>\n
           <tr>\n
           <th width=100 bgcolor=lightblue>QC Flag</th>\n
           <th width=450 bgcolor=lightblue>Definition</th>\n </tr>\n
           <tr> <th> <font color=green> G, T</font></th> <th>Good, Trace</th></tr>\n
           <tr> <th> <font color=orange> D, E</font></th> <th> Dubious
                 (questionable), Estimated</th></tr>\n
           <tr> <th> <font color=red> B</font></th> <th> Bad (unlikely)</th> </tr>\n
           <tr> <th> <font color=Black> M, U, N, X, I, C</font></th>\n
                <th>Missing, Unchecked, Not Available, Glitch,\n
                Insufficient data to compute derived parameter,\n
                Reported value exceeds output format field size or\n
                was negative precipitation. </th> </tr>\n
           </table> <br> <br> <br> \n
           <H3><b><u>Complete Project data for <font color=red>$date</font> \n
              for station:</u> <font color=red>$net_name, Lat $lat, Lon $lon, \n
              Elev $elev</font></b></H3>\n

           <table border cols=9> <tr bgcolor=lightblue>\n

           <th width=150> Date/Time</th>\n
           <th width=10>stnP</th> <th width=10>SLP</th> <th width=10>CSLP</th>\n
           <th width=10>T</th> <th width=10>Td</th>\n
           <th width=10>Wsp</th> <th width=10>Wdir</th>\n
           <th width=10>Pcp</th> </tr>\n\n";
    
         if ($debug) {printf debug_OUTFILE "\nWrite HDR info into $QCFlagFileArray[0].\n"; }

         } # Have NOT seen this stn in this file (day) before?
      else
         {
         #-----------------------------------------------------------
         # Pick out the right file ptr to write to for current stnID.
         #-----------------------------------------------------------
         @QCFlagFileArray = @{ $QCFlagFiles{ $stnID } };

         if ($debug) {printf debug_OUTFILE 
            "\nBefore open, stnID, QCFlagFileArray[0], QCFlagFileArray[1]: $stnID, $QCFlagFileArray[0], $QCFlagFileArray[1]\n"; }

         open ($QCFlagFileArray[1], ">>$QCFlagFileArray[0]") ||
            die "Can NOT open stn info output file for appending $QCFlagFileArray[0]";     

         if ($debug){$open_files++; printf debug_OUTFILE "open $QCFlagFileArray[0] 8Number of open_files = $open_files\n";}  

         if ($debug) {printf debug_OUTFILE "\nWrite MORE QCFlag data into $QCFlagFileArray[1].\n"; }
         }

      #-------------------------------------------------------
      # Write YR/MO and day info into top Navigation Day file.
      # Link day to lowest level stnQC_file_name file.
      #-------------------------------------------------------
      if ($debug) 
        {
        printf debug_OUTFILE "\ncurrent_year_month, year_month:: $current_year_month, $year_month\n";
        printf debug_OUTFILE 
           "Compare stnID, LastYYMM{stnID} ne year_month:: $stnID, $LastYYMM{$stnID}, $year_month\n";
        printf debug_OUTFILE  "stnID, LastDay{stnID} ne day:: $stnID, $LastDay{$stnID}, $day\n";
        }

     #-------------------------------
     # Has the year or month changed?
     #-------------------------------
     if ($LastYYMM {$stnID} ne $year_month)   #For this stn this is unique
      {
      $LastYYMM {$stnID} = $year_month;
      $current_year_month = $year_month;

      printf {$navgArray[1]} "<tr><td valign=\"top\"> <center>$current_year_month</center></td>\n";

      if ($debug) 
         {printf debug_OUTFILE "\nWRITE $current_year_month to Navigation file: $navgArray[1].\n\n";}
      }

      #--------------------- 
      # Has the day changed? 
      #---------------------
      if ($LastDay {$stnID} ne $day)
         {
         printf {$navgArray[1]} 
            "<td valign=\"top\"><a href=\"$http_output_location\/$QCFlagFileArray[0]\" target=\"_top\">$day</a></td>\n";      

         $LastDay {$stnID} = $day;
         $current_day = $day;

         if ($debug){printf debug_OUTFILE "\nWRITE day ($day) to Navigation file: $QCFlagFileArray[0].\n";}
         }

      close ($navgArray[1]);    # Open and close each time. Could be slow.
      if ($debug){$open_files--; printf debug_OUTFILE "close 9Number of open_files = $open_files";}

      #-----------------------------------------------
      # Write data into lowest level stn QC flag file.
      #-----------------------------------------------
      if ($debug) {printf debug_OUTFILE "\nWRITE QCFlag data for $day into file $QCFlagFileArray[1].\n";}

      printf {$QCFlagFileArray[1]} "<th bgcolor=lightyellow> $current_date_time </th>\n";
  
      for ($ii=2;$ii<=17; $ii=$ii+2)
        {
        if ($debug) {print debug_OUTFILE $ii, $line_parts[$ii],"\n";}

        if ($line_parts[$ii] eq "G" || $line_parts[$ii] eq "T")
           { $color = "green"; }
        elsif ($line_parts[$ii] eq "D" || $line_parts[$ii] eq "E")
           { $color = "orange"; }
        elsif ($line_parts[$ii] eq "B")
           { $color = "red"; }
        elsif ($line_parts[$ii] eq "M" || $line_parts[$ii] eq "U" ||
               $line_parts[$ii] eq "N" || $line_parts[$ii] eq "X" ||
               $line_parts[$ii] eq "I" || $line_parts[$ii] eq "C" )
           { $color = "black"; }
           else
           { $color = "lightblue"; }

        printf {$QCFlagFileArray[1]} "<th ><font color=%-s>%-s</th>", $color, $line_parts[$ii];

        } # for each QC flag             

      printf {$QCFlagFileArray[1]} "</tr>\n";    

      close ($QCFlagFileArray[1]);

      if ($debug){$open_files--; printf debug_OUTFILE "close $QCFlagFileArray[1] 10Number of open_files = $open_files\n";}

      if ($debug) {print debug_OUTFILE "Get Next Record in current file.\n";}

      }  # while data in file


   if ($debug) {print debug_OUTFILE "No more data in $input_file. Close $input_file.\n";}
   close (INFILE);

   if ($debug){$open_files--; printf debug_OUTFILE "close INPUT 11Number of open_files = $open_files\n";}

   #--------------------------------------------------------------
   # Close all the open lower level "day" files - one foreach stn.
   # Terminate them with the final </table></html> tags.
   #--------------------------------------------------------------
   foreach $stnID (sort keys %QCFlagFiles)
      {
      @QCFlagFileArray = @{ $QCFlagFiles{ $stnID } };

      open ($QCFlagFileArray[1], ">>$QCFlagFileArray[0]") ||
         die "Can NOT open stn info output file for appending $QCFlagFileArray[0]";

      if ($debug){$open_files++; printf debug_OUTFILE "open $QCFlagFileArray[0] 99Number of open_files = $open_files\n";}   
       
      if ($debug)
         {
         printf debug_OUTFILE "Write last line of QCFLAG $QCFlagFileArray[1] and CLOSE the file.\n";
         }

      printf  {$QCFlagFileArray[1]} "</tr></table></HTML>"; #Closing all $QCFlagFileArray[1]s

      close($QCFlagFileArray[1]);

      if ($debug){$open_files--; printf debug_OUTFILE "close 12Number of open_files = $open_files\n";}
      }

   printf ("Number of records read: %d for file %-s\n\n", $number_lines_read, $input_file);

   $number_lines_read = 0;
   $number_files_processed++;

   if ($debug) {print debug_OUTFILE "-------> Process NEXT file.<-------\n";}

   } # while files to process


   #----------------------------------------------------
   # Close all the open stn Navgation files.
   # Terminate each with the final </table></html> tags.
   #----------------------------------------------------
   foreach $stnID (sort keys %NavgFiles)
      {
      @navgArray = @{ $NavgFiles{ $stnID } };

      open ($navgArray[1], ">>$navgArray[0]") ||
        die "Can NOT open stn Navigation output file for appending: $navgArray[0]";

      if ($debug){$open_files++; printf debug_OUTFILE "111Number of open_files=$open_files\n";}      

      if ($debug)
         {
         printf debug_OUTFILE "Write last line of NAVG $navgArray[1] and CLOSE the file.\n";
         }
 
      printf  {$navgArray[1]} "</tr></table></HTML>"; #Closing all $navgArray[1]s
      close($navgArray[1]);
      if ($debug){$open_files--; printf debug_OUTFILE "close 13Number of open_files = $open_files\n";}
      }

#-----------------------------------------------------------
# Write out highest level Stn list HTML file. This is a list
# of all the unique stns seen in this project (i.e., all the
# input files.
#-----------------------------------------------------------
if ($debug) {print debug_OUTFILE "Dump TOP_OUTFILE!\n";}

printf TOP_OUTFILE "<HTML>\n <body bgcolor=white>\n
   <TITLE>JOSS Surface Quality Control</TITLE>\n
   <center><H1>JOSS Surface Quality Control</H1></center><hr><br>\n
   <center><H2><b>$project</b></H2></center>\n
   <H3><b>Files below contain all data for single stn sorted by date/time.</b>
   <font color=red>Select a Station.</font></b></H3>\n
   <table border cols=4><tr bgcolor=lightblue>\n
     <th width=200> Network:Station</th>
     <th width=75> Lat</th> <th width=75> Lon</th> <th width=60> Elev</th> </tr>
   \n\n <tr>\n";               

#---------------------------------------------------------------
# Print stnID with stn info to TOP level navigation output file.
#---------------------------------------------------------------
foreach $stnID (sort keys %stnInfo)
   {
   @stn_LatLonElev = @{ $stnInfo{ $stnID } };  
   
   $day_file_name = $stnID."/".$stnID."_day_table.html";

   printf TOP_OUTFILE "<tr><th bgcolor=lightyellow> 
     <a href=\"$http_output_location\/$day_file_name\" target=\"_top\"> 
        $stnID </a></th>
      <th bgcolor=lightyellow> $stn_LatLonElev[0] </th>
      <th bgcolor=lightyellow> $stn_LatLonElev[1] </th>
      <th bgcolor=lightyellow> $stn_LatLonElev[2] </th></tr>\n";
   }

printf TOP_OUTFILE "</tr></table></HTML>";

close(TOP_OUTFILE);
if ($debug){$open_files--; printf debug_OUTFILE "close 14Number of open_files = $open_files\n";}

close(INFILE);
if ($debug){$open_files--; printf debug_OUTFILE "close 15Number of open_files = $open_files\n";}

close(debug_OUTFILE);
if ($debug){$open_files--; printf debug_OUTFILE "Close 16Number of open_files = $open_files\n";}

printf ("Number of files processed: %d\n\n", $number_files_processed);

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "-----create_HQC_bystn_html.pl ended processing $ARGV[0] at $year/$mon/$mday $hour:$min:$sec\n\n";
