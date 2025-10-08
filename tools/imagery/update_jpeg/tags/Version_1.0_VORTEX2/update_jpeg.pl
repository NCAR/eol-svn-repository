#!/usr/bin/perl 

#-------------------------------------------------------------------
# update_jpeg.pl - This perl program updates/converts the Central Daylight
#   Savings Time (CDT) in the jpeg header of an image to be UTC and then
#   renames the file to include the UTC time in the file name
#   along with the original file name. This s/w calls the jhead tool.
#
#   BEWARE!! You can only run this tool once on any set of files!
#     Else you will be doing multiple modifications to the files.
#
#   BEWARE that if a file is not completely processed, it is likely
#     that the suffix of *.JPG will not be changed to be *.jpg.
#     The jhead tool appears to change the suffix to *.jpg automatically.
#
#   BEWARE that the jhead tool must exist in the expected location
#     (i.e., /usr/local/codiac/extra/bin/jhead ). If not, then the
#     user must update the location of jhead in the code. 
#
#   BEWARE that this s/w was developed to process VORTEX2 imagery
#     that was collected in Central Daylight Savings Time zone
#     which is 5 hours off from UTC.  To convert for other time
#     times, search for "jhead -ta+5" below and reset time offset
#     accordingly.
#
#   BEWARE that this s/w changes the header info in ALL the *.JPG
#      images located in the specified directory. 
#      Run with info option set to 1 to verify this.
#
#   Execute: update_jpeg.pl [directory_name] [info_level]
#
#   Required Inputs: Two input parameters are required as shown below:
#
#   where [directory_name] is the name of the directory where all
#   *.JPG images will be modified.
#
#   and [info_level] is either 0 or 1. Level 0 produces minimal
#   information to be printed to screen. Level 1 produces more detailed
#   information to be printed to screen. The default info_level is 0.
#
#   Required Inputs: Two input parameters are required as shown above.
#
#   To process all *.JPG files in the current directory with minimal output
#   and to save all text and error messages to a file named output.txt 
#   while excuting as a background process, run the following command:
#           "update_jpeg.pl . 0 >& output.txt &"
#
#   Output:
#     This s/w prints Informational Messages to the screen.  
#     When a file is processed correctly both the date/time inside
#       the file will be updated to be UTC and the file name will
#       be changed to include the UTC time. Here's an example of
#       the messages when both jhead cmds work properly (next 3 lines):
# --------------------->>> Processing file ./cama-2009-06-05-17-19-29.JPG.
# Modified: ./cama-2009-06-05-17-19-29.JPG
# cama-2009-06-05-17-19-29.JPG --> cama-2009-06-05-17-19-29_20090606081929.jpg
#
#     This s/w changes the name of the input file to be as follows:
#
#       [origFileName]_[UTCdateTime].jpg
#
#     where origFileName is the name of the file being processed,
#     UTCdateTime will the collected image time converted to UTC
#     and in the form of YYYYMMDDHHMMSS. Note that jhead automatically
#     updates the suffix to be "jpg".
#--------------------------------------------------------------------
# Jan 2011 lec
#   Created Version 1.0.
#------------------------------------------------------------------------
printf "\nupdate_jpeg.pl of directory $ARGV[0] began on ";print scalar localtime; printf "\n";

#----------------------------------
# Set the output information level.
#----------------------------------
$info = 0; # default
$number_files_processed = 0;

if ($ARGV[1] == 0) {$info = 0;}
if ($ARGV[1] == 1) {$info = 1;}

#--------------------------------------------------
# Process all jpeg image files in the requested dir
#--------------------------------------------------
$directory = $ARGV[0];

if ($info == 1) { print "Open directory:: xxx $directory xxx \n";}

opendir (DIR, $directory) or die "Can't opendir $directory \n";
@files = grep { /\.JPG$/ } readdir(DIR);
closedir(DIR);

if ($info == 1) {printf "\nProcessing files:: @files\n";}

foreach $image_file (@files)
   {
   $number_files_processed++;    

   $image_file = $directory . "/" . $image_file;

   print "\n--------------------->>> Processing file $image_file.\n"; 

   if ($info == 1) 
       { 
       print "Original Image File $image_file Info.\n"; 
       system "/usr/local/codiac/extra/bin/jhead $image_file";
       print  "--->> Change time to UTC for $image_file\n "; 
       }

    #-------------------------------------------------
    # Use the jhead tool to convert times to UTC.
    # Testing jhead shows that it correctly rolls over
    # hours, days, and months. Year roll over was not
    # tested. Only the 24 hrs clock (0-23) was tested.
    #
    # BEWARE: This does change the actual file info!!!
    #-------------------------------------------------
    system "/usr/local/codiac/extra/bin/jhead -ta+5 $image_file";

    if ($info == 1)
       {
       print "\nTime has been changed to UTC in file $image_file. \nNew File info::";
       system "/usr/local/codiac/extra/bin/jhead $image_file";   # Dump a listing of all header info
       print "--->> Update file name to include UTC time.\n";
       }

    #-------------------------------------------------
    # Use jhead tool to rename a file.
    #
    # BEWARE: Doing this changes the actual file info.
    # Consider using mv cmd instead.
    #----------------------------------------------------------------------------
    # /usr/local/codiac/extra/bin/jhead -n$image_file_%Y%m%d%H%M%S  $image_file";
    #---------------------------------------------------------------------------- 

    if ($directory eq ".") {substr($image_file, 0, 2) = ""; } # Strip "./" 

    @fileNameParts = split(/\./, $image_file);

    $new_image_name = $fileNameParts[0] . "_" . "%Y%m%d%H%M%S";

    if ($info == 1) { print "fileNameParts[0] = $fileNameParts[0]\n"; print "fileNameParts[1] = $fileNameParts[1]\n"; print "new_image_name = $new_image_name\n"; }

    system "/usr/local/codiac/extra/bin/jhead -n$new_image_name  $image_file";

    } # foreach file


   if ($info == 1)
       {
       print "\n------------------------------\n------------------------------\n";
       print "\nInfo for all Renamed Files::\n\n";
       system "/usr/local/codiac/extra/bin/jhead *.jpg";
       print "\n------------------------------\n------------------------------\n";
       }

   #------------------------------------------------------------------
   # Ensure all files were processed and report those that had issues
   # i.e., files that were not renamed with the *.jpg suffix. 
   # BEWARE that some files may be processed partially and retain
   #    the *.JPG suffix..
   #------------------------------------------------------------------
   print "\n----->>> Check the following files for possible processing ERRORS. <<<-----\n\n";
   system "ls -al --lcontext *.JPG";
   print "\n----- end check file list  -----\n";

print "\nTotal number files processed in directory $ARGV[0]: $number_files_processed\n";
printf "\nupdate_jpeg.pl of file $ARGV[0] ended on ";print scalar localtime;
