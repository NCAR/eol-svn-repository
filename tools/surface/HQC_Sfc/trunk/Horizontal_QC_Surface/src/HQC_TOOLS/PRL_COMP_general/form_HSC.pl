#!/usr/bin/perl
#-----------------------------------------------------------------
# form_HSC.pl - Forms Hourly Surface Composite using PERL.
#
# Open and read all the dirs that contain Hrly Sfc data.
# This reading gets dir listings for each directory that
# can be used below. See the WARNING below.
#
# 23 Jan 98 LEC
#   Created.
#-----------------------------------------------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nform_HSC.pl began at $year/$mon/$mday $hour:$min:$sec\n";

opendir ARMSFC_dir,  "../../ARMSFC/data" or die "Can't open ARMSFC dir: $!";
@ARMSFC_files = grep !/^\./, readdir ARMSFC_dir;
closedir ARMSFC_dir;

opendir ASOS_dir,  "../../ASOS/data" or die "Can't open ASOS dir: $!";
@ASOS_files = grep !/^\./, readdir ASOS_dir;
closedir ASOS_dir;

opendir COAG_dir,  "../../COAG/data" or die "Can't open COAG dir: $!";
@COAG_files = grep !/^\./, readdir COAG_dir;
closedir COAG_dir;

opendir HPCN_dir,  "../../HPCN/data" or die "Can't open HPCN dir: $!";
@HPCN_files = grep !/^\./, readdir HPCN_dir;
closedir HPCN_dir;

opendir MOCAWS_dir,  "../../MOCAWS/data" or die "Can't open MOCAWS dir: $!";
@MOCAWS_files = grep !/^\./, readdir MOCAWS_dir;
closedir MOCAWS_dir;

opendir MODOC_dir,  "../../MODOC/data" or die "Can't open MODOC dir: $!";
@MODOC_files = grep !/^\./, readdir MODOC_dir;
closedir MODOC_dir;

opendir NCDCSAO_dir,  "../../NCDCSAO/data" or die "Can't open NCDCSAO dir: $!";
@NCDCSAO_files = grep !/^\./, readdir NCDCSAO_dir;
closedir NCDCSAO_dir;

opendir NMSU_dir,  "../../NMSU/data" or die "Can't open NMSU dir: $!";
@NMSU_files = grep !/^\./, readdir NMSU_dir;
closedir NMSU_dir;

opendir WPNSFC_dir,  "../../WPNSFC/data" or die "Can't open WPNSFC dir: $!";
@WPNSFC_files = grep !/^\./, readdir WPNSFC_dir;
closedir WPNSFC_dir;

#-------------------------------
# Check the number of day files.
#-------------------------------
print "There are $#ARMSFC_files in ARMSFC.\n";
print "There are $#ASOS_files in ASOS.\n";
print "There are $#COAG_files in COAG.\n";
print "There are $#HPCN_files in HPCN.\n";
print "There are $#MOCAWS_files in MOCAWS.\n";
print "There are $#MODOC_files in MODOC.\n";
print "There are $#NCDCSAO_files in NCDCSAO.\n";
print "There are $#NMSU_files in NMSU.\n";
print "There are $#WPNSFC_files in WPNSFC.\n";

if ( $#ARMSFC_files != $#ASOS_files ||
     $#ASOS_files   != $#COAG_files ||
     $#COAG_files   != $#HPCN_files ||
     $#HPCN_files   != $#MOCAWS_files ||
     $#MOCAWS_files != $#MODOC_files ||
     $#MODOC_files  != $#NCDCSAO_files ||
     $#NCDCSAO_files != $#NMSU_files ||
     $#NMSU_files   != $#WPNSFC_files )
   {
   print "BEWARE: Number of files in each dir does NOT match!\n";
   }
   
#-----------------------------------------------------------------
# WARNING: You will see if you print the following listings
#          (i.e, print "@NCDCSAO_files\n";) 
#          that the order of the file names may NOT be as expected
#          say in alpha order.  Must check this carefully.
#
# Use print "@ARMSFC_files[$#ARMSFC_files]\n"; to print last 
# element of the array; +1 gives length.
#-----------------------------------------------------------------
# Form each day of the final HSC composite - ready for HQC.
# Loop off the most complete set of day/data. That will ensure
# that the greatest number of day files are created.
#-----------------------------------------------------------------
use File::Copy;

foreach $ARMfile (@ARMSFC_files)
   {
   print "ARMfile = $ARMfile\n";
   
   copy ("../../ARMSFC/data/$ARMfile", "input.0qc");

   #---------------------------------
   # Get the date from the file name.
   #---------------------------------
   $date = substr ($ARMfile, 7, 6);
   printf "Processing current date =: xxx%sxxx\n", $date;

   system "cat ../../ASOS/data/ASOSH_$date.0qc >> input.0qc";
   system "cat ../../COAG/data/COAG_$date.0qc >> input.0qc";
   system "cat ../../HPCN/data/hplains_$date.0qc >> input.0qc";
   system "cat ../../MOCAWS/data/MOCAWS60_$date.0qc >> input.0qc";
   system "cat ../../MODOC/data/MODOC60_$date.0qc >> input.0qc";
   system "cat ../../NCDCSAO/data/NCDC60_$date.0qc >> input.0qc";
   system "cat ../../NMSU/data/NMSU60_$date.0qc >> input.0qc";
   system "cat ../../WPNSFC/data/WPN_$date.0qc >> input.0qc";

   system "cosort final_HSC_HQC_sort";

   system "mv input.0qc.sort $date.0qc";
   }

#-------------------------------------
# Remove the left over input.0qc file.
#-------------------------------------
system "/bin/rm  input.0qc";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "form_HSC.pl ended at $year/$mon/$mday $hour:$min:$sec\n";
