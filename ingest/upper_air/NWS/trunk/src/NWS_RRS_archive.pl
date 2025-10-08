#! /usr/bin/perl -w

##Module------------------------------------------------------------------
# <p>The NWS_RRS_archive.pl script is a script that moves the RRS buffer
# files from the ingest directory to the local archive directory and then
# inventories the soundings.  The files are archived by year and month.</p>
# <p>The inventory of the soundings tests to make sure that there are not
# any missing ascension numbers, that the nominal date in the file name is
# the exptected nominal date from the actual release datetime in the file
# header, and that the station information (id,WMO,WBAN) is the same throughout
# all of the files with the same WBAN number in the filename.</p>
#
# @author Linda Echo-Hawk 4/2012
# @ version 2.5 <ul>
#   <li>Updated incoming file location from /net/ingest/ncdc to 
# /net/ftp/pub/data/incoming/ncdc_soundings.  NOTE:  Documentation and 
# some functions will still refer to the "ingest" area.  The area for 
# incoming files has moved from /net/ingest to the new /net/ftp location.
# </li> </ul>
#
# @author Linda Cully 12/2010
# @version 2.5 <ul>
#   <li>Continued to updated error log messages to be more informative.
# A serious look at this code is
# needed when time is available. This software should not simply fail
# upon error without more informative messages. Plus, in some cases,
# the software should continue and simply issue a warning message.
# This has been discussed with S. Loehrer.
# </li> </ul>
#
# @author Linda Cully 08/2008
# @version 2.4 <ul>
#   <li>Update the script to include debug and the ability to shut off
# email to support testing purposes. See $debug and $send_email parameters.
# Improved Error log messages to be more informative. Added white space
# and comment lines to clarify and increase readability. Added code to
# catch uncaught errors, particularly for zero length files. This can
# happen during the grabbufr and decoding (Fortran) processing. Note that this 
# script calls those Fortran routines which are black box type s/w supplied
# by NCDC. Most output errors are now also being written to "RRS_inventory.lock" 
# file as well as to email message to monitors. So if process quits unexpectedly,
# the user can look at the "lock" file for some debug. Search for the word BEWARE.
# Updated HTML code to indicate what "red" numbers on HTML chart indicate.
# </ul>
#
# @author Joel Clawson 02/01/2008
# @version 2.3 <ul>
#   <li>Update the script to store all extracted text files from the buffer
# files instead of just the meta data file.  They are put into the rrs_text
# directory within the local archive.</li>
#   <li>Altered the script to run as a cron script.  This stores any problem
# in an error report and sends a resulting email to the monitors if there were
# any problems.  Because of this, the status messages informing the user what
# the script is current doing have been commented out.</li>
#   <li>Added a 15 minute file modification limit to prevent files that are
# in the process of being FTP'd to the server from being archived before they
# are finished.</li>
#   <li>Added a lock file to prevent multiple versions of the script from
# running at the same time and becoming confused by attempting to do the same
# thing on the same set of files.</li>
# </ul>
#
# @author Joel Clawson 02/16/2007
# @version 2.2 <ul>
#   <li>Removed the ability to handle multiple soundings at the same time for
# a site.  This is the result of an email from the source that states that all
# new files are replacements for exisiting one.</li>
#   <li>Added the archival of the meta data files for each sounding.  This allows
# the script to run faster if it does not have to decode all of the binary
# files for a month to be inventoried.  This also checks the modification time
# of the meta data file and the binary file to force the meta data file to be
# extracted if the binary file has been updated.</li>
# </ul> 
#
# @author Joel Clawson 01/30/2006
# @version 2.1 <ul>
#   <li>Renamed the inventory directory to ../docs/inventory from ../inventory
#   <li>Renamed the archive directory to ../archive/rrs_files from ../rrs_files
#   <li>Added a pre-execution check to determine the OSTYPE of the system running
# the script and assigned the buffer extraction software accordingly.
#   <li>Changed the buffer extraction checks to work on both linux and solaris
# platforms.
# </ul>
#
# @author Joel Clawson 01/27/2006
# @version 2.0 <ul>
#   <li>Upgraded to automatically handle files that older than the processing 
# year and month.  Instead of generating a log, it checks for differences and 
# if the file is different gives it adds an increased count value to the file 
# name.
#   <li>Enhanced the individual sounding inventory to handle multiple sounding
# files.  (i.e. 99999_2006012600.gz 99999_2006012600.1.gz
#   <li>Added a missing check to ensure that the WBAN in the station name is in
# the station list as an RRS station.
#   <li>Added gzip compression to the archived files.
#   <li>Removed the log file creation.
#   <li>Changed the module version to Version5.
# </ul>
#
# @author Joel Clawson 01/12/2006
# @version 1.1 <p>Updated the script to store the files in the archive 
#    directory by year/month/id instead of by year/month.  Also added in
#    the reads from the station list to tell the script which stations
#    should appear in the inventory pages.</p>
# 
# @author Joel Clawson 12/15/2005
# @version 1.0 Created the script to archive and inventory the RRS buffer
#    files.
##Module------------------------------------------------------------------
use strict;
use lib "/net/work/lib/perl/Utilities";
use lib "/work/lib/perl/Utilities";
use DpgDate;

##use Email::MIME;
##use Email::MIME::Creator;

use File::Copy;
# Dump to see hash contents
use Data::Dumper; 

#----------------
# Other Constants
#----------------
my $debug = 0;       # BEWARE debug
my $debug_test_data = 0;
my $send_email = 0;  # BEWARE send_email

$! = 1;

#----------------------------------------------------------
# Determine where the extractors are located on the system.
#----------------------------------------------------------
if (-e "/net/work") {
   chdir("/net/work/operational/sounding/NWS/bin");
} else {
   chdir("/work/operational/sounding/NWS/bin");
}

#---------------------
# Directory contstants
#---------------------
# my $INGEST_DIR = "/ingest/ncdc";
my $INGEST_DIR = "/net/ftp/pub/data/incoming/ncdc_soundings";
my $IVEN_DIR = "../docs/inventory";
my $ARCHIVE_DIR = "../archive/rrs_files";
my $META_ARCHIVE_DIR = "../archive/rrs_text";
my $WORK_DIR = "../rrs_work";

#-------------------------------------------
# Constants used for sending email messages.
#-------------------------------------------
my @MONITORS = ("loehrer\@ucar.edu", "cully\@ucar.edu");
###my @MONITORS = ("cully\@ucar.edu");

my $ERROR_REPORT = "";

#------------------------------------------------------------------------
# Set the Unbuffering commands according to the Operating System type.
# The original code is a "black" box set of Fortran code from NCDC which
# is the data source. Beware that the Linux version requires that the
# LD_LIBRARY_PATH be set to a specific library. See below.
#------------------------------------------------------------------------
my ($GRABBUFR,$DECODER);
if (!defined($ENV{"OSTYPE"}) || $ENV{"OSTYPE"} eq "linux") {

    $GRABBUFR = "/net/work/bin/extractors/NWS_RRS/grabbufr_linux";
    $DECODER = "/net/work/bin/extractors/NWS_RRS/decoder_linux";

    # $INGEST_DIR = "/net/ingest/ncdc";
    $INGEST_DIR = "/net/ftp/pub/data/incoming/ncdc_soundings";

    if ($debug) { printf("Running on a Linux machine. $GRABBUFR, $DECODER\n"); }

    #--------------------------------------------------
    # Need to set up some special paths so it can 
    # final all of the correct linux fortran libraries.
    #--------------------------------------------------
    if (!defined($ENV{"LD_LIBRARY_PATH"})) { $ENV{"LD_LIBRARY_PATH"} = ""; }
    $ENV{"LD_LIBRARY_PATH"} = sprintf("/usr/local/lf9562/lib:%s",$ENV{"LD_LIBRARY_PATH"});

} elsif ($ENV{"OSTYPE"} eq "solaris") {

    $GRABBUFR = "/net/work/bin/extractors/NWS_RRS/unixbufr_solaris";
    $DECODER = "/net/work/bin/extractors/NWS_RRS/decoder_solaris";

    if ($debug) { printf("Running on a Solaris machine. $GRABBUFR, $DECODER\n"); }



} else {
    $ERROR_REPORT .= sprintf("OSTYPE: %s does not have unbuffering commands defined!\n",$ENV{"OSTYPE"});
    if ($debug)
       {
       printf("ERROR_REPORT::\n$ERROR_REPORT\n");
       }
}

# Linux systems currently require /net/ infront of dir names.
if (!(-e "/net/work")) {
    $GRABBUFR =~ s/^\/net//;
    $DECODER =~ s/^\/net//;
}

#-------------------------------------------------------------------
# A lock file to prevent multiple copies of the script from running.
#-------------------------------------------------------------------
my $LOCK_FILE = "../RRS_inventory.lock";
my $LOCK;

#-------------------------------------------------------------------
#my $GRABBUFR = "../RRS/bin/grabbufr";  # Joel commented this out.
#my $DECODER  = "../RRS/bin/decoder";   # Joel commented this out.
#-------------------------------------------------------------------

#---------------------------------------------------------------------------
# Station Information constants. Note that station list file requires
# updates periodically. See the Plone and web documentation for processing
# the NWS data. Note the "Typical Errors" section of the Plone sounding doc.
#---------------------------------------------------------------------------
my $wbans;
my $stnids;
my $STN_FILE = "../docs/NWS_station.list";

&main();

##------------------------------------------------------------------------
# @signature void main()
# <p>Run the script to archive the RRS files and create inventory files
# for them.</p>
##------------------------------------------------------------------------
sub main {
    if ($debug) {printf("Enter Main\n"); }

    if (!(@ARGV == 0 || @ARGV == 2)) {
	printf("Usage: NWS_RRS_archive.pl - archive and inventory\n");
	printf("Usage: NWS_RRS_archive.pl MM YYYY - reinventory specified month\n");
	exit(1);
    }

    #-----------------------------------------------------------------
    # Only try to run the script if an instance isn't already running.
    #-----------------------------------------------------------------
    if (-e $LOCK_FILE) {
        #---------------------------------------------------------------
	# Either another version of the script is running or the last
        # run generated errors, warnings, or other messages....so notify
        # the user that the script is trying to run multiple instances.
        #---------------------------------------------------------------
        printf ("WARNING: File RRS_inventory.lock already exists. Look at the lock file RRS_inventory.lock and pass messages, warnings, errors to S. Loehrer or L. Cully. Once the lock file contents have been reviewed, delete the lock file and rerun this script. Aborting processing.\n");
        exit(1);

    } else { 
        #--------------------------------------------------
        # Lock file does NOT exist. Process the data files.
        #-----------------------------------------------------------------------
	# Generate the lock file to prevent another instance from being created.
        #-----------------------------------------------------------------------
	open($LOCK, ">$LOCK_FILE") or die("Can't create lock file.");

	printf($LOCK "Script NWS_RRS_archive.pl started.  Creating file RRS_inventory.lock to lock all future copies of this process from running.\n");

        if ($debug) { printf ("Main:: Call load_station_list().\n");}
	printf($LOCK "Call load_station_list()\n");
	load_station_list();
	
	my $dates;
	if (@ARGV == 0) {
            if ($debug) { printf ("Main:: Call archive_RRS().\n"); }
	    printf($LOCK "Call archive_RRS()\n");
	    $dates = archive_RRS();
	} else {
            if ($debug) { printf ("Main:: Only archive specific set of data.\n"); }
	    printf($LOCK "Only process specific set of dates.\n");
	    $dates->{sprintf("%04d",$ARGV[1])}->{sprintf("%02d",$ARGV[0])} = 1;

	} # open lock file

        #----------------------------------------
        # Inventory the /ingest/ncdc FTP'd files.
        #----------------------------------------
        if ($debug) { printf ("Main:: Call create_inventory_files().\n"); }
        printf($LOCK "Call create_inventory_files()\n");
	create_inventory_files($dates);

        #---------------------------------------------------------
        # Send email to contacts if error report log is not empty.
        #---------------------------------------------------------
        if ($send_email)
           {
           if ($debug) { printf ("Main:: Prepare and send email to contacts.\n"); }

           # Send an email if there are errors to report.
           if ($ERROR_REPORT ne "") {
               $ERROR_REPORT = "There were some issues running the RRS Inventory script.\n\n" . $ERROR_REPORT;
               if ($debug) { printf ("Main:: There were some issues running the RRS Inventory script.\n"); }

               # Create the body of the message
###               my @parts = (Email::MIME->create(attributes => { content_type => "text/plain" }, body => $ERROR_REPORT));

               # Create the final email message
###               my $email = Email::MIME->create(parts => [ @parts ]);

               # Generate the header
###               $email->header_set("From" => $MONITORS[0]);
###               $email->header_set("Reply_to" => $MONITORS[0]);
###               $email->header_set("To" => join(",", @MONITORS));
###               $email->header_set("Subject" => "NCDC NWS RRS Inventory Script");

               # Send the message.
###               my $SENDMAIL;
###               open($SENDMAIL, "|/usr/lib/sendmail -t") || return ("Unable to open sendmail.");
###               printf($SENDMAIL $email->as_string());
###               close($SENDMAIL);
           } # if ERROR_REPORT is not blank

          } # if send_email

         #-----------------------------------------------------------------------------
         # Delete the lock file now that the script has finished with the inventorying.
         # But only if there weren't any processing errors or warnings!
         #-----------------------------------------------------------------------------
         if ($ERROR_REPORT eq "") {
            close($LOCK) if defined $LOCK;

            if ($debug) { printf ("Main:: Delete the lock file, if possible.\n"); }
            unlink($LOCK_FILE) or $ERROR_REPORT .= "The lock file could not be deleted!\n";
            }
         else
            {
            #---------------------------------------------------------------------------------
            # There were errors, so dump them to the lock file and don't delete the lock file!
            #---------------------------------------------------------------------------------
            printf($LOCK $ERROR_REPORT); # Write error report to Lock file.

            printf($LOCK "Exit Main()\n");
            if ($debug) { printf ("Exit Main\n"); }
            }

         if ($debug){printf("Main(): ERROR_REPORT::\n$ERROR_REPORT\n"); }

    } # If lock file does NOT exists. Process data files.

}   # Main()

##------------------------------------------------------------------------
# @signature HashRef archive_RRS()
# <p>Reads in the RRS files in the ingest directory and moves them into the
# directory that archives the files.</p>
# 
# @output $dates The references of the months that had files that were copied
# to the archive directory.
##------------------------------------------------------------------------
sub archive_RRS {
    if ($debug) { printf ("Enter archive_RRS\n"); }

    if ($debug) { printf ("archive_RRS:: Call read_ingest_for_RRS().\n"); } 
    my @files = read_ingest_for_RRS();
    my $dates = {};

    foreach my $file (@files) {
	$file =~ /(\d{5})_(\d{4})(\d{2})/;
	$dates->{$2}->{$3} = 1;
	my $id = $wbans->{$1}->{"id"};

        #------------------------------------------------------------------
	# Make sure the station exists (Can't create directory without it!)
        #------------------------------------------------------------------
	if (!defined($id)) {
	    $ERROR_REPORT .= sprintf("Station $1 not found as an RRS station in the station list for $file.  Skipping...\n");
            printf($LOCK "Station $1 not found as an RRS station in the station list for $file. Skipping...\n");
	    next;
	}

        #----------------------------------------
	# Define and create the archive directory
        #----------------------------------------
	my $archive_dir = sprintf("%s/%04d/%s/%02d",$ARCHIVE_DIR,$2,$id,$3);
	create_directory($archive_dir) unless (-e $archive_dir);

        #--------------------------------------------------------------------------------------
	# Only archive the RRS file if enough time has passed since it was FTP'd to our server.
        #--------------------------------------------------------------------------------------
	next if (!enough_time_from_ftp($INGEST_DIR, $file));

        #---------------------------------
	# Make sure the file is compressed
        #---------------------------------
	$file = gzip($INGEST_DIR,$file) if ($file !~ /\.gz$/i);

        #---------------------------------------
	# Pull out the base file name and suffix
        #---------------------------------------
	$file =~ /^(\d{5}_\d{10})(\.\d+)?(.+)?$/;
	my ($file_date,$file_suffix) = ($1,$3);

        #-----------------------------------------------------
	# Get the list of files that contain the same sounding
        #-----------------------------------------------------
	opendir(my $ARCH,$archive_dir) or die("Cannot read archive directory: $archive_dir\n");
	my @like_files = grep(/^$file_date/,readdir($ARCH));
	closedir($ARCH);


        #-----------------------------------
        # This section commented out by Joel.
        #-----------------------------------
	# From an email from the source, all files that come from the
	# source that have already been received are replacements.
        #
	# Compare the specified file with the archived files.
	#if (scalar(@like_files) > 0) {
        #	    
	#    # Try to find exact matches to the file.
	#    my $found = 0;
	#    foreach my $like_file (@like_files) {
	#	if (system(sprintf("diff %s/%s %s/%s",$INGEST_DIR,$file,$archive_dir,$like_file)) == 0) {
	#	    $found = 1;
	#	    last;
	#	}
	#    }
	#    
	#    # Copy the file with an incremented count if the specified file was not in the archive.
	#    if (!$found) {
	#	my $new_file = sprintf("%s.%d%s",$file_date,scalar(@like_files),$file_suffix);
	#	copy(sprintf("%s/%s",$INGEST_DIR,$file),sprintf("%s/%s",$archive_dir,$new_file)) or
	#	    die("Cannot copy $INGEST_DIR/$file to $archive_dir/$new_file\n");
	#    }
        #
	#    # Remove the file from ingest
	#    unlink(sprintf("%s/%s",$INGEST_DIR,$file)) or die("Cannot delete file: $INGEST_DIR/$file\n");
	#}
	# No files to compare, so just copy it over to the archive
	#else {
        #--------------------------------------------------------------------
        
	    my $new_file = sprintf("%s%s",$file_date,$file_suffix);
	    my $error = 0;
	    copy(sprintf("%s/%s",$INGEST_DIR,$file),sprintf("%s/%s",$archive_dir,$new_file)) or $error = 1;

	    if ($error == 1) {
		$ERROR_REPORT .= "Cannot copy $INGEST_DIR/$file to $archive_dir/$new_file\n";
                printf($LOCK "Cannot copy $INGEST_DIR/$file to $archive_dir/$new_file\n");
	    } else {
                chmod(0664, sprintf("%s/%s", $archive_dir, $new_file));
                system(sprintf("chgrp cds %s/%s", $archive_dir, $new_file)); 

                #----------------------------
	        # Remove the file from ingest
                #----------------------------
	        unlink(sprintf("%s/%s",$INGEST_DIR,$file)) or die("Cannot delete file: $INGEST_DIR/$file\n");
            }
	#}
    }

    if ($debug) { printf ("Exit archive_RRS.\n"); } 
    return $dates;
} # archive_RRS()

##------------------------------------------------------------------------
# @signature void create_directory(String path)
# <p>Create the directory structure specified in the path.</p>
#
# @input $path The path to be created.
# @warning This function will die if it cannot make a directory.
##------------------------------------------------------------------------
sub create_directory {
    my $path = shift;
    my @dirs = split(/\//,$path);
    my $count = 1;
    my $accum_dir = $dirs[0];

    while ($count < scalar(@dirs)) {
        $accum_dir = sprintf("%s/%s",$accum_dir,$dirs[$count]);
        if (!(-e $accum_dir)) {
            mkdir($accum_dir) || die("Cannot create $accum_dir\n");
	    chmod(0755, $accum_dir);
            #printf($LOG "Created directory: %s\n",$accum_dir); # Commented out by Joel.
        }
        $count++;
    }
} # create_directory()

##------------------------------------------------------------------------
# @signature void create_HTML(int year, int month, Hash* data)
# <p>Create the HTML inventory page for the specified month and year for
# the data in the hash.</p>
#
# @input $year The year of the inventory page.
# @input $month The month of the inventory page.
# @input $data The reference to the hash of data to be inventoried.
##------------------------------------------------------------------------
sub create_HTML {
    if ($debug) { printf ("Enter create_HTML.\n"); }

    my ($year,$month,$data) = @_;
    my ($mnt,$day_count) = get_month_data($month,$year);

    #--------------------------------------------------------------
    # Only create the inventory file if there is data to create it.
    #--------------------------------------------------------------
    if (!(keys(%{ $data}))) {
	$ERROR_REPORT .= sprintf("No RRS data for %02d %04d.  Not creating inventory.\n",$month,$year);
        printf($LOCK "No RRS data for $month $year.  Not creating inventory.\n");
	return;
    }

    create_directory(sprintf("%s/%04d",$IVEN_DIR,$year));

    open(my $HTML,sprintf(">%s/%04d/NWS_rrs_inventory_%04d%02d.html",$IVEN_DIR,$year,$year,$month)) or
	die("Can't create the HTML inventory for $mnt $year\n");

    printf($HTML "<html>\n");
    printf($HTML "<head>\n");
    printf($HTML "   <title>NWS: RRS Inventory: %s %04d</title>\n",$mnt,$year);
    printf($HTML "   <style>\n");
    printf($HTML "<!--\n");
    printf($HTML "      a { font-weight: bold; padding: 0 1em; }\n");
    printf($HTML "      body { font-family: sans-serif; }\n");
    printf($HTML "      font.miss { color: #FF0000; font-weight: bold;  }\n");
    printf($HTML "      th { background-color: #660000; color: white; }\n");
    printf($HTML "      tr.oddstation { background-color: #CCCCFF; color: #000066; }\n");
    printf($HTML "      tr.evenstation { background-color: #CCFFCC; color: #006600; }\n");
    printf($HTML "      tr.oddstation td.data { background-color: #000066; color: white; }\n");
    printf($HTML "      tr.evenstation td.data { background-color: #006600; color: white; }\n");
    printf($HTML "      tr.oddeven { background-color: #CCCCFF; color: black }\n");
    printf($HTML "      tr.oddodd { background-color: #FFFFFF; color: black }\n");
    printf($HTML "      tr.eveneven { background-color: #CCFFCC; color: black }\n");
    printf($HTML "      tr.evenodd { background-color: #FFFFFF; color: black }\n");
    printf($HTML "-->\n");
    printf($HTML "   </style>\n");
    printf($HTML "</head>\n\n");
    printf($HTML "<body>\n");

    printf($HTML "<h1 align=\"center\">NWS: RRS Inventory: %s %04d</h1>\n",$mnt,$year);
    printf($HTML "<h4 align=\"center\">Hours shown in <font color=red>red</font> indicate previous Acsension Number is missing (i.e., Ascension Numbers are non-sequential).</h4>\n");

    my ($prev_year,$prev_month) = get_previous_month($year,$month);
    my ($next_year,$next_month) = get_next_month($year,$month);


    printf($HTML "<table align=\"center\" border=1>\n");
    printf($HTML "   <tr>\n");
    printf($HTML "   <td align=\"left\" colspan=5><a href=\"../%04d/NWS_rrs_inventory_%04d%02d.html\">Previous Month</a></td>\n",$prev_year,$prev_year,$prev_month);
    printf($HTML "   <td align=\"right\"><a href=\"../%04d/NWS_rrs_inventory_%04d%02d.html\">Next Month</a></td>\n",$next_year,$next_year,$next_month);
    printf($HTML "   </tr>\n");

    my $station_count = 0;
    foreach my $id (sort(keys(%{ $data}))) {
	
	foreach my $wban (sort(keys(%{ $data->{$id}}))) {
	    # repeat the column headings after every 5th station
	    if ($station_count % 5 == 0) {
		printf($HTML "   <tr>\n");
		printf($HTML "      <th>ID</th>\n");
		printf($HTML "      <th>WBAN</th>\n");
		printf($HTML "      <th>WMO</th>\n");
		printf($HTML "      <th>Location</th>\n");
		printf($HTML "      <th>Ascension Range</th>\n");
		printf($HTML "      <th><table><tr>\n");
		for (my $day = 1; $day <= $day_count; $day++) {
		    printf($HTML "      <th width=20>%02d</th>\n",$day);
		}
		printf($HTML "      </tr></table></th>\n");
		printf($HTML "   </tr>\n");
	    } # end if ($station_count % 5 == 0)
	    
	    printf($HTML "   <tr class=\"%s\">\n",$station_count % 2 == 0 ? "evenstation" : "oddstation");
	    printf($HTML "      <td align=\"center\"><b>%s</b></td>\n",$id);
	    printf($HTML "      <td align=\"center\">%s</td>\n",$wban);
	    # LEH: The failure is on the line below:  "Can't use string ("PGUM") as a HASH ref
	    # while "strict refs" in use ... on line 561"
	    # this error occurs when no data has been received for the month
	    printf($HTML "      <td align=\"center\">%s</td>\n",$data->{$id}->{$wban}->{"wmo"});
	    printf($HTML "      <td nowrap><b>%s</b></td>\n",$data->{$id}->{$wban}->{"location"});
	    printf($HTML "      <td align=\"center\">%d<br>%d</td>\n",
		   $data->{$id}->{$wban}->{"first_ascension"},$data->{$id}->{$wban}->{"last_ascension"});
	
	    printf($HTML "      <td class=\"data\">\n");
	    printf($HTML "         <table>\n");

	    my $hour_count = 0;
	    foreach my $hour (sort(keys(%{ $data->{$id}->{$wban}->{"hours"}}))) {
		printf($HTML "            <tr class=\"%s\">\n",$station_count % 2 == 0 ? ($hour_count % 2 == 0 ? "eveneven" : "evenodd") : ($hour_count % 2 == 0 ? "oddeven" : "oddodd"));
		for (my $day = 1; $day <= $day_count; $day++) {
		    printf($HTML "            <td width=20 align=\"center\">%s</td>\n",
			   defined($data->{$id}->{$wban}->{"hours"}->{$hour}->{sprintf("%02d",$day)}) ? ($data->{$id}->{$wban}->{"hours"}->{$hour}->{sprintf("%02d",$day)} == 1 ? sprintf("<font class=\"miss\">%s</font>",$hour) : $hour) : "&nbsp;");
		}
		printf($HTML "            </tr>\n");
		$hour_count++;
	    } # end foreach my $hour...
	    printf($HTML "         </table>\n");
	    printf($HTML "      </td>\n");
	    printf($HTML "   </tr>\n");
	    
	    $station_count++;
	 } # end foreach my $wban (sort(keys(%{ $data->{$id}}))) 
    } # end foreach my $id (sort(keys(%{ $data}))) 
    
    printf($HTML "   <tr>\n");
    printf($HTML "      <th>ID</th>\n");
    printf($HTML "      <th>WBAN</th>\n");
    printf($HTML "      <th>WMO</th>\n");
    printf($HTML "      <th>Location</th>\n");
    printf($HTML "      <th>Ascension Range</th>\n");
    printf($HTML "      <th><table><tr>\n");
    for (my $day = 1; $day <= $day_count; $day++) {
	printf($HTML "      <th width=20>%02d</th>\n",$day);
    }
    printf($HTML "      </tr></table></th>\n");
    printf($HTML "   </tr>\n");

    printf($HTML "   <tr>\n");
    printf($HTML "   <td align=\"left\" colspan=5><a href=\"../%04d/NWS_rrs_inventory_%04d%02d.html\">Previous Month</a></td>\n",$prev_year,$prev_year,$prev_month);
    printf($HTML "   <td align=\"right\"><a href=\"../%04d/NWS_rrs_inventory_%04d%02d.html\">Next Month</a></td>\n",$next_year,$next_year,$next_month);
    printf($HTML "   </tr>\n");
    printf($HTML "</table>\n");

    printf($HTML "</body>\n");
    printf($HTML "</html>\n");
    close($HTML);
    if ($debug) { printf ("Exit create_HTML.\n");}

} # create_HTML()

##------------------------------------------------------------------------
# @signature void create_inventory_files(HashRef dates)
# <p>Create a new inventory HTML page for each of the dates specified in
# the Hash reference.</p>
#
# @param $dates The reference to the Hash containing the dates to create
# the inventory files for.
##------------------------------------------------------------------------
sub create_inventory_files {
    my ($dates) = @_;

    if ($debug) { printf ("\n---------------Enter create_inventory_files.\n"); } 

    create_directory($WORK_DIR);

    foreach my $year (sort(keys(%{$dates}))) {
	my $iven_dir = sprintf("%s/%04d",$IVEN_DIR,$year);
	create_directory($iven_dir);


	foreach my $month (sort(keys(%{ $dates->{$year}}))) {	
	    my $data = {};
	    
	    opendir(my $ARCH,sprintf("%s/%04d",$ARCHIVE_DIR,$year)) or die("Can't read archive directory\n");
	    my @idlist = grep(/^[A-Z]{4}/,readdir($ARCH));
	    closedir($ARCH);

            #-----------------------------------------------------------
	    # Make sure a station has the date set from the station list
            #  and did not appear out of nowhere
            #-----------------------------------------------------------
	    my $should_die = 0;
	    foreach my $stn (sort(@idlist)) {
		if (!defined($stnids->{$stn})) {
		    $ERROR_REPORT .= sprintf("create_inventory_files:: ERROR: Station %s not found in station list (Begin or end date may not be set)\n",$stn);
                    printf($LOCK "Station $stn not found in station list (Begin or end date may not be set)\n");
                    if ($debug) { printf ("create_inventory_files:: ERROR: Station %s not found in station list (Begin or end date may not be set)\n",$stn); } 
		    $should_die = 1;
		}
	    }
	    die("create_inventory_files:: Station NOT found in station list!\n") if ($should_die);

	    my $count = 0;   # Commented out by Joel.

	    foreach my $id (sort(keys(%{ $stnids}))) {

		if ($stnids->{$id}->{"start_date"} <= sprintf("%04d%02d",$year,$month)) {

                    #----------------------------------------------------------------
		    # Initialize with the station list information
		    # Also ensures that the station will be printed in the inventory.
                    #----------------------------------------------------------------
		    foreach my $wban (@{ $stnids->{$id}->{"wbans"}}) {
			$data->{$wban}->{"location"} = $stnids->{$id}->{"name"};
			$data->{$wban}->{"wban"} = $wban;
			$data->{$wban}->{"wmo"} = $stnids->{$id}->{"wmo"};
			$data->{$wban}->{"id"} = $id;
		    }
		    
		    my $archive_dir = sprintf("%s/%04d/%s/%02d",$ARCHIVE_DIR,$year,$id,$month);
		    my $meta_archive_dir = sprintf("%s/%04d/%s/%02d",$META_ARCHIVE_DIR,$year,$id,$month);

		    create_directory($meta_archive_dir);
		    # added create archive dir 10/3/2011 - if no data has been received
		    # for the month, not having a directory has caused problems
		    create_directory($archive_dir) unless (-e $archive_dir);

		    
		    opendir(my $STN,$archive_dir) or die("Can't read directory: $archive_dir\n");
		    my @files = grep(/\d{5}_\d{10}/,readdir($STN));
		    closedir($STN);
		    
		    foreach my $file (reverse(sort(@files))) {
			if ($debug) {printf("create_inventory_files:: %5d - Processing file:
				%s/%s\n",++$count,$archive_dir,$file);} #Commented out by Joel

			my $zero_size = 0;

			my $meta_file = $file;
			$meta_file =~ s/\.gz$//;
			$meta_file .= "_1Meta.txt";

                        if ($debug) { printf ("create_inventory_files:: (1) zero_size = $zero_size (0=no,1=yes)\n"); } 

                        if (-e sprintf("%s/%s.gz", $meta_archive_dir, $meta_file)) {
                            gunzip($meta_archive_dir, sprintf("%s.gz", $meta_file));

                           if ($debug){
                              printf ("create_inventory_files:: (2) zero_size = $zero_size (0=no,1=yes)\n");

                              my $metaFileName = sprintf("%s/%s", $meta_archive_dir, $meta_file);
                              my $FileName     = sprintf("%s/%s", $archive_dir,      $file);

                              my @statMetaFile = stat($metaFileName);
                              my @statFile = stat($FileName);

                              if ($statMetaFile[9] ne '')
                                 { 
                                 printf ("create_inventory_files:: fileName and stats: $FileName, $statFile[9]\n" ); 
                                 printf ("create_inventory_files:: metafileName and stats: $metaFileName, $statMetaFile[9]\n" ); 
                                 }
                              else
                                 { printf ("create_inventory_files:: (metafile) STATS ARE NULL for file: $metaFileName\n" ); }
                           } # debug

                        } # if meta file exits


                        #-----------------------------------------
                        # Check if file exists and if so check
                        # if last modify time of meta file is 
                        # before last modify time of archive file.
                        # If all true, then we have an updated or
                        # new data file, so try and unbuffer it.
                        #------------------------------------------
			if (!(-e sprintf("%s/%s",$meta_archive_dir,$meta_file)) ||
			    ((stat(sprintf("%s/%s",$meta_archive_dir,$meta_file)))[9] <
			     (stat(sprintf("%s/%s",$archive_dir,$file)))[9])) {

                           if ($debug) { printf ("create_inventory_files:: (3) zero_size = $zero_size (0=no,1=yes)\n"); } 

                          #------------------------
                          # Unbuffer the data file.
                          #------------------------
                          if ($debug) { printf ("\ncreate_inventory_files:: Before call unbuffer() - B1. zero_size = $zero_size (0=no,1=yes)\n"); } 
			  $zero_size = unbuffer($archive_dir,$file,$id,$meta_archive_dir);

                          if ($debug) { printf ("create_inventory_files:: After call unbuffer() - A1. zero_size = $zero_size(0=no,1=yes)\n\n"); } 
			}

                        #-------------------------
                        # Parse the Metadata file.
                        #-------------------------
                        if ($debug) { printf ("\ncreate_inventory_files:: Before call parse_metdata() - B1. zero_size=$zero_size (0=no,1=yes)\n"); } 
			parse_metadata($file,$data,$meta_archive_dir) if (!$zero_size);

                        if ($debug) { printf ("create_inventory_files:: After Call parse_metdata() - A1. zero_size=$zero_size (0=no,1=yes)\n"); } 

                        gzip($meta_archive_dir, $meta_file) if (-e sprintf("%s/%s", $meta_archive_dir, $meta_file));
		    }
		    
		    foreach my $wban (keys(%{ $data})) {
			my ($dir,$file) = find_last_file($year,$month,$id);
			if (defined($file)) {
			    my $zero_size = 0;
			    
			    my $meta_file = $file;
			    $meta_file =~ s/\.gz$//;
			    $meta_file .= "_1Meta.txt";

                            if (-e sprintf("%s/%s.gz", $meta_archive_dir, $meta_file)) {
                                gunzip($meta_archive_dir, sprintf("%s.gz", $meta_file));
                            }

			    if (!(-e sprintf("%s/%s",$meta_archive_dir,$meta_file)) ||
				((stat(sprintf("%s/%s",$meta_archive_dir,$meta_file)))[9] <
				 (stat(sprintf("%s/%s",$archive_dir,$file)))[9])) {

                              #------------------------
                              # Unbuffer the data file.
                              #------------------------
                              if ($debug) { printf ("create_inventory_files:: before Call unbuffer() - B2. zero_size = $zero_size (0=no,1=yes)\n"); } 
			      $zero_size = unbuffer($dir,$file,$id,$meta_archive_dir);
                              if ($debug) { printf ("create_inventory_files:: before Call unbuffer() - A2. zero_size = $zero_size (0=no,1=yes)\n"); } 
			    }

                            #------------------------
                            # Parse the Metadata file.
                            #------------------------
                            if ($debug){printf("create_inventory_files:: Before call parse_metdata() - B2. zero_size=$zero_size (0=no,1=yes)\n"); } 
			    parse_metadata($file,$data,$meta_archive_dir) if (!$zero_size);

                            if ($debug){printf("create_inventory_files:: After call parse_metdata() - A2. zero_size=$zero_size (0=no, 1=yes)\n"); } 
                            gzip($meta_archive_dir, $meta_file) if (-e sprintf("%s/%s", $meta_archive_dir, $meta_file));
			} # end if (defined($file))
		    } # end foreach my $wban
		} # end foreach my $file (reverse(sort(@files)))
	    }

            if ($debug){printf("create_inventory_files:: Call test_data(), year = $year, month=$month\n"); } 
            if ($debug){printf("create_inventory_files:: Call test_data(), data = $data\n"); } 

	    test_data($year,$month,$data);

	    # Commented out and replaced by echohawk 12 May 2011
	    # if ($debug){printf("create_inventory_files:: Call create_HTML()\n"); } 
	    # printf($LOCK "create_inventory_files:: Call create_HTML()\n");
            if ($debug){printf("create_inventory_files:: Call create_HTML() for date: $year $month\n"); } 
            printf($LOCK "create_inventory_files:: Call create_HTML() for date: $year $month\n");
	    
	    create_HTML($year,$month,$data);
	}
    }

    rmdir($WORK_DIR);

    if ($debug) { printf ("Exit create_inventory_files().\n\n"); } 

} # create_inventory_files()

##------------------------------------------------------------------------
# @signature boolean enough_time_from_ftp(String dir, String file)
# <p>Determine if the specified file is at least 15 minutes old from the
# current time.  This was decided to be enough time that the entire file
# had arrived and it is not partially transmitted.</p>
#
# @input $dir The directory where the file is located.
# @input $file The name of the file to be tested.
# @output $pass A flag that specifies if the file is at least 15 mintues old.
##------------------------------------------------------------------------
sub enough_time_from_ftp {
    my ($dir, $file) = @_;
    
    # Get the current time in seconds from the epoch.
    my $time = time();
    # Get the last modify time of the file in seconds from the epoch.
    my $filetime = (stat(sprintf("%s/%s", $dir, $file)))[9];

    # Return true if the file is over 15 minutes old, false if it is not.
    return ($time - $filetime > (15.0 * 60.0));

} # enough_time_from_ftp()

##------------------------------------------------------------------------
# @signature String dir, String file find_last_file(int year, int month, int wban)
# <p>Find the most recent file for the WBAN provided to find the start
# ascension number for the provided month.</p>
#
# @input $year The year to find the most recent file for.
# @input $month The month to find the most recent file before.
# @input $wban The WBAN number of the station.
# @output $dir The directory where the most recent file can be found.
# @output $file The most recent file.
##------------------------------------------------------------------------
sub find_last_file {
    my ($year,$month,$id) = @_;
    my ($dir,$file);
    
    while (!defined($file) && $month - 1 > 0) {
	$dir = sprintf("%s/%04d/%02d/$id",$ARCHIVE_DIR,$year,$month-1,$id);
	if (-e $dir) {
	    opendir(my $DIR,$dir) or die("Can't open $dir\n");
	    $file = (reverse(sort(grep(/\d{5}_\d{10}/,readdir($DIR)))))[0];
	    closedir($DIR);
	}
	$month--;	
    }

    return ($dir,$file);

} # find_last_file()

##------------------------------------------------------------------------
# @signature (String,int) get_month_data(int month, int year)
# <p>Get the name and number of days for the specified month.</p>
#
# @input $month The month to be retreived.
# @input $year The year for the month.
# @output $name The name of the month.
# @output $days The number of days in the month.
##------------------------------------------------------------------------
sub get_month_data {
    my ($month,$year) = @_;
    return ((("January","February","March","April","May","June",
	      "July","August","September","October","November","December"))[$month - 1],
	    ((31,daysInFeb($year),31,30,31,30,31,31,30,31,30,31))[$month - 1]);
} # get_month_data()

##------------------------------------------------------------------------
# @signature (int,int) get_next_month(int year, int month)
# <p>Get the month that follows the specified month.</p>
#
# @input $year The current year.
# @input $month The current month.
# @output $year The next year.
# @output $month The next month.
##------------------------------------------------------------------------
sub get_next_month {
    my ($year,$month) = @_;
    $month++;

    if ($month > 12) {
	$month -= 12;
	$year++;
    }
    return ($year,$month);

} # get_next_month()

##------------------------------------------------------------------------
# @signature (int,int) get_previous_month(int year, int month)
# <p>Get the month that preceded the specified month.</p>
#
# @input $year The current year.
# @input $month The current month.
# @output $year The previous year.
# @output $month The previous month.
##------------------------------------------------------------------------
sub get_previous_month {
    my ($year,$month) = @_;
    $month--;

    if ($month == 0) {
	$month = 12;
	$year--;
    }
    return ($year,$month);

} # get_previous_month()

##------------------------------------------------------------------------
# @signature String gzip(String dir, String file)
# <p>Compress the specified file using gzip.</p>
#
# @input $dir The directory where the file is stored.
# @input $file The file to be gzipped.
# @output $file The name of the gzipped file after compression.
##------------------------------------------------------------------------
sub gzip {
    my ($dir,$file) = @_;
    system(sprintf("/usr/bin/gzip -f %s/%s",$dir,$file)) == 0 or die("Error gzipping file $dir/$file\n\t$?\n");
    return sprintf("%s.gz",$file);

} # gzip()

##------------------------------------------------------------------------
# @signature Strin gunzip(String dir, String file)
# <p>Uncompress the specified file using gunzip.</p>
#
# @input $dir The directory where the file is stored.
# @input $file The file to be gunzipped.
# @output $file The name of the file after the decompression.
##------------------------------------------------------------------------
sub gunzip {
    my ($dir,$file) = @_;
        system(sprintf("/usr/bin/gunzip %s/%s",$dir,$file)) == 0 or die("Error gunzipping file $dir/$file\t$?\n");
    $file =~ s/\.gz$//;
    return $file;

} # gunzip()

##------------------------------------------------------------------------
# @signature void load_station_list()
# <p>Read in the data from the station list file and load it into the 
# appropriate hash tables.</p>
##------------------------------------------------------------------------
sub load_station_list {
    open(my $STN,$STN_FILE) or die("Can't read station list: $STN_FILE\n");
    <$STN>;

    foreach my $line (<$STN>) {
	chomp($line);
	next if ($line =~ /^\s*$/);
	my @data = split(";",$line);
	
	if (defined($wbans->{$data[1]})) {
            $ERROR_REPORT .= "Station $data[0] ($data[1]) already exists in station list. $line\n";
            printf($LOCK "Station $data[0] $data[1] already exists in station list. See line $line.\n");
	    exit(1);
	}

        #--------------------------------------------	
        # Set the WMO # to missing if it is not known
        #--------------------------------------------	
        $data[2] = "-9999" if (!defined($data[2]) || $data[2] =~ /^\s*$/);
	
	if (defined($data[6])) {
	    # Hash when the wban is known
	    $wbans->{$data[1]}->{"id"} = $data[0];
	    $wbans->{$data[1]}->{"wmo"} = $data[2];
	    $wbans->{$data[1]}->{"name"} = $data[3];
	    $wbans->{$data[1]}->{"start_date"} = $data[6];

	    # Hash when the id is known
	    push(@{ $stnids->{$data[0]}->{"wbans"}},$data[1]);
	    $stnids->{$data[0]}->{"wmo"} = $data[2];
	    $stnids->{$data[0]}->{"name"} = $data[3];
	    $stnids->{$data[0]}->{"start_date"} = $data[6];
	}
    }
    
    close($STN);

} # load_station_list()

##------------------------------------------------------------------------
# @signature void parse_metadata(String file, Hash* data)
# <p>Read the metadata file to parse out the station information, station
# id number, sounding ascension number, and release time.</p>
#
# @input $file The name of the buffer file that is to have its metadata
#     parsed
# @input $data The reference to the Hash that contains the station data.
##------------------------------------------------------------------------
sub parse_metadata {
    my ($file,$data,$meta_archive_dir) = @_;

    my $metafile = $meta_archive_dir.$file;
    my ($Mdev,$Mino,$Mmode,$Mnlink, $Muid,$Mgid,$Mrdev,$Msize, $Matime,$Mmtime,$Mctime, $Mblksize,$Mblocks) = 0;

    if ($debug) { printf (" \n ---> Enter parse_metadata(). Metadata file = $metafile\n"); } 

    my $sounding = {};
    $sounding->{"filename"} = trim($file);

    $file =~ s/(\.\d+)?\.gz//;
    $file .= "_1Meta.txt";

    if (-e sprintf("%s/%s.gz", $meta_archive_dir, $file)) {
        gunzip($meta_archive_dir, sprintf("%s.gz", $file));
    }

    unless (-e sprintf("%s/%s",$meta_archive_dir,$file)) {
        $ERROR_REPORT .= sprintf("$file could not be found!\n");
        printf($LOCK "$file could not be found!\n");
        return;
    }

    open(my $META,sprintf("%s/%s",$meta_archive_dir,$file)) or die("Can't read $file\n");

    #-----------------------------------
    # check the stats on the Meta file.
    #-----------------------------------
    ($Mdev,$Mino,$Mmode,$Mnlink, $Muid,$Mgid,$Mrdev,$Msize,
     $Matime,$Mmtime,$Mctime, $Mblksize,$Mblocks) = stat $META;

    if ($debug) {
       printf ("parse_metadata:: metafile stats are $Msize, $Mblksize, $Mblocks\n");
       printf ("parse_metadata:: Size of metafile $metafile = $Msize\n");

       if ($Msize == 0) {
          printf ("parse_metadata:: BAD META - FILE SIZE!!!\n"); 
          printf ("parse_metadata - need to exit!\n");
          }
       }

    if ($Msize == 0) {  
       $ERROR_REPORT .= sprintf("parse_metadata:: Metadata file for %s %s has zero length. Terminate processing.\n", $meta_archive_dir, $file);
       printf($LOCK "parse_metadata:: Found zero size metafile. BAD FILE SIZE. EXIT.\n");
       die("parse_metadata:: Found zero size metafile. BAD FILE SIZE. EXIT.");
       } 

    my $line = <$META>;
    my $key = substr($file,0,5);

    if ($debug) {printf ("parse_metadata:: key, line:: $key, $line\n");}
   
    $data->{$key}->{"soundings"} = () unless(defined($data->{$key}->{"soundings"}));

    #------------------------------------------- 
    # Parse out the station location information
    #------------------------------------------- 
    $line =~ /Metadata,\s+(.+)$/;
    test_metadata($data,$key,"location",trim($1));

    #------------------------------------------- 
    # Get to the data the inventory cares about
    #------------------------------------------- 
    while ($line !~ /1\s+DATA SIGNIFICANCE/) { $line = <$META>; }

    #------------------------------------------- 
    # Parse out the ICAO Location ID
    #------------------------------------------- 
    $line = <$META>; $line =~ /([A-Z]+)\s+SHORT ICAO LOCATION IDENTIFIER/;
    test_metadata($data,$key,"id",trim($1));

    #------------------------------------------- 
    # Parse out the WMO number
    #------------------------------------------- 
    $line = <$META>; $line =~ /(\d+)\s+WMO BLOCK AND STATION NUMBER/;
    test_metadata($data,$key,"wmo",trim($1));

    #------------------------------------------- 
    # Parse out the WBAN number
    #------------------------------------------- 
    $line = <$META>; $line =~ /(\d+)\s+WBAN NUMBER/;
    test_metadata($data,$key,"wban",trim($1));

    #------------------------------------------- 
    # Parse out the ascension number
    #------------------------------------------- 
    while ($line !~ /RADIOSONDE ASCENSION NUMBER/) { $line = <$META>; }
    $line =~ /(\d+)\s+RADIOSONDE ASCENSION NUMBER/;
    $sounding->{"ascension"} = trim($1);

    #------------------------------------------- 
    # Parse out the actual date and time
    #------------------------------------------- 
    while ($line !~ /3\s+DATA SIGNIFICANCE,.*, balloon launch point/i) { $line = <$META>; }
    $line = <$META>; $line =~ /(\d+)\s+YEAR/; $sounding->{"year"} = trim($1);
    $line = <$META>; $line =~ /(\d+)\s+MONTH/; $sounding->{"month"} = trim($1);
    $line = <$META>; $line =~ /(\d+)\s+DAY/; $sounding->{"day"} = trim($1);
    $line = <$META>; $line =~ /(\d+)\s+HOUR/; $sounding->{"hour"} = trim($1);
    $line = <$META>; $line =~ /(\d+)\s+MINUTE/; $sounding->{"min"} = trim($1);
    $line = <$META>; $line =~ /([\d\.]+)\s+SECOND/; $sounding->{"sec"} = trim($1);

    push(@{ $data->{$key}->{"soundings"}},$sounding);

    close($META);

    #------------------------------------------- 
    # Following commented out by Joel.
    #------------------------------------------- 
    #unlink(sprintf("%s/%s",$WORK_DIR,$file)) unless (-e sprintf("%s/%s.out",$WORK_DIR,substr($file,0,16)));

    if ($debug) { printf ("--->Exit parse_metadata().\n\n"); } 

} # parse_metadata()

##------------------------------------------------------------------------
# @signature void read_ingest_for_RRS()
# <p>Read the ingest directory and find the files that match the RRS 
# filename convention.</p>
#
# @return files[] The list of RRS files.
##------------------------------------------------------------------------
sub read_ingest_for_RRS {
    opendir(my $INGEST,$INGEST_DIR) or die("Can't read $INGEST_DIR\n");
    my @files = sort(grep(/^\d{5}_\d{10}(\.gz)?$/,readdir($INGEST)));
    closedir($INGEST);

    return @files;

} # read_ingest_for_RRS()

##------------------------------------------------------------------------
# @signature void test_data(int year, int month, Hash* data)
# <p>Test the values in the data hash for the specified month and year.  It
# will make sure that the wban in the header matches the filename, the 
# actual release time matches the nominal time in the filename, and that
# there are not missing ascension numbers for the month.</p>
#
# @input $year The year to be tested.
# @input $month The month to be tested.
# @input $data The data to be tested.
##------------------------------------------------------------------------
sub test_data 
{
    my ($year,$month,$data) = @_;

    if ($debug_test_data) { printf ("Enter test_data(). Yr, Mon:: $year,$month\n"); } 
    if ($debug_test_data) { printf ("Enter test_data(). Data:: $data\n"); } 

    foreach my $key (sort(keys(%{ $data}))) 
    {
        if ($debug_test_data) { printf ("Enter test_data(). Foreach loop() - 1\n"); } 

	my $stuff = {};
	$stuff->{"location"} = $data->{$key}->{"location"};
	$stuff->{"wban"} = $data->{$key}->{"wban"};
	$stuff->{"wmo"} = $data->{$key}->{"wmo"};

	# if ($debug_test_data) { print Dumper($data); }

        if ($debug_test_data) 
        { 
            my $loc = $stuff->{"location"};
            my $wban = $stuff->{"wban"};
            my $wmo = $stuff->{"wmo"};
            printf ("  stuff:loc,wban,wmo :: $loc, $wban, $wmo \n"); 
        } 

	my $last_asc;

	# LEH: this fixed the failure: "Can't use an undefined
	# value as an ARRAY reference at ... line 1171."
	if (defined($data->{$key}->{"soundings"}))
	{

	    foreach my $sounding (reverse(@{ $data->{$key}->{"soundings"}})) 
	    {
	        if ($debug_test_data) { printf ("Enter test_data(). Foreach loop() - 2 - Handle dates.\n"); } 

	        # Handle the dates
	        my $act_date = sprintf("%04d%02d%02d",$sounding->{"year"},$sounding->{"month"},$sounding->{"day"});
	        my $act_time = sprintf("%02d%02d%02d",$sounding->{"hour"},$sounding->{"min"},$sounding->{"sec"});
	        my ($nom_date,$nom_time) = adjustDateTime($act_date,"YYYYMMDD",$act_time,"HHMMSS",0,0,
		  				      -1 * $sounding->{"min"},
						      -1 * sprintf("%02d",$sounding->{"sec"}));
	        my ($next_nom_date,$next_nom_time) = adjustDateTime($nom_date,"YYYYMMDD",
								$nom_time,"HHMMSS",0,1,0,0);
                if ($debug_test_data) { printf ("    act_date, act_time, nom_date, nom_time, next_nom_date, next_nom_time::$act_date, $act_time, $nom_date,$nom_time, $next_nom_date,$next_nom_time \n"); } 

                #-------------------------------------
                # Following commented out by Joel. - 2 lines  HERE!
                #-------------------------------------
                if ($debug_test_data) 
		{
   	            printf("test_data(): Checking file, asc, actual date & time::  %s: %4d %s %s\n",$sounding->{"filename"},$sounding->{"ascension"},$act_date,$act_time);
	            printf("test_data(): Checking file, asc, actual date & time:: %s: %4d %s %s\n\n",$sounding->{"filename"},$sounding->{"ascension"},$nom_date,$nom_time);
                }

	        if (!(substr($sounding->{"filename"},6,8) == $nom_date && substr($sounding->{"filename"},14,2) == substr($nom_time,0,2)) &&
		     !(substr($sounding->{"filename"},6,8) == $next_nom_date && substr($sounding->{"filename"},14,2) == substr($next_nom_time,0,2))) 
	        {
		    $ERROR_REPORT .= sprintf("Actual time (%s %s) does not align with time in file name for %s\n",$act_date,$act_time,$sounding->{"filename"});
                    printf($LOCK "Actual time ($act_date $act_time) does not align with time in file name. See the Error Report\n");
	        }

	        if ($year == substr($sounding->{"filename"},6,4) && $month == substr($sounding->{"filename"},10,2)) 
		{
		    $stuff->{"hours"}->{substr($sounding->{"filename"},14,2)}->{substr($sounding->{"filename"},12,2)} = 0;
	        }
       

                #-----------------------
	        # Handle the wban number
                #-----------------------
                if ($debug_test_data) { printf ("Handle the wban number.\n"); } 

	        if ($data->{$key}->{"wban"} != substr($sounding->{"filename"},0,5)) 
		{
		    $ERROR_REPORT .= sprintf("WBAN in file name does not match with data in header. File: %s\n",$sounding->{"filename"});
                    printf($LOCK "WBAN in file name does not match with data in header. See Error Report.\n");
	        }

                #-----------------------------
	        # Handle the ascension numbers
                #-----------------------------
                if ($debug_test_data) { printf ("Handle the ascension numbers.\n"); } 

	        $stuff->{"first_ascension"} = $sounding->{"ascension"} if (!defined($stuff->{"first_ascension"}) && $year == substr($sounding->{"filename"},6,4) && $month == substr($sounding->{"filename"},10,2));
	    
	        if (!defined($last_asc) || $last_asc + 1 == $sounding->{"ascension"}) { 
		    $last_asc = $sounding->{"ascension"};
	        } else {
		    # if ($debug_test_data) { printf ("Ascension number missing (%d %d) at file %s!!\n",$last_asc,$sounding->{"ascension"},$sounding->{"filename"}); }
	            ####$ERROR_REPORT .= sprintf("Ascension number missing (%d %d) at file %s!!\n",$last_asc,$sounding->{"ascension"},$sounding->{"filename"}); # Commented out by Joel.
		    $last_asc = $sounding->{"ascension"};
		    $stuff->{"hours"}->{substr($sounding->{"filename"},14,2)}->{substr($sounding->{"filename"},12,2)} = 1;
	        }
	    }
	
	    $stuff->{"last_ascension"} = $last_asc;

	    $data->{$data->{$key}->{"id"}}->{$key} = $stuff;
	    delete($data->{$key});
        } # LEH:  if (defined($data->{$key}->{"soundings"})) (added by LEH)
    }

    if ($debug_test_data) { printf ("Exit test_data().\n"); } 
} # test_data()

##------------------------------------------------------------------------
# @signature void test_metadata(Hash* data, String key, String type, String value)
# <p>Test a metadata value with the previously stored value in the data
# hash.</p>
# 
# @input $data The reference to the hash containing the station metadata.
# @input $key The key that uniquely identifies the station (WBAN # from file name).
# @input $type The key for the type of the value to be tested.
# @input $value The value to be tested for the key and type.
##------------------------------------------------------------------------
sub test_metadata {
    my ($data,$key,$type,$value) = @_;

    if (defined($data->{$key}->{$type}) && $data->{$key}->{$type} ne $value) {
		$ERROR_REPORT .= sprintf("%s: Type %s has value %s being tested against %s.\n",
				$key,$type,$data->{$key}->{$type},$value);
                printf($LOCK "$key: Type $type has value $data->{$key}->{$type} being tested against $value.\n");
    } else {
	$data->{$key}->{$type} = $value;
    }
} # test_metadata()

##------------------------------------------------------------------------
# @signature int test_to_bufr_out_file(String realfile, String file)
# <p>Test the output generated by the UNIX buffer converter.  It is a 
# sanity check to see if the converter worked as expected.</p>
#
# @input $realfile The name of the buffer file that was converted.
# @input $file The log file for the converting of the realfile.
# @output $passed If the log file passed the expected format tests.
##------------------------------------------------------------------------
sub test_to_bufr_out_file {
    my ($realfile,$file,$id) = @_;
    my $passed = 0;

    open(my $FILE,$file) or die("Can't open file: $file\n");
    my ($dev,$ino,$mode,$nlink, $uid,$gid,$rdev,$size, $atime,$mtime,$ctime, $blksize,$blocks) = stat $FILE;

    if ($debug) {
      printf ("   test_to_bufr_out_file::file stats are $size, $blksize, $blocks\n");
      if ($size == 0) {printf ("test_to_bufr_out_file:: BAD FILE SIZE of zero for $file!\n"); }
      }

    if ($size == 0) {
        if ($debug) {
           printf("   test_to_bufr_out_file()::Input file has zero length. $file\n" );
           }

        $ERROR_REPORT .= sprintf("test_to_bufr_out_file:: Test FAILED. BAD FILE SIZE of zero for file %s", $realfile);
        printf($LOCK "test_to_bufr_out_file:: Test FAILED. BAD FILE SIZE of zero for file $realfile.\n");

       return 1;
    }

    my @lines = <$FILE>;
    close($FILE);

    my $year = substr($realfile,6,4);
    my $month = substr($realfile,10,2);

    if (scalar(grep(/[Ii]nfile:\s+$ARCHIVE_DIR\/$year\/$id\/$month\/$realfile/,@lines)) != 1 ||
	scalar(grep(/[Oo]utfile:\s+$WORK_DIR\/$realfile\.bufr/,@lines)) != 1 || 
        scalar(grep(/[Ee][Rr][Rr][Oo][Rr]/,@lines) > 0)) {

        if ($debug) {
           printf("   test_to_bufr_out_file()::grabbufr/unixbufr output for file %s was NOT what was expected. Output follows.\n\t%s", $realfile,join("\t",@lines) ); 
           }

	$ERROR_REPORT .= sprintf("test_to_bufr_out_file:: grabbufr/unixbufr output for file %s was NOT what was expected.  Output follows.\n\t%s", $realfile,join("\t",@lines));
        printf($LOCK "test_to_bufr_out_file:: grabbufr/unixbufr output for file $realfile was NOT what was expected. Output follows. See the Error Report.\n");
	$passed = 0;
    } else {
        if ($debug) {
           printf("   test_to_bufr_out_file()::grabbufr/unixbufr output for file %s WAS as expected.\n", $file);
           }
	$passed = 1;
    }

    return $passed;

} # test_to_bufr_out_file()

##------------------------------------------------------------------------
# @signature int test_to_text_out_file(String realfile, String file)
# <p>Test the output generated by the decoder that creates the text files
# from the buffer file.  It is a sanity check to see if the decoding
# worked as expected.</p>
#
# @input $realfile The name of the buffer file that was converted to text. 
#                This is the actual raw data sent to us by NCDC and copied
#                in from the /net/ingest/ncdc directory.  The nominal time
#                is part of the file name.
# @input $file The log file for the decoding of the realfile.
# @output $passed If the log file passed the expected format tests.
##------------------------------------------------------------------------
sub test_to_text_out_file {
    my ($realfile,$file) = @_;
    my $passed = 0;

    open(my $FILE,$file) or die("Can't open file: $file\n");
    my ($dev,$ino,$mode,$nlink, $uid,$gid,$rdev,$size, $atime,$mtime,$ctime, $blksize,$blocks) = stat $FILE;

    if ($debug) {
      printf ("   test_to_text_out_file:: file stats are $size, $blksize, $blocks\n");
      if ($size == 0) {printf ("   test_to_text_out_file:: BAD FILE SIZE of zero for $file!\n"); }
      }

    if ($size == 0) {
      $ERROR_REPORT .= sprintf("test_to_text_out_file:: Input file has size of ZERO for %s\n", $file);
      printf($LOCK "test_to_text_out_file:: Input file has size of ZERO for $file.\n");
      return 1;
      }

    my @lines = <$FILE>;
    close($FILE);

    if (scalar(grep(/($realfile)_1Meta\.txt/,@lines)) != 1 ||
	scalar(grep(/($realfile)_2rPTU\.txt/,@lines)) != 1 ||
	scalar(grep(/($realfile)_3GPSu\.txt/,@lines)) != 1 ||
	scalar(grep(/($realfile)_4GPSs\.txt/,@lines)) != 1 ||
	scalar(grep(/($realfile)_5pPTU\.txt/,@lines)) != 1 ||
	scalar(grep(/($realfile)_6pGPS\.txt/,@lines)) != 1 ||
	scalar(grep(/($realfile)_7Lvls\.txt/,@lines)) != 1) {

        if ($debug)
           {
           printf("   test_to_text_out_file()::grabbufr/unixbufr output for file %s was NOT what was expected. Output follows.\n\t%s", $realfile,join("\t",@lines) );
           }

	$ERROR_REPORT .= sprintf("test_to_text_out_file:: decoder output for file %s was NOT what was expected.  Output follows.\n\t%s",
	       $realfile,join("\t",@lines));
        printf($LOCK "test_to_text_out_file:: decoder output for file $realfile was NOT what was expected. See the Error Report.\n");
	$passed = 0;
    } else {
        if ($debug){printf("   test_to_text_out_file()::decoder output for file %s WAS as expected.\n", $realfile);}
	$passed = 1;
    }
} # test_to_text_out_file()

##------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the leading and trailing white space from a String.</p>
#
# @input $line The line to be trimmed.
# @output $line The trimmed line.
##------------------------------------------------------------------------
sub trim {
    my ($line) = @_;
    
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;

    return $line;

} # trim()

##------------------------------------------------------------------------
# @signature void unbuffer(String archive, String file)
# <p>Read the data in the FTP'd buffer file and pull out the text file
# containing the metadata.  This will remove all of the intermediate
# files if there was not an error.</p>
#
# @input $archive The directory where the buffer file can be found.
# @input $file The name of the buffer file to be unbuffered.
##------------------------------------------------------------------------
sub unbuffer {
    my ($archive,$file,$id,$meta_archive_dir) = @_;

    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev, $size, $atime,$mtime,$ctime,$blksize,$blocks) = 0;
    $size = 0;

    if ($debug) { printf ("\n----- Enter unbuffer:: id = $id, file = $file\n");} 

    $file = gunzip($archive,$file) if ($file =~ /\.gz$/);

    if ($debug) { 
       my $archiveFileSize = (-s sprintf("%s/%s",$archive,$file));
       printf ("   unbuffer:: File size is $archiveFileSize\n" ); 
       }

    #---------------------------------------------------------
    # If the Original FTP'd input file has zero length,
    # then nothing to process, so return 1 without processing.
    #---------------------------------------------------------
    if ( (-z sprintf("%s/%s",$archive,$file) ) && ($debug) ) {
        printf ("   unbuffer:: input File %s has zero size. Return 1 on -z check\n", $file);
        }

    if (-z sprintf("%s/%s",$archive,$file)) {
        $ERROR_REPORT .= sprintf("unbuffer:: input File %s has zero size.\n",$file);
        printf($LOCK "unbuffer:: input File %s has zero size. Exit unbuffer. CAN NOT PROCESS FILE. Archive = $archive; File = $file \n");

        $file = gzip($archive,$file);
        printf($LOCK ".\n");
        if ($debug) {printf ("   unbuffer:: File size is ZERO length for $file . Return 1. Exit unbuffer. CAN NOT PROCESS FILE.\n" );}

        return 1;
    }

    #-----------------------------------------------------------------------------
    # Original FTP'd file has size, so try to 2 the two step processing on it.
    # The two steps are 1. grabbufr file then 2. decode the file. If the first
    # step fails, don't try the second step, just return 1 and be done.
    # Call the "grabbufr" routine. First step in unbuffering the binary file.
    #-----------------------------------------------------------------------------
    if ($debug) {
       my $command = sprintf("%s %s/%s %s/%s.bufr >& %s/%s.bufr.out",
                           $GRABBUFR,$archive,$file,$WORK_DIR,$file,$WORK_DIR,$file);
       printf ("   unbuffer:: Execute: xxx $command xxx\n");
       }

    if (system(sprintf("%s %s/%s %s/%s.bufr >& %s/%s.bufr.out",$GRABBUFR,$archive,$file,$WORK_DIR,$file,$WORK_DIR,$file)) == 0) {

      if ($debug) {printf ("   unbuffer:: command successful. Call test_to_bufr_out_file()\n"); }

      if (test_to_bufr_out_file($file,sprintf("%s/%s.bufr.out",$WORK_DIR,$file),$id)) {
	unlink(sprintf("%s/%s.bufr.out",$WORK_DIR,$file));

        #-------------------------------------------
        # If grabbufr worked, then do decoding step.
        #-------------------------------------------
        if ($debug) {
           my $command = sprintf("%s %s/%s.bufr >& %s/%s.out",$DECODER,$WORK_DIR,$file,$WORK_DIR,$file);
           printf ("   unbuffer:: Execute: xxx my $command xxx\n");
           }

        #----------------------------------------------------------
        # Don't run this decode command if input file is zero size.
        # Double checking is OK since this is run as a cron script.
        # Could just do -s or -z option check instead of getting all stats.
        #----------------------------------------------------------
        open(my $FILE, sprintf("%s/%s.bufr", $WORK_DIR, $file)) or die("Can't open file to decode: $WORK_DIR, $file.bufr\n");
        my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = stat $FILE;
        close($FILE);

        if ($debug) { printf("   unbuffer:: Run decoder if file size not zero. file= $file size=$size\n"); }

        if ($size == 0) 
           {
           printf("   unbuffer:: $WORK_DIR, $file could NOT be grabbufrd. Grabbufr processing generated a zero length file. Skip decoder step and STOP processing. File size = $size\n");
           $ERROR_REPORT .= sprintf("unbuffer:: %s %s could NOT be decoded. Grabbufr processing generated a zero length file. Skip decoder step and STOP processing.\n", $WORK_DIR, $file);
           printf($LOCK "unbuffer:: $WORK_DIR $file could NOT be decoded. Grabbufr processing generated a zero length file. Skip decoder step and STOP processing..\n");
           die("   unbuffer:: File ($WORK_DIR $file) could NOT be decoded. Grabbufr processing generated a zero length file.\n");
           }

        #---------------------------------------------------
        # Grabbufr fn generated non-zero length file, try
        # to run decoder command to generate text files from
        # binary file.
        #---------------------------------------------------
	if (system(sprintf("%s %s/%s.bufr >& %s/%s.out",$DECODER,$WORK_DIR,$file,$WORK_DIR,$file)) == 0) {

          if ($debug) {printf ("   unbuffer:: Call to decoder successful. Call test_to_text_out_file()\n"); }

	  if (test_to_text_out_file($file,sprintf("%s/%s.out",$WORK_DIR,$file))) {
            #----------------------
	    # Remove temporary files
            #----------------------
	    unlink(sprintf("%s/%s.out",$WORK_DIR,$file));
	    unlink(sprintf("%s/%s.bufr",$WORK_DIR,$file));
	    
            #---------------------------------
	    # Remove the non metadata files.
            # Joel commented out this section.
            # So, now keep all files types.
            #---------------------------------
	    #unlink(sprintf("%s/%s_2rPTU.txt",$WORK_DIR,$file));
	    #unlink(sprintf("%s/%s_3GPSu.txt",$WORK_DIR,$file));
	    #unlink(sprintf("%s/%s_4GPSs.txt",$WORK_DIR,$file));
	    #unlink(sprintf("%s/%s_5pPTU.txt",$WORK_DIR,$file));
	    #unlink(sprintf("%s/%s_6pGPS.txt",$WORK_DIR,$file));
	    #unlink(sprintf("%s/%s_7Lvls.txt",$WORK_DIR,$file));
	  }
	} else {   # call test_to_text_out_file()

          if ($debug) {printf("   unbuffer:: Call to decoder FAILED! ");}

          $ERROR_REPORT .= sprintf("unbuffer:: %s %s.bufr could NOT be decoded. Decoder processing generated a zero length file.\n", $WORK_DIR, $file);
          printf($LOCK "unbuffer:: $WORK_DIR $file.bufr could NOT be decoded. Decoder processing generated a zero length file.\n");

	  open(my $FILE, sprintf("%s/%s.out", $WORK_DIR, $file)) or die("Can't open file: $WORK_DIR, $file\n");
	  my @lines = <$FILE>;
	  close($FILE);

	  $ERROR_REPORT .= sprintf("\n%s\n\n", join("\t", @lines));

          if ($debug) {
             printf ("   unbuffer:: %s %s.bufr could NOT be decoded. Decoder processing generated a zero length file.\n", $WORK_DIR,$file);
             printf ("ERROR_REPORT::\n $ERROR_REPORT\n");
             }

        #------------------------------------------------------------
        # Unbuffering failed, so stop processing this file.
        # Not sure if in some cases partial files might be generated.
        #------------------------------------------------------------
        if ($debug) {printf ("   unbuffer:: Unbuffering failed. Return 1. Exit unbuffer.\n");}

        $ERROR_REPORT .= sprintf("unbuffer:: %s %s.bufr could NOT be unbuffered.\n", $WORK_DIR,$file);
        printf($LOCK "unbuffer:: $WORK_DIR $file could NOT be unbuffered.\n");
        return 1;

	}  # call test_to_text_out_file()

      } # if test_to_bufr_out_file

      #------------------------------------------------------
      # Unbuffering binary to ASCII done.
      # Move the metadata file to the local metadata archive.
      #------------------------------------------------------
	move(sprintf("%s/%s_1Meta.txt",$WORK_DIR,$file), sprintf("%s/%s_1Meta.txt",$meta_archive_dir,$file));
        move(sprintf("%s/%s_2rPTU.txt",$WORK_DIR,$file), sprintf("%s/%s_2rPTU.txt",$meta_archive_dir,$file));
        move(sprintf("%s/%s_3GPSu.txt",$WORK_DIR,$file), sprintf("%s/%s_3GPSu.txt",$meta_archive_dir,$file));
        move(sprintf("%s/%s_4GPSs.txt",$WORK_DIR,$file), sprintf("%s/%s_4GPSs.txt",$meta_archive_dir,$file));
        move(sprintf("%s/%s_5pPTU.txt",$WORK_DIR,$file), sprintf("%s/%s_5pPTU.txt",$meta_archive_dir,$file));
        move(sprintf("%s/%s_6pGPS.txt",$WORK_DIR,$file), sprintf("%s/%s_6pGPS.txt",$meta_archive_dir,$file));
        move(sprintf("%s/%s_7Lvls.txt",$WORK_DIR,$file), sprintf("%s/%s_7Lvls.txt",$meta_archive_dir,$file));

        #------------------------------------------------------------
        # Gzip the file in the archive location if it has been moved.
        #------------------------------------------------------------
        gzip($meta_archive_dir, sprintf("%s_1Meta.txt",$file)) if (-e sprintf("%s/%s_1Meta.txt", $meta_archive_dir, $file));
	gzip($meta_archive_dir, sprintf("%s_2rPTU.txt",$file)) if (-e sprintf("%s/%s_2rPTU.txt", $meta_archive_dir, $file));
        gzip($meta_archive_dir, sprintf("%s_3GPSu.txt",$file)) if (-e sprintf("%s/%s_3GPSu.txt", $meta_archive_dir, $file));
        gzip($meta_archive_dir, sprintf("%s_4GPSs.txt",$file)) if (-e sprintf("%s/%s_4GPSs.txt", $meta_archive_dir, $file));
        gzip($meta_archive_dir, sprintf("%s_5pPTU.txt",$file)) if (-e sprintf("%s/%s_5pPTU.txt", $meta_archive_dir, $file));
        gzip($meta_archive_dir, sprintf("%s_6pGPS.txt",$file)) if (-e sprintf("%s/%s_6pGPS.txt", $meta_archive_dir, $file));
        gzip($meta_archive_dir, sprintf("%s_7Lvls.txt",$file)) if (-e sprintf("%s/%s_7Lvls.txt", $meta_archive_dir, $file));

        system(sprintf("chmod 775 %s/%s_*", $meta_archive_dir, $file));
        system(sprintf("chgrp cds %s/%s_*", $meta_archive_dir, $file));

    } else { # if call to grabbufr worked

      #-------------------------
      # Call to grabbufr FAILED!
      #-------------------------
      if ($debug) {printf("   unbuffer:: Call to grabbufr FAILED!\n");}

      open(my $FILE, sprintf("%s/%s.bufr.out", $WORK_DIR, $file)) or die("Can't open file: $WORK_DIR, $file.bufr.out\n");
      my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = stat $FILE;

      my @lines = <$FILE>;
      close($FILE);

      $ERROR_REPORT .= sprintf("unbuffer:: There was a problem in the extraction of the %s.bufr file. Call to grabbufr failed.\n", $file);
      $ERROR_REPORT .= sprintf("unbuffer:: File stats: $dev,$ino,$mode,$nlink, $uid,$gid,$rdev,$size, $atime,$mtime,$ctime, $blksize,$blocks\n", $file);
      printf($LOCK "unbuffer:: There was a problem in the extraction of the $file.bufr file. Call to grabbufr failed. File size: $size . See the Error Report.\n");

      if ($size == 0) {$ERROR_REPORT .= sprintf("unbuffer:: BAD FILE SIZE of zero for $file!\n"); }
      $ERROR_REPORT .= sprintf("\n%s\n\n", join("\t", @lines));

      if ($debug) {
         printf ("   unbuffer::file stats are $size, $blksize, $blocks\n");
         if ($size == 0) {printf ("   unbuffer:: BAD FILE SIZE of zero for $file!<<<-----\n"); }
         printf ("ERROR_REPORT::\n $ERROR_REPORT\n");
         }

    #--------------------------------------------
    # Grabbufr failed. Stop processing this file.
    #--------------------------------------------
    if ($debug) {printf ("----->Exit unbuffer return 1. Grabbufr failed.\n");} 

    return 1;

    } # if call to grabbufr worked

    #----------------------------
    # Gzip the archive file.
    #----------------------------
    $file = gzip($archive,$file);

    if ($debug) {
       printf ("   unbuffer:: id = $id ");
       printf ("   unbuffer:: file = $file ");
       printf ("   unbuffer:: size = $size \n\n");
       }

   if ($debug) {printf ("----->Exit unbuffer return 0. Successful grabbufr and decoder processing.\n");} 
   return 0;

} # unbuffer()
