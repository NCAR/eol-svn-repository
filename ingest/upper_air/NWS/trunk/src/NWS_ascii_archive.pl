#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The <code>NWS_ascii_archive.pl</code> script creates an inventory file of the
# ASCII files sent to JOSS by NCDC.<p>
# <p>The script does the following operations:<ol>
#   <li>Copy the current month's ASCII sounding files to the archive directory.
#   <li>Check to see if any previously archived data files exist and either
#      remove them if they contain the same data, or copy the files to the
#      archive with a count number in the file name.
#   <li>Create an inventory log file for the ASCII files that JOSS has received and
#       has archived.
# </ol><p>
#
#
# @author Linda Echo-Hawk 4/2012
# @ version 2.1  Updated incoming file location from /net/ingest/ncdc to 
#    /net/ftp/pub/data/incoming/ncdc_soundings.  NOTE:  Documentation and 
#    some functions will still refer to the "ingest" area.  The area for 
#    incoming files has moved from /net/ingest to the new /net/ftp location.
#
# @author Joel Clawson 01/30/2006
# @version 2.01 Renamed the inventory directory to ../docs/inventory from ../inventory
#   and the archive directory to ../archive/ascii_files from ../ascii_files.
#
# @author Joel Clawson 01/24/2006
# @version 2.0 <ul>
#   <li>Changed to read from a different station list.  It uses start and end dates,
# along with RRS start dates to only show stations in the inventory that are expected.
#   <li>Changed the archiving from year/month to year/station.</li>
#   <li>Upgraded to automatically handle files that older than the processing year and
# month.  Instead of generating a log, it checks for differences and if the file is
# different gives it adds an increased count value to the file name.</li>
#   <li>Made the script recreate the inventory files for any data files that were
# archived for that year/month.</li>
#   <li>Turned off the saving data in INGEST_DIR for 3 months.  It will now remove all
# the files that have been archived.</li>
#   <li>Removed all log files, since no one looks at them.</li>
#   <li>Added warnings to the inventory file for cases where the same ascension number
# has two different release times, and when the same release time has two different
# ascension numbers.</li>
#   <li>Enhanced the individual sounding inventory to handle multiple month files for
# the same month. (i.e. wban05.11.gz and wban05.11.1.gz)</li>
#   <li>Changed the module library to version 5.</li>
#   <li>Added checks to see if an unexpected station started to get data.</li>
# </ul>
#
# @author Joel Clawson 12/15/2005
# @version 1.1 Changed the script to read in the individual soundings, not just the
#   stations.  Also changed the inventory file to be an HTML list instead of a text
#   file.
#
# @author Joel Clawson
# @version 1.0 This is an adaptation of a number of scripts that were written or at
#   least used by Darren Gallant for handling the archiving and inventory.
#
##Module------------------------------------------------------------------------
use strict;
#use lib "/work/software/conversion_modules/Version5/";
use lib "/work/lib/perl/Utilities";
use lib "/net/work/lib/perl/Utilities";
use DpgDate;
use File::Copy;
$! = 1;
my $stations;


# Constants used by the script.
# my $INGEST_DIR = "/ingest/ncdc";
# new ingest location March 2012
my $INGEST_DIR = "/net/ftp/pub/data/incoming/ncdc_soundings";
my $STN_FILE = "../docs/NWS_station.list";
my $IVEN_DIR = "../docs/inventory";
my $ARCHIVE_DIR = "../archive/ascii_files";

&main();

##------------------------------------------------------------------------
# @signature void main(int month, int year)
# <p>Inventory the ASCII files for the specified month and year.</p>
#
# @input $month The last month to include in the inventory.
# @input $year The year being inventoried.
##------------------------------------------------------------------------
sub main() {
    if (scalar(@ARGV) != 2) {
	printf("Usage: NWS_ascii_archive.pl MM YYYY\n");
	exit(1);
    }

    my ($month,$year) = @ARGV;

    read_station_list();
    my $iven_dates = archive_ascii($year,$month);

    foreach my $iven_year (keys(%{ $iven_dates})) {
	foreach my $iven_month (keys(%{ $iven_dates->{$iven_year}})) {
	    create_inventory($iven_year,$iven_month);
	}
    }
}

##------------------------------------------------------------------------
# @signature (int year, int month) adjust_month(int year, int month, int offset)
# <p>Get a month and year from the specified offset of another month and year.</p>
#
# @input $year The year to be offset from.
# @input $month The month to be offset from.
# @input $offset The offset to be applied to the specified month and year.
##------------------------------------------------------------------------
sub adjust_month {
    my ($year,$month,$offset) = @_;

    $month += $offset;
    while ($month > 12) { $year++; $month -= 12; }
    while ($month < 01) { $year--; $month += 12; }

    return ($year,$month);
}

##------------------------------------------------------------------------
# @signature Hash* archive_ascii()
# <p>Move all of the files in the INGEST_DIR to its appropriate ARCHIVE_DIR.
# This will create all necessary directories to archive the files.</p>
#
# @output $iven_dates A hash reference of dates by year and month that had
# new files archived.
##------------------------------------------------------------------------
sub archive_ascii() {
    my ($year,$month) = @_;
    my @files = read_ingest_directory();
    my $iven_dates = {};

    # Make sure the specified year and month get inventoried!
    $iven_dates->{$year}->{$month} = 1;

    foreach my $file (@files) {
	# Rename the file to its gzipped file name.
	$file = gzip($INGEST_DIR,$file) if ($file !~ /\.(Z|gz)$/i);

	$file =~ /^(\d{5})(\d{2})\.(\d{2})/;
	my ($wban,$file_year,$file_month) = ($1,$2,$3);

	# Only archive files that are not newer than the processing month/year
	if (sprintf("20%02d%02d",$file_year,$file_month) <= sprintf("%04d%02d",$year,$month)) {
	    if (defined($stations->{$wban})) {
		my $archive_dir = sprintf("%s/20%02d/%s",$ARCHIVE_DIR,$file_year,$stations->{$wban}->{"id"});
		create_directory($archive_dir) unless (-e $archive_dir);
		my ($iven_year,$iven_month) = archive_ascii_file($archive_dir,$file,$year,$month);
		$iven_dates->{$iven_year}->{$iven_month} = 1;
	    } else {
		printf("Cannot find station for WBAN %d.  Ignoring file: %s/%s\n",$wban,$INGEST_DIR,$file);
	    }
	}
    }

    return $iven_dates;
}

##------------------------------------------------------------------------
# @signature (int,int) archive_ascii_file(String archive_dir, String file, int year, int month)
# <p>Copy the file into its archive directory for the specified month and
# year.  This will make sure that it is not the same as another month file
# in the directory before copying it over.  If this is different than any
# of the month files, it renames it be appending a number to the file name.</p>
#
# @input $archive_dir The directory where the file is to be archived.
# @input $file The file to be archived.
# @input $year The year of the archive file.
# @input $month The month of the archive file.
# @output $file_year The year from the file name in YYYY format.
# @output $file_month The month from the file name in MM format.
##------------------------------------------------------------------------
sub archive_ascii_file {
    my ($archive_dir,$file,$year,$month) = @_;

    # Parse out the file date and suffix from the file name.
    $file =~ /^(\d{5}\d{2}\.\d{2})(\.\d+)?(.+)$/;
    my ($file_date,$file_suffix) = ($1,$3);
    
    # Read in all of the files that are for the same year and month as the specified file.
    opendir(my $ARCH,$archive_dir) or die("Cannot read archive directory: $archive_dir\n");
    my @like_files = grep(/^$file_date/,readdir($ARCH));
    closedir($ARCH);

    # Compare the specified file with the archived files.
    if (scalar(@like_files) > 0) {

	# Try to find exact matches to the file.
	my $found = 0;
	foreach my $like_file (@like_files) {	
	    if (system(sprintf("diff %s/%s %s/%s",$INGEST_DIR,$file,$archive_dir,$like_file)) == 0) {
		$found = 1;
		last;
	    }
	}

	# Copy the file with an incremented count if the specified file was not in the archive.
	if (!$found) {
	    my $new_file = sprintf("%s.%d%s",$file_date,scalar(@like_files),$file_suffix);
	    copy(sprintf("%s/%s",$INGEST_DIR,$file),sprintf("%s/%s",$archive_dir,$new_file)) or 
		die("Cannot copy $INGEST_DIR/$file to $archive_dir/$new_file\n");
	}

	# Remove the file from ingest if it is not to be saved there
	if (is_save_file($INGEST_DIR,$file,$year,$month) == 0) {
	    unlink(sprintf("%s/%s",$INGEST_DIR,$file)) or die("Cannot delete file: $INGEST_DIR/$file\n");
	}
    }
    # No files to compare, so just copy it over to the archive
    else {
	my $new_file = sprintf("%s%s",$file_date,$file_suffix);
	copy(sprintf("%s/%s",$INGEST_DIR,$file),sprintf("%s/%s",$archive_dir,$new_file)) or 
	    die("Cannot copy $INGEST_DIR/$file to $archive_dir/$new_file\n");

	# Remove the file from ingest if it is not to be saved there.
	if (is_save_file($INGEST_DIR,$file,$year,$month) == 0) {
	    unlink(sprintf("%s/%s",$INGEST_DIR,$file)) or die("Cannot delete file: $INGEST_DIR/$file\n");
	}
    }

    # Return the file year and month that was archived
    $file_date =~ /^\d{5}(\d{2})\.(\d{2})/;
    return (sprintf("%02d%02d",$1 >= 95 ? 19 : 20,$1),sprintf("%02d",$2));
}

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
        }
        $count++;
    }
}

##------------------------------------------------------------------------
# @signature void create_inventory(int year, int month)
# <p>Create the HTML inventory page of all of the soundings for the specified
# month and year.</p>
#
# @input $year The year to be inventoried.
# @input $month The month to be inventoried.
##------------------------------------------------------------------------
sub create_inventory {
    my ($year,$month) = @_;

    printf("Creating inventory file for: %02d %04d\n",$month,$year);

    # Create the inventory log directory if it doesn't already exist.
    my $iven_dir = sprintf("%s/%04d",$IVEN_DIR,$year);
    create_directory($iven_dir) unless (-e $iven_dir);

    # Get the list of files that have been archived for the year and month.
    my @files = read_archive_directory($year,$month);

    # Make sure there is data to be inventoried.
    if (!@files) {
	printf("No data for %02d %04d.  Inventory not being created!\n",$month,$year);
	return;
    }

    my $data = {};
    
    # Read in all of the sounding data to be inventoried.  Also read in the
    # previous sounding data to find the last known ascension number.
    foreach my $file (@files) {
	get_last_sounding($data,$year,$month,$file);
	parse_metadata($data,$year,$month,split(/\//,$file));
    }

    # Test the data, trim it to the inventory time of interest, and reorganize it for easy use.
    test_data($data,$year,$month);

    # Create the HTML page
    my ($mnt,$day_count) = get_month_data($month,$year);
    open(my $HTML,sprintf(">%s/%s/NWS_ascii_inventory_%04d%02d.html",$IVEN_DIR,$year,$year,$month)) or
        die("Cannot open inventory file.\n");

    # Create the header information
    printf($HTML "<html>\n");
    printf($HTML "<head>\n");
    printf($HTML "   <title>NWS: ASCII MicroArt Inventory: %s %04d</title>\n",$mnt,$year);
    printf($HTML "   <style>\n");
    printf($HTML "<!--\n");
    printf($HTML "      a { font-weight: bold; padding: 0 1em; }\n");
    printf($HTML "      body { font-family: sans-serif; }\n");
    printf($HTML "      font.miss { color: #FF0000; font-weight: bold;  }\n");
    printf($HTML "      font.warn1 { color: #CC00CC; font-weight: bold;  }\n");
    printf($HTML "      font.warn2 { color: #FF9600; font-weight: bold;  }\n");
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

    printf($HTML "<h1 align=\"center\">NWS: ASCII MicroArt Inventory: %s %04d</h1>\n",$mnt,$year);

    print_legend($HTML);
    printf($HTML "<table align=\"center\" border=1>\n");
    print_month_links($HTML,$year,$month);

    # Loop through all of the stations and print out their inventory in a new table row
    my $station_count = 0;
    foreach my $id (sort(keys(%{ $data}))) {

	foreach my $wban (sort(keys(%{ $data->{$id}}))) {

#	    printf("%s",$id);
#	    printf(" (%s)",$wban);
#	    printf(" %s\n",$data->{$id}->{$wban}->{"location"});

	    # Print out a table header line after every 5th station.
	    if ($station_count % 5 == 0) {
		print_table_header($HTML,$day_count);
	    }
	    
	    printf($HTML "   <tr class=\"%s\">\n",$station_count % 2 == 0 ? "evenstation" : "oddstation");
	    printf($HTML "      <td align=\"center\"><b>%s</b></td>\n",$id);
	    printf($HTML "      <td align=\"center\">%s</td>\n",$wban);
	    printf($HTML "      <td align=\"center\">%s</td>\n",$data->{$id}->{$wban}->{"wmo"});
	    printf($HTML "      <td nowrap><b>%s</b></td>\n",$data->{$id}->{$wban}->{"location"});
	    printf($HTML "      <td align=\"center\">%d<br>%d</td>\n",
		   $data->{$id}->{$wban}->{"first_ascension"},$data->{$id}->{$wban}->{"last_ascension"});
	    printf($HTML "      <td class=\"data\">\n");
	    printf($HTML "         <table>\n");
	    
	    # Create a new table to contain the hour data.
	    # Keep track of the count for alternating colors.
	    my $hour_count = 0;
	    foreach my $hour (sort(keys(%{ $data->{$id}->{$wban}->{"hours"}}))) {
		printf($HTML "            <tr class=\"%s\">\n",
		       $station_count % 2 == 0 ? ($hour_count % 2 == 0 ? "eveneven" : "evenodd") : 
		       ($hour_count % 2	 == 0 ? "oddeven" : "oddodd"));
		
		for (my $day = 1; $day <= $day_count; $day++) {
		    printf($HTML "            <td width=20 align=\"center\">%s</td>\n",
			   defined($data->{$id}->{$wban}->{"hours"}->{$hour}->{sprintf("%02d",$day)}) ? 
			   sprintf("<font%s>%s</font>",$data->{$id}->{$wban}->{"hours"}->{$hour}->{sprintf("%02d",$day)},$hour) :
			   "&nbsp;&nbsp;");
		}
		printf($HTML "            </tr>\n");
		$hour_count++;
	    }
	    printf($HTML "         </table>\n");
	    printf($HTML "      </td>\n");
	    printf($HTML "   </tr>\n");
	    
	    $station_count++;
	}
    }


    # Close the table and reprint the table header and legend.
    print_table_header($HTML,$day_count);
    print_month_links($HTML,$year,$month);
    printf($HTML "</table>\n");
    print_legend($HTML);

    printf($HTML "</body>\n");
    printf($HTML "</html>\n");

    close($HTML);
}

##------------------------------------------------------------------------
# @signature void get_last_sounding(Hash* data, int year, int month, String file)
# <p>Search for the last sounding before the specified year and month while
# staying in the current year.  If there are multiple files for the month,
# read both of them since either one could have the latest sounding.</p>
#
# @input $data The Hash reference that contains the sounding metadata.
# @input $year The year of the search.
# @input $month The month of the current data.
# @input $file The file that contains the soundings that the last sounding
#   is being searched for.
##------------------------------------------------------------------------
sub get_last_sounding {
    my ($data,$year,$month,$file) = @_;
    $file =~ /^([^\/]+)\//;
    my $arch_dir = sprintf("%s/%04d/%s",$ARCHIVE_DIR,$year,$1);

    # Loop until a previous file has been found or until the beginning of the year
    # has been reached.
    my @pastfiles = ();
    while (scalar(@pastfiles) == 0 && $month > 1) {
	$month = sprintf("%02d",$month - 1);
	opendir(my $DIR,$arch_dir) or die("Can't read archive directory: $arch_dir\n");
	@pastfiles = grep(/^\d{7}\.$month/,readdir($DIR));
	closedir($DIR);
    }

    # Read the metadata for all of the past sounding files.
    foreach my $pastfile (@pastfiles) {
	parse_metadata($data,$year,$month,$1,$pastfile);
    }
}

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
}

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
}

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
}

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
    system(sprintf("gzip %s/%s",$dir,$file)) == 0 or die("Error gzipping file $dir/$file\n\t$?\n");
    return sprintf("%s.gz",$file);
}

##------------------------------------------------------------------------
# @signature int is_save_file(String dir, String file, int year, int month)
# <p>Determine if the specified file should be saved.</p>
#
# @input $dir The directory where the file is stored.
# @input $file The file to be tested.
# @input $year The year to use for testing the file.
# @input $month The month to use for testing the file.
# @output $save If the file should be saved in its directory.
##------------------------------------------------------------------------
sub is_save_file {
    my ($dir,$file,$year,$month) = @_;
    
    # Uncomment these lines to save data in INGEST_DIR.  Currently set to 3 months.
#    $file =~ /^\d{5}(\d{2})\.(\d{2})/;
#    my $file_date = sprintf("20%02d%02d",$1,$2);
#    my $save_date = sprintf("%04d%02d",adjust_month($year,$month,-3));
#    return ($file_date > $save_date);

    # Do not save any of the files in INGEST_DIR
    # Comment this line out if data is being saved.
    return 0;
}

##------------------------------------------------------------------------
# @signature void parse_metadata(Hash* data, int year, int month, String id, String file)
# <p>Read the metadata in the file and put it into the data hash for each
# individual sounding in the file.</p>
#
# @input $data The Hash reference for holding the individual sounding data.
# @input $year The year of the file.
# @input $month The month of the file.
# @input $id The station id for the file.
# @input $file The file to be parsed.
##------------------------------------------------------------------------
sub parse_metadata {
    my ($data,$year,$month,$id,$file) = @_;

    # Read in the header line for the sounding.
    open(my $FILE,sprintf("zmore %s/%04d/%s/%s |",$ARCHIVE_DIR,$year,$id,$file)) or die("Can't read archived file: $ARCHIVE_DIR/$year/$id/$file\n");
    my @soundings = grep(/\#\# WMO/,<$FILE>);
    close($FILE);

    # Pull out the WBAN number from the file name.
    $file =~ /^(\d{5})/;
    my ($wban) = ($1);

    # Parse out the sounding metadata from each sounding in the file.
    foreach my $sounding (@soundings) {

	$sounding =~ /WBAN:(\d+)\s+YEAR\/MONTH:(\d{2})(\d{2})\s+DAY\/HOUR:(\d{2})(\d{2})\s+ASCENSION\s+NUM:(\d+)/;
	
	if (!defined($2) || !defined($3) || !defined($4) || !defined($5) || !defined($6)) {
	    printf("%s: Can't parse sounding: %s\n",$file,$sounding);
	    next;
	}

	$data->{$wban}->{"wban"} = $1;
	$year = substr($year,2,2) == $2 ? $year : -99;

	# Determine if a sounding was already found at the time of the sounding with a different
	# ascension number.
	if (defined($data->{$wban}->{"soundings"}->{$year}->{$3}->{$4}->{$5})) {
	    if ($data->{$wban}->{"soundings"}->{$year}->{$3}->{$4}->{$5} != substr($6,1)) {
		$data->{$wban}->{"warnings"}->{$year}->{$3}->{$4}->{$5} = " class=warn1";
	    }
	} else {
	    $data->{$wban}->{"soundings"}->{$year}->{$3}->{$4}->{$5} = substr($6,1);
	}

	# Determine if an ascension number was already found at a different time.
	if (defined($data->{$wban}->{"ascensions"}->{substr($6,1)})) {
	    if ($data->{$wban}->{"ascensions"}->{substr($6,1)}->{"year"} != $year ||
		$data->{$wban}->{"ascensions"}->{substr($6,1)}->{"month"} != $3 ||
		$data->{$wban}->{"ascensions"}->{substr($6,1)}->{"day"} != $4 ||
		$data->{$wban}->{"ascensions"}->{substr($6,1)}->{"hour"} != $5) {
		$data->{$wban}->{"warnings"}->{$data->{$wban}->{"ascensions"}->{substr($6,1)}->{"year"}}->{$data->{$wban}->{"ascensions"}->{substr($6,1)}->{"month"}}->{$data->{$wban}->{"ascensions"}->{substr($6,1)}->{"day"}}->{$data->{$wban}->{"ascensions"}->{substr($6,1)}->{"hour"}} = " class=warn2";
		$data->{$wban}->{"warnings"}->{$year}->{$3}->{$4}->{$5} = " class=warn2";
	    }
	} else {
	    $data->{$wban}->{"ascensions"}->{substr($6,1)}->{"year"} = $year;
	    $data->{$wban}->{"ascensions"}->{substr($6,1)}->{"month"} = $3;
	    $data->{$wban}->{"ascensions"}->{substr($6,1)}->{"day"} = $4;
	    $data->{$wban}->{"ascensions"}->{substr($6,1)}->{"hour"} = $5;
	}
    }
}

##------------------------------------------------------------------------
# @signature void print_legend(FileHandle HTML)
# <p>Print out the legend to describe the meaning of the different color
# codes.</p>
#
# @input $HTML The FileHandle where the legend is to be printed.
##------------------------------------------------------------------------
sub print_legend {
    my ($HTML) = @_;

    printf($HTML "<h2>Legend</h2>\n");
    printf($HTML "<ul>\n");
    printf($HTML "   <li><font class=miss>XX</font>- There are one or more soundings missing before time XX.</li>\n");
    printf($HTML "   <li><font class=warn1>XX</font>- There are multiple ascension numbers for time XX.</li>\n");
    printf($HTML "   <li><font class=warn2>XX</font>- The same ascension number has muliple times.  (There should be more than one highlighted time.)  This may be caused by multiple files.</li>\n");
    printf($HTML "</ul>\n");
}

##------------------------------------------------------------------------
# @signature void print_month_links(FileHandle HTML, int year, int month)
# <p>Print out the table row that contains the links to the previous and
# next months.</p>
#
# @input $HTML The FileHandle where the row is to be printed.
# @input $year The current year for the file.
# @input $month The current month for the file.
##------------------------------------------------------------------------
sub print_month_links {
    my ($HTML,$year,$month) = @_;

    my ($prev_year,$prev_month) = get_previous_month($year,$month);
    my ($next_year,$next_month) = get_next_month($year,$month);

    printf($HTML "   <tr>\n");
    printf($HTML "   <td align=\"left\" colspan=5><a href=\"../%04d/NWS_ascii_inventory_%04d%02d.html\">Previous Month</a></td>\n",
	   $prev_year,$prev_year,$prev_month);
    printf($HTML "   <td align=\"right\"><a href=\"../%04d/NWS_ascii_inventory_%04d%02d.html\">Next Month</a></td>\n",
	   $next_year,$next_year,$next_month);
    printf($HTML "   </tr>\n");
}

##------------------------------------------------------------------------
# @signature void print_table_header(FileHandle HTML, int day_count)
# <p>Print out the table row that contains the header information for the
# table.</p>
#
# @input $HTML The FileHandle where teh row is to be printed.
# @input $day_count The number of days to print out in the header. 
##------------------------------------------------------------------------
sub print_table_header {
    my ($HTML,$day_count) = @_;

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
}

##------------------------------------------------------------------------
# @signature String[] read_archive_directory(int year, int month)
# <p>Find the list of files that have been archived for the specified year
# and month.</p>
#
# @input $year The year of the archived files to find.
# @input $month The month of the archived files to find.
# @output file[] The list of files that have been archived for the year and month.
##------------------------------------------------------------------------
sub read_archive_directory {
    my ($year,$month) = @_;
    my $arch_dir = sprintf("%s/%04d",$ARCHIVE_DIR,$year);
    my ($yy,$mm) = (substr($year,2,2),sprintf("%02d",$month));

    my @files = ();

    # Loop through all of the stations for the year.
    opendir(my $ARCH,$arch_dir) or die("Can't read archive directory: $arch_dir\n");
    foreach my $stn (grep(/^[^\.]+$/,readdir($ARCH))) {

	# Get the list of month files for the station.
	opendir(my $STN,sprintf("%s/%s",$arch_dir,$stn)) or die("Can't read archive directory: $arch_dir/$stn");
	my @stnfiles = grep(/^\d{5}$yy\.$mm/,readdir($STN));
	closedir($STN);

	foreach my $file (@stnfiles) {

	    # ------------ SANITY CHECK -------------------------
	    # Make sure that the station can be found in the station list for the file!
	    $file =~ /^(\d{5})/;
	    if ($stations->{$1}->{"id"} ne $stn) {
		printf("WARNING!!!!  Station Id %s (%s) not found in station list!\n",$stn,$1);
	    }

	    push(@files,sprintf("%s/%s",$stn,$file));
	}
    }
    closedir($ARCH);

    return @files;
}

##------------------------------------------------------------------------
# @signature String[] read_ingest_directory()
# <p>Get the list of all of the ASCII files that are in the INGEST_DIR.</p>
#
# @output files[] The list of ASCII files in INGEST_DIR.
##------------------------------------------------------------------------
sub read_ingest_directory {
    opendir(my $INGEST,$INGEST_DIR) or die("Can't read ingest directory: $INGEST_DIR\n");
    my @files = (grep(/^\d{7}\.\d{2}/,readdir($INGEST)));
    closedir($INGEST);
    return @files;
}

##------------------------------------------------------------------------
# @signature void read_station_list()
# <p>Read that data in the station list file into the stations Hash.</p>
##------------------------------------------------------------------------
sub read_station_list {
    open(my $STN,$STN_FILE) or die("Can't read station file: $STN_FILE\n");
    <$STN>; # Read off the header line
    foreach my $line (<$STN>) {
	next if ($line =~ /^\s*$/);

	chomp($line);
	my @data = split(/;/,$line);

	if (defined($stations->{$data[1]})) {
	    printf("Station %s (%s) is already defined in the station list: %s\n",$data[0],$data[1],$line);
	}

	# Set the WMO # to missing if it is not known
	$data[2] = "-9999" if (!defined($data[2]) || $data[2] =~ /^\s*$/);

	$stations->{$data[1]}->{"id"} = trim($data[0]);
	$stations->{$data[1]}->{"wban"} = trim($data[1]);
	$stations->{$data[1]}->{"wmo"} = trim($data[2]);
	$stations->{$data[1]}->{"name"} = trim($data[3]);
	$stations->{$data[1]}->{"start_date"} = trim($data[4]);
	$stations->{$data[1]}->{"end_date"} = trim($data[5]) if (defined($data[5]) && $data[5] !~ /^\s*$/);
	$stations->{$data[1]}->{"rrs_date"} = trim($data[6]) if (defined($data[6]));
    }
    close($STN);
}

##------------------------------------------------------------------------
# @signature void test_data(Hash* data, int year, int month)
# <p>Test the data and remove all of the data not relevant for the time
# of the inventory file.  Also, reorganize the order so the inventory can 
# easily output the data.</p>
#
# @input $data The Hash reference that contains the sounding information.
# @input $year The year of the inventory file.
# @input $month The month of the inventory file.
##------------------------------------------------------------------------
sub test_data {
    my ($data,$year,$month) = @_;

    # Loop through the WBAN numbers that have data.
    foreach my $key (keys(%{ $data})) {

	# Initialize the data for the station id/WMO pair
	my $local_data = {};
	$local_data->{"location"} = $stations->{$key}->{"name"};
	$local_data->{"wban"} = $stations->{$key}->{"wban"};
	$local_data->{"wmo"} = $stations->{$key}->{"wmo"};


	# Determine the first and last ascension number for the station for the given month
	my $last_asc;
	foreach my $number (sort(keys(%{ $data->{$key}->{"ascensions"}}))) {

	    $local_data->{"first_ascension"} = $number if (($year == $data->{$key}->{"ascensions"}->{$number}->{"year"} && 
							    $month == $data->{$key}->{"ascensions"}->{$number}->{"month"}) &&
							   (!defined($local_data->{"first_ascension"}) ||
							    $number < $local_data->{"first_ascension"}));

	    # Search for missing ascension numbers.
	    if ($data->{$key}->{"ascensions"}->{$number}->{"year"} == $year &&
		$data->{$key}->{"ascensions"}->{$number}->{"month"} == $month) {
		if (!defined($last_asc) || $last_asc + 1 == $number) {
		    $last_asc = $number;
		    $local_data->{"hours"}->{$data->{$key}->{"ascensions"}->{$number}->{"hour"}}->
		    {$data->{$key}->{"ascensions"}->{$number}->{"day"}} = "";
		} elsif ($last_asc == $number) {
		    # Do Nothing
		} else {
		    $last_asc = $number;
		    $local_data->{"hours"}->{$data->{$key}->{"ascensions"}->{$number}->{"hour"}}->
		    {$data->{$key}->{"ascensions"}->{$number}->{"day"}} = " class=miss";
		}
	    }
	}
	$local_data->{"last_ascension"} = $last_asc;

	# Make sure the ascension numbers are set if none were found in the inventory month
	$local_data->{"first_ascension"} = -999 if (!defined($local_data->{"first_ascension"}));
	$local_data->{"last_ascension"} = -999 if (!defined($local_data->{"last_ascension"}));

	# Reorganize the warning data for the inventory read
	if (defined($data->{$key}->{"warnings"}->{$year}->{$month})) {
	    foreach my $day (keys(%{ $data->{$key}->{"warnings"}->{$year}->{$month}})) {
		foreach my $hour (keys(%{ $data->{$key}->{"warnings"}->{$year}->{$month}->{$day}})) {
		    $local_data->{"hours"}->{$hour}->{$day} = $data->{$key}->{"warnings"}->{$year}->{$month}->{$day}->{$hour};
		}
	    }
	}

	# Assign the data to the station id and remove the WBAN data
	$data->{$stations->{$key}->{"id"}}->{$key} = $local_data;
	delete($data->{$key});
    }

    # Make sure all of the stations in the station list appear in the data with default values
    # Do not include stations that no longer expect data.
    my $date = sprintf("%04d%02d",$year,$month);
    foreach my $wban (keys(%{ $stations})) {
	if (!defined($data->{$stations->{$wban}->{"id"}}->{$wban}) && 
	    !($stations->{$wban}->{"start_date"} >= $date ||
	      (defined($stations->{$wban}->{"end_date"}) && $stations->{$wban}->{"end_date"} < $date) ||
	      (defined($stations->{$wban}->{"rrs_date"}) && $stations->{$wban}->{"rrs_date"} < $date))) {
	    $data->{$stations->{$wban}->{"id"}}->{$wban}->{"wban"} = $stations->{$wban}->{"wban"};
	    $data->{$stations->{$wban}->{"id"}}->{$wban}->{"wmo"} = $stations->{$wban}->{"wmo"};
	    $data->{$stations->{$wban}->{"id"}}->{$wban}->{"location"} = $stations->{$wban}->{"name"};
	    $data->{$stations->{$wban}->{"id"}}->{$wban}->{"first_ascension"} = -999;
	    $data->{$stations->{$wban}->{"id"}}->{$wban}->{"last_ascension"} = -999;
	}
    }
}

##------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove all of the leading and trailing whitespace from a String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed String.
##------------------------------------------------------------------------
sub trim {
    my ($line) = @_;
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}
