#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The <code>NWS_binary_archive.pl</code> script is run once a month (or so)
# to create an inventory of the soundings, in binary form, sent by NCDC that 
# JOSS archives.</p>
# <p>The script does the following processes:<ol>
#   <li>Rename the <code>pgac</code> station files to match the format used
#       by the rest of the stations.  (i.e. pgac1001.S0 -> pgac1001.S0Y where
#       Y is the last digit of the year of the sounding)
#   <li>Gzip all of the binary sounding files that are in the ingest directory
#       that need to be compressed.
#   <li>Copy the binary files into the archive directory by region and station.
#       (i.e. ABQ1001.S04.gz -> $binary_archive/continental_us/albuquerque)
#       Only the files that have not previously been archived are copied.
#   <li>Remove all of the binary files in the ingest directory that over 100
#       days old and are not updated files from the source.  This effectively 
#       maintains the most recent three months of data in the ingest directory.
#   <li>Create an inventory log file for all of the stations that contains:<ul>
#       <li>The missing assension numbers.
#       <li>The duplicate assension numbers.
#       <li>Files with zero length.
#       <li>Stations that do not contain any sounding data.
#       </ul>
#   </ol></p>
#
#
# @author Linda Echo-Hawk 4/2012
# @ version 2.1 <ul>
#   <li>Updated incoming file location from /net/ingest/ncdc to 
# /net/ftp/pub/data/incoming/ncdc_soundings.  NOTE:  Documentation and 
# some functions will still refer to the "ingest" area.  The area for 
# incoming files has moved from /net/ingest to the new /net/ftp location.
# </li> </ul>
#
# @author Joel Clawson 2006/01/30
# @version 2.01 Renamed the inventory directory to ../docs/inventory from ../inventory
#   and the archive directory to ../archive/binary_files from ../binary_files.
#
# @author Joel Clawson 2006/01/26
# @version 2.0 <ul>
#   <li>Changed the archive from year/region/station_name to year/station_id
#   <li>Removed log files that were never looked at.
#   <li>Pointed to the convesion modules (Version 5) and removed the script
# functions that did the same thing.
# </ul>
#
# @author Joel Clawson
# @version 1.1 <p>Changed the algorithm that is used to delete files from ingest
# to use <code>diff</code> between the archived and ingest files.  Also made
# a correction to read in all binary data and check it with the appropriate year.
# The script now copies all files over and renames the different ones by adding
# a number to the end of the file name.
#
# @author Joel Clawson
# @version 1.0 This is an adaptation of a number of scripts that were written
# or at least used by Darren Gallant for handling the archiving and iventorying
# of the NWS binary files.
##Module---------------------------------------------------------------------
use strict;
#use lib "/work/software/conversion_modules/Version5";
use lib "/work/lib/perl/Utilities";
use lib "/net/work/lib/perl/Utilities";
use DpgDate;
use File::Copy;
$! = 1;
#my ($LOG,$UPDATE);
                      
# Global Module Variables
# my $ingest_dir = "/work/ncdc";
# my $ingest_dir = "/ingest/ncdc";
# new ingest location March 2012
my $ingest_dir = "/net/ftp/pub/data/incoming/ncdc_soundings";
my $binary_archive = "../archive/binary_files";
my $binary_iven_log = "../docs/inventory";
my $log_dir = "../logs";

# The hash of stations
#my %stations = ("abq"  => {"station" => "albuquerque","region" => "continental_us"},
#		"abr"  => {"station" => "aberdeen","region" => "continental_us"},
#		"adq"  => {"station" => "kodiak","region" => "alaska"},
#		"akn"  => {"station" => "king_salmon","region" => "alaska"},
#		"alb"  => {"station" => "albany","region" => "continental_us"},
#		"anc"  => {"station" => "anchorage","region" => "alaska"},
#		"ann"  => {"station" => "annette","region" => "alaska"},
#		"ama"  => {"station" => "amarillo","region" => "continental_us"},
#		"apx"  => {"station" => "gaylord","region" => "continental_us"},
#		"bet"  => {"station" => "bethel","region" => "alaska"},
#		"bis"  => {"station" => "bismark","region" => "continental_us"},
#		"bmx"  => {"station" => "birmingham","region" => "continental_us"},
#		"bna"  => {"station" => "nashville","region" => "continental_us"},
#		"boi"  => {"station" => "boise","region" => "continental_us"},
#		"bro"  => {"station" => "brownsville","region" => "continental_us"},
#		"brw"  => {"station" => "barrow","region" => "alaska"},
#		"buf"  => {"station" => "buffalo","region" => "continental_us"},
#		"car"  => {"station" => "caribou","region" => "continental_us"},
#		"cdb"  => {"station" => "cold_bay","region" => "alaska"},
#		"chh"  => {"station" => "chatham","region" => "continental_us"},
#		"chs"  => {"station" => "charleston","region" => "continental_us"},
#		"crp"  => {"station" => "corpus_christi","region" => "continental_us"},
#		"ddc"  => {"station" => "dodgecity","region" => "continental_us"},
#		"dnr"  => {"station" => "denver","region" => "continental_us"},
#		"dra"  => {"station" => "desert_rock","region" => "continental_us"},
#		"drt"  => {"station" => "del_rio","region" => "continental_us"},
#		"dtx"  => {"station" => "detroit","region" => "continental_us"},
#		"dvn"  => {"station" => "quad_city","region" => "continental_us"},
#		"epz"  => {"station" => "elpaso","region" => "continental_us"},
#		"eyw"  => {"station" => "key_west","region" => "continental_us"},
#		"fai"  => {"station" => "fairbanks","region" => "alaska"},
#		"ffc"  => {"station" => "atlanta","region" => "continental_us"},
#		"fgz"  => {"station" => "flagstaff","region" => "continental_us"},
#		"fwd"  => {"station" => "fortworth","region" => "continental_us"},
#		"ggw"  => {"station" => "glasgow","region" => "continental_us"},
#		"gjt"  => {"station" => "grand_junction","region" => "continental_us"},
#		"grb"  => {"station" => "green_bay","region" => "continental_us"},
#		"gso"  => {"station" => "greensboro","region" => "continental_us"},
#		"gyx"  => {"station" => "portland","region" => "continental_us"},
#		"iad"  => {"station" => "sterling","region" => "continental_us"},
#		"iln"  => {"station" => "cincinnati","region" => "continental_us"},
#		"ilx"  => {"station" => "lincoln","region" => "continental_us"},
#		"inl"  => {"station" => "internat_falls","region" => "continental_us"},
#		"jan"  => {"station" => "jackson","region" => "continental_us"},
#		"jax"  => {"station" => "jacksonville","region" => "continental_us"},
#		"lbf"  => {"station" => "north_platte","region" => "continental_us"},
#		"lch"  => {"station" => "lake_charles","region" => "continental_us"},
#		"lit"  => {"station" => "littlerock","region" => "continental_us"},
#		"lix"  => {"station" => "slidell","region" => "continental_us"},
#		"lkn"  => {"station" => "elko","region" => "continental_us"},
#		"lwx"  => {"station" => "sterling","region" => "continental_us"},
#		"lzk"  => {"station" => "littlerock","region" => "continental_us"},
#		"maf"  => {"station" => "midland","region" => "continental_us"},
#		"mcg"  => {"station" => "mcgrath","region" => "alaska"},
#		"mfl"  => {"station" => "miami","region" => "continental_us"},
#		"mfr"  => {"station" => "medford","region" => "continental_us"},
#		"mhx"  => {"station" => "morehead_city","region" => "continental_us"},
#		"mkcg" => {"station" => "grand_cayman","region" => "central_america"},
#		"mkg"  => {"station" => "grand_cayman","region" => "central_america"},
#		"mkp"  => {"station" => "barbados","region" => "central_america"},
#		"mkpb" => {"station" => "barbados","region" => "central_america"},
#		"mkpp" => {"station" => "trinidad","region" => "central_america"},
#		"mpx"  => {"station" => "chanhassen","region" => "continental_us"},
#		"mzbz" => {"station" => "belize_city","region" => "central_america"},
#		"nkx"  => {"station" => "san_diego","region" => "continental_us"},
#		"nst"  => {"station" => "pago_pago","region" => "pacific_island"},
#		"nstu" => {"station" => "pago_pago","region" => "pacific_island"},
#		"oak"  => {"station" => "oakland","region" => "continental_us"},
#		"oax"  => {"station" => "omaha","region" => "continental_us"},
#		"ohx"  => {"station" => "nashville","region" => "continental_us"},
#		"okx"  => {"station" => "upton","region" => "continental_us"},
#		"ome"  => {"station" => "nome","region" => "alaska"},
#		"otx"  => {"station" => "spokane","region" => "continental_us"},
#		"otz"  => {"station" => "kotzebue","region" => "alaska"},
#		"oun"  => {"station" => "norman","region" => "continental_us"},
#		"pabe" => {"station" => "bethel","region" => "alaska"},
#		"pabr" => {"station" => "barrow","region" => "alaska"},
#		"pacd" => {"station" => "cold_bay","region" => "alaska"},
#		"padq" => {"station" => "kodiak","region" => "alaska"},
#		"pafa" => {"station" => "fairbanks","region" => "alaska"},
#		"pafc" => {"station" => "anchorage","region" => "alaska"},
#		"pakn" => {"station" => "king_salmon","region" => "alaska"},
#		"pamc" => {"station" => "mcgrath","region" => "alaska"},
#		"pant" => {"station" => "annette","region" => "alaska"},
#		"paom" => {"station" => "nome","region" => "alaska"},
#		"paot" => {"station" => "kotzebue","region" => "alaska"},
#		"pasn" => {"station" => "st_paul_island","region" => "alaska"},
#		"paya" => {"station" => "yakutat","region" => "alaska"},
#		"pbz"  => {"station" => "pittsburgh","region" => "continental_us"},
#		"pga"  => {"station" => "guam","region" => "pacific_island"},
#		"pgac" => {"station" => "guam","region" => "pacific_island"},
#		"pgum" => {"station" => "guam","region" => "pacific_island"},
#		"phl"  => {"station" => "lihue","region" => "pacific_island"},
#		"phli" => {"station" => "lihue","region" => "pacific_island"},
#		"pht"  => {"station" => "hilo","region" => "pacific_island"},
#		"phto" => {"station" => "hilo","region" => "pacific_island"},
#		"pit"  => {"station" => "pittsburgh","region" => "continental_us"},
#		"pmk"  => {"station" => "majuro","region" => "pacific_island"},
#		"pmko" => {"station" => "majuro","region" => "pacific_island"},
#		"ptk"  => {"station" => "chuuk","region" => "pacific_island"},
#		"ptkk" => {"station" => "chuuk","region" => "pacific_island"},
#		"ptp"  => {"station" => "ponape","region" => "pacific_island"},
#		"ptpn" => {"station" => "ponape","region" => "pacific_island"},
#		"ptr"  => {"station" => "koror","region" => "pacific_island"},
#		"ptro" => {"station" => "koror","region" => "pacific_island"},
#		"pty"  => {"station" => "yap","region" => "pacific_island"},
#		"ptya" => {"station" => "yap","region" => "pacific_island"},
#		"pwak" => {"station" => "wake_island","region" => "pacific_island"},
#		"rap"  => {"station" => "rapid_city","region" => "continental_us"},
#		"rev"  => {"station" => "reno","region" => "continental_us"},
#		"riw"  => {"station" => "riverton","region" => "continental_us"},
#		"rnk"  => {"station" => "roanoke","region" => "continental_us"},
#		"sgf"  => {"station" => "springfield","region" => "continental_us"},
#		"sgx"  => {"station" => "san_diego","region" => "continental_us"},
#		"shv"  => {"station" => "shreveport","region" => "continental_us"},
#		"sil"  => {"station" => "slidell","region" => "continental_us"},
#		"sju"  => {"station" => "san_juan","region" => "central_america"},
#		"slc"  => {"station" => "salt_lake_city","region" => "continental_us"},
#		"sle"  => {"station" => "salem","region" => "continental_us"},
#		"snp"  => {"station" => "st_paul_island","region" => "alaska"},
#		"tae"  => {"station" => "tallahasse","region" => "continental_us"},
#		"tbw"  => {"station" => "tampa_bay","region" => "continental_us"},
#		"tfx"  => {"station" => "great_falls","region" => "continental_us"},
#		"tlh"  => {"station" => "tallahasse","region" => "continental_us"},
#		"top"  => {"station" => "topeka","region" => "continental_us"},
#		"tus"  => {"station" => "tucson","region" => "continental_us"},
#		"uil"  => {"station" => "quillayute","region" => "continental_us"},
#		"unr"  => {"station" => "rapid_city","region" => "continental_us"},
#		"yak"  => {"station" => "yakutat","region" => "alaska"});


# Run the script.
&main();

##------------------------------------------------------------------------
# @signature void main(int month, int year)
# <p>Take an inventory of the NWS soundings that were FTP'd to JOSS.</p>
#
# @input $month The month to be inventoried.
# @input $year The year of the month.
##------------------------------------------------------------------------
sub main {
    if (scalar(@ARGV) != 2) {
	printf("Usage: ncdc_inventory.pl MM YYYY\n");
	exit(1);
    }

    my ($month, $year) = @ARGV;

    organizeBinary($month,$year);
    createInventory($month,$year);
}

##------------------------------------------------------------------------
# @signature (int year, int month) adjustMonth(int year, int month, int offset)
# <p>Get a month and year from the specified offset of another month and year.</p>
#
# @input $year The year to be offset from.
# @input $month The month to be offset from.
# @input $offset The offset to be applied to the specified month and year.
##------------------------------------------------------------------------
sub adjustMonth {
    my $year = shift;
    my $month = shift;
    my $offset = shift;

    $month += $offset;
    while ($month > 12) { $year++; $month -= 12; }
    while ($month < 01) { $year--; $month += 12; }

    return ($year,$month);
}

##------------------------------------------------------------------------
# @signature void achiveBinaryFile(String dir, String file)
# <p>Move the binary file from the FTP directory to a final archive
# location.</p>
#
# @input $dir The directory to place the file.
# @input $file The name of the file to be moved.
# @warning This function will die if the file cannot be moved.
##------------------------------------------------------------------------
sub archiveBinaryFile {
    my $dir = shift;
    my $file = shift;

    my $start = sprintf("%s/%s",$ingest_dir,$file);
    my $end = sprintf("%s/%s",$dir,$file);

    copy($start,$end) || die("Cannot move $start to $end\n");
}

##------------------------------------------------------------------------
# @signature void createDirectory(String path)
# <p>Create the directory structure specified in the path.</p>
#
# @input $path The path to be created.
# @warning This function will die if it cannot make a directory.
##------------------------------------------------------------------------
sub createDirectory {
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
# @signature void createInventory(int month, int year)
# <p>Create a log file for the inventory of the binary files for the
# specified month and year.  It will list the missing and duplicate
# assension numbers along with the zero length files.</p>
#
# @input $month The month being inventoried.
# @input $year The year of the month being inventoried.
# @warning This function will die if it cannot open one of the directories
# containing the files to be inventoried.
##------------------------------------------------------------------------
sub createInventory {
    my $month = shift;
    my $year = shift;
    my ($BIN,$REG,$INVEN);

    # Define the directory to contain the inventory log files.
    my $dir = sprintf("%s/%04d",$binary_iven_log,$year);
    createDirectory($dir) if (!(-e $dir));

    my $bin_dir = sprintf("%s/%04d",$binary_archive,$year);
    opendir($BIN, $bin_dir) or die("Cannot read directory: $bin_dir\n");

    open($INVEN, sprintf(">%s/NWS_binary_inventory_%04d%02d.txt",$dir,$year,$month)) or
	die("Cannot open inventory file.\n");

    # Loop through all of the regions.
    foreach my $station (sort(grep(/^[^\.]+$/,readdir($BIN)))) {
	inventoryStation($station,$bin_dir,$INVEN);
    }
    closedir($BIN);

    # Print out a seperator line
    for (my $i = 0; $i < 60; $i++) { printf($INVEN "*"); } printf($INVEN "\n");

    # Search for completely missing stations
    my @missing = ();
#    foreach my $stn (keys(%stations)) {
#	if (!(-e sprintf("%s/%s/%s",$bin_dir,$stations{$stn}{"region"},$stations{$stn}{"station"}))) {
#	    push(@missing,sprintf("%s/%s",$stations{$stn}{"region"},$stations{$stn}{"station"}));
#	}
#    }

    # Print out the missing stations and make sure only one gets printed out for multiple
    # station ids.
    my $prev = "";
    foreach my $gone (sort(@missing)) {
	if ($gone ne $prev) {
	    printf($INVEN "Station directory (%s) is missing.\n",$gone);
	}
	$prev = $gone;
    }

    close($INVEN);

    unlink(sprintf(">%s/NWS_binary_inventory_%04d%02d.txt",$dir,$year,$month)) 
	if (-z sprintf(">%s/NWS_binary_inventory_%04d%02d.txt",$dir,$year,$month));
}

##------------------------------------------------------------------------
# @signature void deleteFileFromIngest(String file)
# <p>Delete the file from the ingest directory.</p>
#
# @input $file The name of the file to be deleted.
##------------------------------------------------------------------------
sub deleteFileFromIngest {
    my $file = shift;

    # Remove the file from the ingest directory.
    unlink(sprintf("%s/%s",$ingest_dir,$file));
}

##------------------------------------------------------------------------
# @signature String gzip(String dir, String file)
# <p>Run the system gzip command on the file in the directory and return
# the name of the gzipped file.</p>
#
# @input $dir The directory containing the file.
# @input $file The name of the file to be gzipped.
# @output $zfile The name of the gzipped file.
##------------------------------------------------------------------------
sub gzip {
    my $dir = shift;
    my $file = shift;

    system(sprintf("gzip -f %s/%s",$dir,$file));
    return sprintf("%s.gz",$file);
}

##------------------------------------------------------------------------
# @signature void inventoryStation(String station, String dir, FILE* OUT)
# <p>Take an inventory of the files for the specified station.  It will find
# missing assension numbers, duplicate assension numbers, and files that 
# have zero length.  All of the information will be printed to the OUT 
# file.</p>
#
# @input $station The station to use to take the inventory.
# @input $dir The directory contaning the station directory.
# @input $OUT The open file pointer to the log file.
# @warning The function will die if it is unable to open the station
# directory containing the files to be inventoried.
##------------------------------------------------------------------------
sub inventoryStation {
    my $station = shift;
    my $dir = shift;
    my $OUT = shift;
    my $DIR;

    my %valids = ();
    my @zeros = ();

    opendir($DIR,sprintf("%s/%s",$dir,$station)) or 
	die("Cannot open directory: $dir/$station\n");

    my $max_assen = 0;
    foreach my $file (sort(readdir($DIR))) {

	if ($file !~ /^\.+$/) {

	    $file =~ /^\D+(\d)(\d{3})/;
	    my ($drop,$assen) = ($1,$2);
  
	    printf("%s\n",$file) if (!defined($assen));

	    # Add the assension number in the file name to the collection.
	    if (defined($valids{$assen})) { $valids{$assen}++; }
	    else { $valids{$assen} = 1; }

	    # Find the gzipped files that are really zero length when not gzipped.
	    my $size = (stat(sprintf("%s/%s/%s",$dir,$station,$file)))[7];
	    if ($size < 33) { push(@zeros,$file); }
  
	    $max_assen = $assen > $max_assen ? $assen : $max_assen;
	}
    }
    closedir($DIR);

    # Find missing and duplicate assension numbers.
    my $assen = "001";
    my @missings = ();
    my @dupes = ();

    # Loop through all of the assension numbers.
    while ($assen <= $max_assen) {

	# Find the duplicates
	if (defined($valids{$assen}) && $valids{$assen} > 1) { push(@dupes,$assen); }

	# Find the missing numbers
	elsif (!defined($valids{$assen})) { push(@missings,$assen); }

	# Need to format the number for the key in the hash
	$assen = sprintf("%03d",$assen + 1);
    }


    # Only continue if there is data to print to the log file.
    return if (scalar(@missings) == 0 && scalar(@dupes) == 0 && scalar(@zeros) == 0);

    # Print out the station information.
    printf($OUT "%s/%s\n",$dir,$station);

    # Print out the missing assension numbers.
    if (scalar(@missings) > 0) {
	printf($OUT "\t------ Missing Assension Numbers:\n\t");
	my $count = 0;
	foreach my $gone (sort(@missings)) {
	    $count++;
	    printf($OUT " %03d",$gone);
	    printf($OUT "\n\t") if ($count % 12 == 0);
	}
	printf($OUT "\n");
    }

    # Print out the duplicate assension numbers.
    if (scalar(@dupes) > 0) {
	printf($OUT "\t------ Duplicate Assension Numbers:\n\t");
	my $count = 0;
	foreach my $dupe (sort(@dupes)) {
	    $count++;
	    printf($OUT " %03d",$dupe);
	    printf($OUT "\n\t") if ($count % 14 == 0);
	}
	printf($OUT "\n");
    }

    # Print out the zero sized files.
    if (scalar(@zeros) > 0) {
	printf($OUT "\t------ Zero Sized Files:\n\t");
	my $count = 0;
	foreach my $zero (sort(@zeros)) {
	    $count++;
	    printf($OUT " %-15s",$zero);
	    printf($OUT "\n\t") if ($count % 3 == 0);
	}
	printf($OUT "\n");
    }

    # Add a blank line to seperate stations.
    printf($OUT "\n",$dir,$station);
}

##------------------------------------------------------------------------
# @signature int isUpdateFile(String file, int month, int year)
# <p>Determine if the file is a file that may need to be updated in the
# archive.</p>
#
# @input $file The file being checked.
# @input $month The current processing month.
# @input $year The year for the processing month.
##------------------------------------------------------------------------
sub isUpdateFile {
    my $file = shift;
    my $drop_date = shift;
    my $save_date = shift;
    my $dir = shift;

    my $archive_file = sprintf("%s/%s",$dir,$file);
    my $ingest_file = sprintf("%s/%s",$ingest_dir,$file);
    my $diff_file = "bin_archive_file.diff";

    unlink($diff_file) if (-e $diff_file);

    # The file has already been archived.
    if (-e $archive_file) {

	my $file_part = substr($file,0,11);
	opendir(my $DIR,$dir) or die("Cannot open $dir\n");
	my @files = grep(/$file_part/,readdir($DIR));
	closedir($DIR);

	foreach my $archived (@files) {

	    system(sprintf("diff %s/%s %s > %s",$dir,$archived,$ingest_file,$diff_file));
	    
	    # The ingest file and archive file are the same.
	    if ((-s $diff_file) == 0) {
		unlink($diff_file);
		return 0;
	    } else {
		# Need to unlink the diff file for the next file in the loop
		unlink($diff_file);
	    }
	}

	# The ingest file is different from all of the archive file.
	unlink($diff_file);
	return 1;
    }

    # The file has not yet been archived.
    else {
	return 1;
    }
}


##------------------------------------------------------------------------
# @signature void organizeBinary(int month, int year)
# <p>Perform the task of organizing the binary sounding files.</p>
#
# @input $month The month to be organized.
# @input $year The year of the month.
# @warning This function will die if the station id is not in the hash.
##------------------------------------------------------------------------
sub organizeBinary {
    my $month = shift;
    my $year = shift;

    # Find the Binary files in the ingest directory.
    my @files = readIngestDirectoryForBinary($month,$year);

    my ($next_year,$next_month) = adjustMonth($year,$month,1);
    my ($save_date,$save_time) = adjustDateTime(sprintf("%04d/%02d/%02d",$next_year,$next_month,1),
						"YYYY/MM/DD","00:00","HH:MM",0,-100,0,0);
    my ($temp_year,$temp_month,$temp_day) = split('\/',$save_date);
    my $save_julian = convertJulian($temp_year,$temp_day,$temp_month);
    $save_date = sprintf("%04d%03d",$temp_year,$save_julian);

    # Loop through all of the Binary files.
    foreach my $file (@files) {
	$file =~ /^(\D+)(\d+).[S|s](\d{2})/;
	my $stn_id = lc($1);
	my $drop_no = substr($2,1);
	my $drop_yr = sprintf("20%02d",$3);

	# Make sure the station is known before continuing.
#	die(sprintf("Cannot find station: %s for file %s.\n",$stn_id,$file)) 
#	    if (!defined($stations{$stn_id}));

	my $drop_julian = int(($drop_no + 1) / 2);
	my $drop_date = sprintf("%04d%03d",$drop_yr,$drop_julian);

	# Define the directory of the file.
	my $dir = sprintf("%s/%s/%s",$binary_archive,$drop_yr,uc(substr($stn_id,0,3)));

	# Gzip the file if it is not already gzipped.
	$file = gzip($ingest_dir, $file) if ($file !~ /\.gz$/);
	
	# Create the directory for the file if it doesn't exist.
	createDirectory($dir) if (!(-e $dir));
	
	if (isUpdateFile($file,$drop_date,$save_date,$dir)) {

#	    if ($drop_yr != $year) {
#		printf("Update to file %s: not in the current year!!!\n",$file);
#	    }

	    if (-e sprintf("%s/%s",$dir,$file)) {
		opendir(my $DIR,$dir) or die("Cannot open dir $dir\n");
		my $file_part = substr($file,0,11);
		my @files = grep(/$file_part/,readdir($DIR));
		closedir($DIR);
		my $new_file = sprintf("%s.%d.gz",$file_part,@files+1);

		move(sprintf("%s/%s",$ingest_dir,$file),
		     sprintf("%s/%s",$dir,$new_file));
	    } else {
		archiveBinaryFile($dir,$file);
	    }
	}
	    
	deleteFileFromIngest($file);
    }
}

##------------------------------------------------------------------------
# @signature String[] readIngestDirectoryForBinary(int month, int year)
# <p>Find all of the files in the ingest directory that are binary files.</p>
#
# @input $month The month to be found.
# @input $year The year of the month.
# @output files[] The names of the binary files in the ingest directory.
##------------------------------------------------------------------------
sub readIngestDirectoryForBinary {
    my $month = shift;
    my $year = shift;
    my $INGEST;
    my @files;

    opendir($INGEST, $ingest_dir) || die("Cannot read $ingest_dir\n");
    foreach my $file (sort(readdir($INGEST))) {
	
	# Rename the pgac files to match the pattern of the other binary files.
	$file = renamePgacFile($file,$month,$year) 
	    if ($file =~ /^pgac\d+\.S0$/);

	# Find the binary files that may or may not be gzipped.
	if ($file =~ /^\D+\d{4}\.S\d{2}/i) { push(@files,$file); }
    }
    closedir($INGEST);

    return @files;
}

##------------------------------------------------------------------------
# @signature String renamePgacFile(String file, int month, int year)
# <p>Rename a pgac station's file to match the pattern of the rest of the
# binary files that JOSS receives.</p>
# 
# @input $file The old file name to be converted.
# @input $month The month of the files.
# @input $year The year of the files.
# @warning This function will die if the function cannot move the file.
##------------------------------------------------------------------------
sub renamePgacFile {
    my $file = shift;
    my $month = shift;
    my $year = shift;

    # Parse out the sounding number for determining the year.
    $file =~ /^pgac\d(\d+)(\.S0)$/;

    # These records are for the next year.
    if ($month == 12 && $1 < 100) { return; }

    # Define the new name of the file.
    my $new_name = sprintf("%s%01d",$file,substr($year,3,1));

    # Move the file.
    move(sprintf("%s/%s",$ingest_dir,$file),
	 sprintf("%s/%s",$ingest_dir,$new_name)) || 
	     die("Cannot move $file to $new_name\n");

#    printf($LOG "Renamed file %s to %s\n",$file,$new_name);

    return $new_name;
}
