#!/bin/perl 

#-------------------------------------------------------------------------#
# Insert Script which also copies the files from one location to its final 
#   archive location.
#  Author: Phillip Dressen 
#-------------------------------------------------------------------------#

use File::Copy;
$db_path = '/storm/codiac/codiac_db';
$empress = '/usr/empress8.20/rdbms/bin/empcmd';
$ENV{MSPATH} = '/usr/empress8.20/rdbms';
 
# Programs
my $TAR = '/bin/tar -cvf';
my $GZIP = '/opt/bin/gzip';
#my $MV = '/bin/mv -i';
my $MV = '/bin/mv';
$RM = '/bin/rm';
$MSRCP = '/opt/dcs/bin/msrcp -pe 32767 -pr 59110026 -rpwd jossdata -wpwd jossdata';
                        
my $storm_id = "15.900";    # CODIAC Dataset ID Number
my $l_dds_id = $storm_id;

my $start_dir = "/ftp/pub/incoming/pacs/berbery"; # Original file location
#my $end_dir = "/archive/codiac/pacs/model/ETA_Berbery"; # File destination
my $end_dir = "/web/data/pacs/model/ETA_Berbery";


# File containing CODIAC db entries
#my $output_file = "/home/drphil/codiac_datasets/ds$storm_id";

# Code for subdirectory organization
my $sub_dir_size = 3;
# 0 - No subdirectories
# 1 - Use original subdirectory structure
# 2 - Create new subdirectories by year
# 3 - Create new subdirectories by month
# 4 - Create new subdirectories by day

# Expected File Name Regular Expression
my $file_match = '^pp\d{10}_(18|24|30|36)\.gif$'; 

my $file_name_len = 19;     # Expected File Name Length

#- Variables for Correctly Parsing Date/Time -#

# character string appended to the beginning of dates
# Use if filename has two digit years and "19" or "20" needs to appear in 
# the four digit year 
my $date_prefix = "";

# Field Length and intermediate spacing for date/time information 
# in the file name
my $year_skip_ch = 2;
my $year_len     = 4;
my $mon_skip_ch  = 6;
my $mon_len      = 2;
my $day_skip_ch  = 8;
my $day_len      = 2;
my $b_hr_skip_ch = 0;
my $b_hr_len     = 0;
my $b_mn_skip_ch = 0;
my $b_mn_len     = 0;
my $e_hr_skip_ch = 0;
my $e_hr_len     = 0;
my $e_mn_skip_ch = 0;  
my $e_mn_len     = 0;

my $end_time_ch  = 0;   # 0 if begin and end dates are the same

# 1 - Use the date information below instead of parsing from filenames 
my $use_static_date = 0;
my $static_begin = 19000101;
my $static_end   = 19000102;

# 1 - Use the time information below instead of parsing from filenames
my $use_static_times = 1;
my $static_b_hr  = "00";
my $static_b_min = "00";
my $static_e_hr  = "23";
my $static_e_min = "59";

#                    ---- Not Currently Implemented ----
#   Use Julian Days - Set to 1 if days should be parsed as Julian Days
my $use_julian = 0;
# my $julian_skip_ch = 0;          # Number of characters to skip 
# my $julian_len = 3;              # Length of Julian Date

# Static File Logical format 
# list of values are in phys_dir_db:logical_format:logical_fmt
# Or see the file /storm/codiac/codiac_db/logical_fmt.list
#
#  0 = Unknown 
# 15 = VHS Video Tape 
# 34 = Color Graphics Interchange Format (GIF) 
# 42 = ISO-9660 CD
# 50 = PostScript 
# 51 = zip file 
# 53 = Unix Tape ARchive (TAR) Format 
# 55 = GNU Zip (.gz) Format 
# 57 = TAR/GNU Zip (.tar.gz/.tgz) 
# 60 = Joint Photographic Experts Group (JPEG) 
# 68 = PDF 
# 75 = Binary 
# 79 = TIFF 
# 83 = HTML Format

my $logical_fmt = 34;

# Media Code Value 
# code values are in sdmc_order_db:media_code:media_code 
# Or see the file /storm/codiac/codiac_db/media_code.list 
# 10 = Magnetic Disk 
# 11 = VHS Video Tape 
# 12 = Magneto-Optical Disk (Jukebox) 
# 17 = CD-ROM 

$media_code = 12;

# The Physical Format 
# phys_format values are in data_dict_db:physical_format:phys_fmt
# Or see the file /storm/codiac/codiac_db/phys_fmt.list
#  	 0 = Unknown
#  	 1 = Binary (use this for GIF)
#  	 2 = ASCII
#  	 5 = VHS Video Tape
#        7 = JPEG 
#        8 = PDF 
#       13 = TIFF 

$phys_fmt = 1;

my $hw_address = "localhost";  # Hardware Address
my $archive_date = "today";    # Archive Date

# This line actually doesn't matter for this dataset 
# because the desc is set based on four possible  
# descs later in this file, this is just a default 
# name in case of failure 
my $data_desc = "PACS ETA South American Model Forecasts";

# Run Time Messages / Email Settings
#  Message Types - Determines how you view error messages.
#	Options: 
#       0  -  Displayed on screen. 
#       1  -  Emailed to specified user.

my $mess_type = 1;
my $email_to  = "To: janine\@ucar.edu";  # Send Errs to

# Misc Sendmail Stuff
my $sendmail  = "/usr/lib/sendmail -t";
my $email_reply = "Reply-to: janine\@ucar.edu";
my $email_subject = "Subject: Automated Script for ftp PACS data to CODIAC";
my $email_msg  = 	  "Automated Script\n" 
					. "Storm ID: $storm_id \n"
					. "Start Dir: $start_dir\n"
					. "End Dir:   $end_dir\n\n";

my $err_msg = 0;
my $errors = "";

# Declaration of Parsed Variables
my $begin_date;
my $end_date;
my $b_hour;
my $e_hour;
my $b_min;
my $e_min;
my $data_amt;
my $dir_path;
my $file_name;

# Couple of values for holding the min/max dates
$min_date = "";
$max_date = "";

# Loop Through all of the files 
my @unused_files;
my $flag_tar = 0;

chdir( $start_dir ) || die "Can't change to $start_dir";

opendir(TOPDIR, ".") || die "Can't open directory with the files\n";
@dir = readdir(TOPDIR);
closedir(TOPDIR);

@dir = reverse(sort(@dir));
foreach $filename (@dir) {
    if (-d $filename && $filename ne "." && $filename ne "..") {
	push(@sub_dir, $filename);
    }
}
undef @dir;

#$out_file = $output_file;
#open (OUTFILE, ">$out_file") || die "Can't open $out_file for output";
#
$working_dir = $start_dir;

while (defined($working_dir)) {
    print "processing the $working_dir directory...\n";
    chdir($working_dir) || die "can't change to $working_dir";

    opendir(WDIR, ".") || die "Can't open directory with the files\n";
    @dir = readdir(WDIR);
    closedir(WDIR);

    @dir = sort(@dir);

    my $curr_sub_dir = substr( $working_dir, length($start_dir) ) ;

    foreach $filename (@dir) {

		my $name_len = length($filename);
		if( $filename =~ /$file_match/ && $name_len == $file_name_len ) {

	    	# Parse Date and Times from the Filenames

	    	if( $use_static_date == 1 ) {
			# Static Dates
				$begin_date = $static_begin;
				$end_date = $static_end;

	    	} elsif ( $use_julian == 1 ) {
				# Julian Dates - Not Implemented
				# Write Julian Date Code Here
				# $julian_skip_ch, $julian_len fields

	    	} else {
				# Regularly parsed dates
				$begin_date = $date_prefix 
					. substr( $filename, $year_skip_ch, $year_len )
					. substr( $filename, $mon_skip_ch, $mon_len )
					. substr( $filename, $day_skip_ch, $day_len );
				$end_date = $date_prefix
        			. substr( $filename, $year_skip_ch + $end_time_ch, $year_len )
        			. substr( $filename, $mon_skip_ch + $end_time_ch, $mon_len )  
        			. substr( $filename, $day_skip_ch + $end_time_ch, $day_len );
	    	}

	    	# Use Static Begin and End Times
	    	if( $use_static_times == 1 ) {
				$b_hour = $static_b_hr;
				$b_min  = $static_b_min;
				$e_hour = $static_e_hr;
				$e_min  = $static_e_min;

				# Parse Begin and End times
	    	} else {
				$b_hour = substr( $filename, $b_hr_skip_ch, $b_hr_len ); 
				$b_min  = substr( $filename, $b_mn_skip_ch, $b_mn_len );
				$e_hour = substr( $filename, $e_hr_skip_ch, $e_hr_len );
				$e_min  = substr( $filename, $e_mn_skip_ch, $e_mn_len );
	    	}

	    	#                        Special to this Dataset                
	    	#   Save the min and max values for the data encountered, 
            #           for updating the dataset description.
	    	if( $min_date eq "" or $begin_date < $min_date ) {
				$min_date = $begin_date;
	    	} 
	    	if( $max_date eq "" or $end_date > $max_date ) {
				$max_date = $end_date;
	    	}
	    
	    	# Determine the Size of the File in Kilobytes 
	    	$data_amt = int ((stat($filename))[7] / 1024);
	    	$data_amt = 1 if ($data_amt == 0);
	    
	    	# Copy the file to / from the correct locations 
            # 0 - No subdirectories
            # 1 - Use original subdirectory structure
            # 2 - Create new subdirectories by year
            # 3 - Create new subdirectories by month
            # 4 - Create new subdirectories by day
	    	my $f_sub_dir = "";
	    	if( $sub_dir_size == 0 ) {
				$f_sub_dir = "";
		
	    	} elsif ( $sub_dir_size == 1 ) { 
				$f_sub_dir = $curr_sub_dir;

	    	} elsif ( $sub_dir_size == 2 ) {
				$f_sub_dir = "/" . substr( $begin_date, 0, 4 );

	    	} elsif ( $sub_dir_size == 3 ) {
				$f_sub_dir = "/" . substr( $begin_date, 0, 6 );

	    	} elsif ( $sub_dir_size == 4 ) {
				$f_sub_dir = "/" . substr( $begin_date, 0, 8 );
	    	} 
	
	    	$dir_path = $end_dir . $f_sub_dir; # . "/" . $filename;
	    	my $from_dir = $start_dir . $curr_sub_dir . "/" . $filename;

	    	#print "cp " . $from_dir . " " . $dir_path . "\n";

	    	chdir($end_dir);

	    	if( !( -e ( "." . $f_sub_dir )  ) ) {
				#print "MAKING SUBDIR!!! $f_sub_dir\n\n";
				mkdir( $end_dir . $f_sub_dir );
				$flag_tar = 1;	
	    	} 

		my $result;
		if( !(-e ($dir_path . "/" . $filename) ) ) {
		
	    		###--#####--###
	    		# Send Email if copy fails
	    		###--#####--###
	    		$result = copy( $from_dir, $dir_path . "/" . $filename ); # Copy ret 1 for success
	    		if( $result == 0 ) {
				$err_msg = 1;
				$errors = $errors . "ERROR: Copy Failed\n"
			              . "\tFile: $filename\n" 
						  . "\tFrom: " . $from_dir . "\n" 
						  . "\tTo:   " . $dir_path 
						  . "/" . $filename . "\n\n" 
						  . "Script Failed to complete\n\n";
                		stopOnErr();
	 	   	}			

	    		###--#####--###
	    		# Send Email if remove fails
	    		###--#####--###
	    		$result = system("$RM -f $from_dir");
	    		if( $result ) {
				$err_msg = 1;
				$errors = $errors . "ERROR: Remove Failed\n"
						. "\tFile: $filename\n" 
                		. "\tFrom: " . $from_dir
                        	. "/" . $filename . "\n"
                		. "Script Continued\n\n";
	    		}
		} else {
			$err_msg = 1;
			$errors = $errors . "ERROR: File Already Exists\n"
				. "\tFile: $filename\n"
				. "\tCopy Skipped\n"
				. "Script Continued\n\n";
		}
			
	    	# Special to this Dataset
	    	# Changing the Dataset Description Based on the Content 
            # of the filename
            #
	    	# Comment out if this section is unnecesary. 
	    	my $desc_skip_ch = 13;
	    	my $desc_len	 =  2;
	    	if( substr( $filename, $desc_skip_ch, $desc_len) eq '18' ) {
				$data_desc = "PACS 18 Hour Forecast Imagery";
		
	    	} elsif ( substr( $filename, $desc_skip_ch, $desc_len) eq '24' ) {
				$data_desc = "PACS 24 Hour Forecast Imagery";
		
	    	} elsif ( substr( $filename, $desc_skip_ch, $desc_len) eq '30' ) {
				$data_desc = "PACS 30 Hour Forecast Imagery";

	    	} elsif ( substr( $filename, $desc_skip_ch, $desc_len) eq '36' ) {
				$data_desc = "PACS 36 Hour Forecast Imagery";
	    	}

	    	# Add File To The Database 
	    	$file_name = $filename;

	    	InsertRecord();
	    	#WriteRecord2File();

	    	#  Special to this Dataset #
	    	#  Create tar files for a month (when it is 
	    	#  complete), gzip the tar file, 
	    	#  load the zipped tar ball on to mass store 
	    	#  and delete the tar-ball. 

	    	if( $flag_tar == 1 ) {
				chdir( $end_dir );

				$curr_mon = substr( $begin_date, 4, 2 );
				$curr_year = substr( $begin_date, 0, 4 );
		
				$curr_mon--;
				if( $curr_mon == 0 ) {
			    	$curr_mon = 12;
			    	$curr_year--;
				}
				if( $curr_mon < 10 ) {
				    $tar_month = ($curr_year . "0") . $curr_mon;
				} else {
	    		    $tar_month = $curr_year . $curr_mon;
				}

				if( -d( $tar_month ) ) {
    		    	chdir( $end_dir . "/" . $tar_month );
	
				    $result = system("$TAR $tar_month.tar *");
					if( $result ) {
						$err_msg = 1;
						$errors = $errors . "ERROR: Couldn't tar for month: $tar_month\n"; 
				    	die "Couldn't tar for month: $tar_month"; 
					}

				    $result = system("$GZIP $tar_month.tar");
					if( $result ) {
    	    	        $err_msg = 1;
    	    	        $errors = $errors . "ERROR: Couldn't gzip, File: $tar_month.tar";
						die "Couldn't gzip, File: $tar_month.tar";
    	    	    }
			
				    $result = copy("$tar_month.tar.gz", "../");
					if( !$result ) {
    	    	        $err_msg = 1;
    	    	        $errors = $errors . "ERROR: Couldn't copy, File: $tar_month.tar.gz\n";
    	    	        die "Couldn't copy, File: $tar_month.tar.gz";
    	    	    }

			    	$result = system("$MSRCP $tar_month.tar.gz mss:/JOSS/DATA/RAW/BY_PROJECT/PACS/ETA/$tar_month.tar.gz"); 

			    	###--#####--###
			    	# Create an ERROR message if the copy to Mass Store Fails
			    	###--#####--###
			    	if( $result ) {
						$err_msg = 1;
    	    			$errors = $errors . "ERROR: MASS Store Copy Failed\n"
    	        	    	. "\tFile: $tar_month.tar.gz\n"
    	        	    	. "\tFrom: " . $end_dir . "/" . $tar_month
       	                 	. "/$tar_month.tar.gz\n"
       		         		. "\tTo:   mss:/JOSS/DATA/RAW/BY_PROJECT/PACS/ETA\n\n"
            	    		. "Script Continued, tar file moved to: $end_dir\n\n";

						# Preserve the tar-ball, but move to the parent dir
	    				$result = system("$MV $tar_month.tar.gz ../");
	    			} else {
						# Everythings okay so rm tarball
						$result = system("$RM $tar_month.tar.gz");
						if ( $result ) {
							$errors = $errors . "WARNING: Tarball Removal Failed\n"
								. "\tFile: $tar_month.tar.gz\n"
	                	    	. "\tFrom: " . $end_dir . "/" . $tar_month
	                	    	. "/" . $tar_month.tar.gz . "\n"
	                	    	. "Script Continued\n\n";
			    		}
					}

					$flag_tar = 0;
   	  			}
			}
		} else {
        	# If files don't match the expected properties, add 
        	# them to a list of unused files.  
			if( !( $filename eq "." or $filename eq ".." ) ) {
				push( @unused_files , $filename );
	    	}
		} 

	}

    $working_dir = pop(@sub_dir); # Advance to next directory
}

#-----------------------------------------------------------------------------#
#                          Special to this Dataset                            #
# Update the dataset description so it accurately reflects the correct begin  #
#      and end dates as well as the correct 'last_updated' field.             #
#-----------------------------------------------------------------------------#
$sql = "\"SELECT begin_date, end_date FROM dataset WHERE storm_id=$storm_id\"";
@results = `$empress $db_path/catalog_db $sql`;
@old_dates = split( ' ', $results[2]);
#print $old_dates[0] . "\n" . $old_dates[1];

# Update the begin date
if( $old_dates[0] > $min_date ) {
	$sql = "\"UPDATE dataset SET begin_date=\'$min_date\', "
			. "date_last_update=today "
			. "WHERE storm_id=\'$storm_id\'\"";
	print $sql;
	system("$empress $db_path/catalog_db $sql");
}
# Update the end date
if( $old_dates[1] < $max_date ) {
    $sql = "\"UPDATE dataset SET end_date=\'$max_date\', "
            . "date_last_update=today "
            . "WHERE storm_id=\'$storm_id\'\"";
	print $sql;
    system("$empress $db_path/catalog_db $sql");
}

###--#####--###
# Loop Through all of the unused files 
# Send an Email if there are any files of concern 
###--#####--###
foreach $filename (@unused_files) {
	$err_msg = 1;	
	$errors = $errors . 'WARNING: ' . $filename . 
		" did not match expected filename parameters\n";
}
my $num = @unused_files;
$errors = $errors . "\n $num files did not match the expected file parameters."; 

# Send an email at the end of execution if necesary
# I know this is better to have at the beginning of the program stylistically
# But when I'm trying to get this worked out quickly, I didn't want to deal with
# any variable scoping issues that may occur from moving this to the top.
END {
	if( $err_msg == 1 ) {
		if( $mess_type == 1 ) {
			$email_msg = $email_msg . $errors;
			sendEmail();
		} else {
			print $errors;
		}
	}
}

# A Subroutine for entering entries straight into CODIAC             #
# Don't Use 'Unless you Really, REALLY Mean It'                  #
sub InsertRecord {
                        
    # Exit if a Required Field is left NULL
    if( ($l_dds_id eq "") ||
        ($logical_fmt eq "") ||
        ($hw_address eq "") ||
        ($dir_path eq "") ||
        ($file_name eq "") ) {
            print "\nInvalid Operation: Required Field Left NULL\n";
            print $l_dds_id . "\n";
            exit(0);
    }
    
    $sql = "\"INSERT INTO on_line_phys_dir(" .
                        "l_dds_id, " .
                        "begin_date, " .
                        "b_hour, " .
                        "b_min, " .
                        "end_date, " .
                        "e_hour, " .
                        "e_min, " .
                        "data_desc, " .
                        "logical_fmt, " .
                        "data_amt, " .
                        "media_code, " .
                        "phys_fmt, " .
                        "hw_address, " .
                        "dir_path, " .
                        "file_name, " .
                        "archive_date )"   .
            " VALUES\( " .
                        "\'" . $l_dds_id        . "\', " .
                        "\'" . $begin_date      . "\', " .
                        "\'" . $b_hour          . "\', " .
                        "\'" . $b_min           . "\', " .
                        "\'" . $end_date        . "\', " .
                        "\'" . $e_hour          . "\', " .
                        "\'" . $e_min           . "\', " .
                        "\'" . $data_desc       . "\', " .
                        "\'" . $logical_fmt     . "\', " .
                        "\'" . $data_amt        . "\', " .
                        "\'" . $media_code      . "\', " .
                        "\'" . $phys_fmt        . "\', " .
                        "\'" . $hw_address      . "\', " .
                        "\'" . $dir_path        . "\', " .
                        "\'" . $file_name       . "\', " .
                        "\'" . $archive_date    . "\' )\;\"";
    
	#print( "$empress $db_path/phys_dir_db $sql\n\n");
	system("$empress $db_path/phys_dir_db $sql");   
                        
	#print $sql . "\n";
}

# A Subroutine for sending email updates to the status the script 
sub sendEmail {
	open( SENDMAIL, "|$sendmail" ) || die "Unable to open sendmail";

	print SENDMAIL $email_reply . "\n";
	print SENDMAIL $email_subject . "\n";
	print SENDMAIL $email_to . "\n\n\n";
	#print SENDMAIL "Content-type: test/plain\n\n\n";
	print SENDMAIL $email_msg . "\n\n\n";
	close(SENDMAIL);
}

# A Subroutine for handling some errors which cause the script to halt
sub stopOnErr {
	if( $mess_type == 1 ) {
		$email_msg = $email_msg . $errors;
		sendEmail();
	} else {
		print $errors;
	}
	exit(0);
}

