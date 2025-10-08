#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The MySqlInserter module is a class that inserts files into the <code>
# file</code> table of the database.  It is dependant on a configuration
# file that is passed to the <code>insert</code> function.  The formating 
# for the config file is found in the cfg-template file.</p>
#
# <p>This is meant to handle most naming conventions for file names.  <b>It
# does not handle all of them.</b>  In cases where this modules cannot handle
# the file names, it can be extended and the <code>set_dates(MySqlFile, String,
# int)</code> function can be overridden to handle it.</p>
#
# <p>The following lists the functionality of the module:<ul>
#   <li>Multiple File Name Conventions:  The config file is designed to be
# able to insert multiple file types/patterns for a single dataset at a single
# time.  This allows all of the file information for the dataset to be defined
# in a single file.</li>
#   <li>Flexible Date Extraction:  The config file is designed to be able
# to specifiy where the date is located in the file name and with the use of
# grouping, defines which dates/times the date is for.</li>
#   <li>All or Nothing Insert:  The module is designed to do an all or nothing
# insert of the files.  This means that if any occurs with the inserting of
# the files into the database, none of the files will be inserted.  (It only
# commits (saves) the data once all of the files have been inserted without 
# errors.  A rollback (undo) is done if any occurs before the commit is 
# finalized.)</li>
#   <li>Hourly Time Rounding:  <p>The module does not handle minutes and seconds.
# This is a design choice discussed by Joel and Linda.  This is to provide a
# range to people who order the data to actually be able to obtain it, without
# having to know the exact minutes or seconds in the file name.</p>
# <p>How this works:  The begin and end times use the hours provided/derived
# from the information provided in the config file.  The begin minutes and seconds
# are both set to 00 and the end minutes and seconds are set to 59.  This
# ensures the file is define over its full time period with the potential of
# it covering a few extra minutes before or after it to fill in the rest of
# the hour.</p>
# </ul></p>
#
# @author Joel Clawson
# @version 1.2 Changed the error handling to capture 0 size file warnings and
#    display them at the end instead of dying on them.  This will allow a user
#    to still insert all of the files with size.
#
# @author Joel Clawson
# @version 1.1 Update the pattern matcher to only replace YMDJh characters with
#    digit characters when they are between pattern groups.  Also updated the
#    pattern group matches to end at the first match of the closing group char
#    and not the last group char in the file name pattern.
# 
# @author Joel Clawson
# @version 1.0 Original Creation.
##Module--------------------------------------------------------------------
package MySqlInserter;
use strict;
use lib "/work/software/conversion_modules/Version6";
use lib "/net/work/software/conversion_modules/Version6";
use DpgDate qw(:DEFAULT);
use MySqlDatabase;
use MySqlFile;

##--------------------------------------------------------------------------
# @signature void create_files(int index, String directory, String[]* files)
# <p>Create a new MySqlFile for each file that is to be inserted into
# the database.</p>
# @warning This function is recursive.  It will read all of the directories
# in the directory it was given (except for . and ..) and try to find more
# files that match the pattern.
#
# @input $index The index of the data to use from the config file.
# @input $directory The directory to be read.
# @input $files A reference to an array that contains the MySqlFile instances.
##--------------------------------------------------------------------------
sub create_files {
    my ($self,$index,$directory,$files) = @_;

    printf("Processing directory: %s\n",$directory);

    opendir(my $DIR,$directory) or die("Cannot read $directory\n");
    my @dir_files = readdir($DIR);
    closedir($DIR);

    my $pattern = $self->{"cfg"}->{$index}->{"pattern"};
    $pattern =~ s/\+/\.\+/;

    foreach my $file (@dir_files) {

	# Process directories under this directory
	if (-d sprintf("%s/%s",$directory,$file) && $file !~ /^\.+$/) { 
	    $self->create_files($index,sprintf("%s/%s",$directory,$file),$files);
	} elsif ($file =~ /^$pattern$/) {
	    # Process files that match the pattern.
	    my $mysql = MySqlFile->new();
	    
	    $self->set_dates($mysql,$file,$index);
	    $mysql->setDatasetId($self->{"cfg"}->{$index}->{"dataset_id"});
	    $mysql->setFile($directory,$file);
	    $mysql->setFormatId($self->{"cfg"}->{$index}->{"format"});

	    # Add the new file to the list.

	    # Get the acceptable range of dates to be inserted.
	    my @range = ();
	    if (defined($self->{"cfg"}->{$index}->{"insertrange"})) {
		@range = split(/:/,$self->{"cfg"}->{$index}->{"insertrange"});
	    } else {
		@range = ("0000-00-00","9999-99-99");
	    }
	    
	    # Get the file date range
	    my @date = ((split(' ',$mysql->getBeginDate()))[0],
			(split(' ',$mysql->getEndDate()))[0]);

	    # Only insert the files in the defined date range
	    if (compareDates($range[0],"YYYY-MM-DD",$date[0],"YYYY-MM-DD") >= 0 &&
		compareDates($date[1],"YYYY-MM-DD",$range[1],"YYYY-MM-DD") >= 0) {
		push(@{ $files},$mysql);
	    }
	}
    }
}

##--------------------------------------------------------------------------
# @signature void insert(String cfg_file)
# <p>Insert files into the database based on the parameters defined in the
# configuration file.</p>
#
# @input $cfg_file The name of the configuration file used to enter the files.
##--------------------------------------------------------------------------
sub insert {
    my $self = shift;
    
    # Make sure the config file is given to the function
    if (scalar(@_) != 1) {
	printf("Usage: MySqlInserter::insert needs config file.\n");
	exit(1);
    }
    my ($cfg_file) = @_;

    # Read the data from the config file
    $self->read_config_file($cfg_file);

    # Create the files to be inserted into the database
    my @files;
    foreach my $index (keys(%{ $self->{"cfg"}})) {
	$self->create_files($index,$self->{"cfg"}->{$index}->{"directory"},\@files);
    }

    # Insert the files if they were created.
    if (scalar(@files) > 0) {
	my $database = MySqlDatabase->new("zediupdate","change-456");
	my $err = $database->connect();

	# Quit if the connection could not be created.
	die($err) if ($err ne "");

	# Holder for storing multiple errors
	my $err_report = "";

	# Insert all of the flies.
	foreach my $file (@files) {
#	    printf("Inserting file: %s/%s\n",$file->getDirectory(),$file->getFilename());

            my $directory = $file->getDirectory();
            $directory =~ s/^\/net/\/export/;
            $directory = sprintf("/export%s",$directory) if ($directory =~ /^\/archive/);

            if ($directory =~ /^\/export\/archive\/codiac/) {
               printf("Cannot use /export/archive/codiac, use /export/archive/data instead.  Rolling back previous commands.\n");
               printf("%s",$database->rollback());
               printf("%s",$database->disconnect());
               exit(1);
            }

	    $file->setDirectory($directory);

	    printf("%s %s %s %02d %05d %s/%s\n",$file->getDatasetId(),$file->getBeginDate(),$file->getEndDate(),$file->getFormatId(),$file->getSize(),$file->getDirectory(),$file->getFilename());
	    $err = $file->insert($database);

	    # Undo all of the commands if an error occured.
	    if ($err =~ /has\s+0\s+size/i) {
		$err_report .= $err."\n";
		$err = "";
	    } elsif ($err ne "") {
		printf("%s  Rolling back previous commands.\n",$err);
		printf("%s",$database->rollback());
		printf("%s",$database->disconnect());
		exit(1);
	    }
	}

	if ($err_report ne "") {
	    printf("There were potential errors in the loading:\n\n%s\n\n",$err_report);
	}

	# Provide the user the ability to cancel the commit and insert after timeout
#	my $result = "y";
#	eval {
#	    local $SIG{ALRM} = sub { die("Alarm went off!\n"); };
#
#	    printf("\n\nIn 10 seconds the files will be inserted automatically\n");
#	    printf("Press Enter now to insert now\n");
#	    printf("Press enter after any value to cancel the insert.\n\n");
#	    alarm 10;
#	    
#	    eval {
#		printf("Continue (yes == Enter, no == anything else):\t");
#		$result = <STDIN>;
#	    };
#	    alarm 0;
#	};
#	alarm 0;

	# Provide the user the ability to cancel the commit.
	printf("\n\nTo insert the files, press Enter.\n");
	printf("To cancel the insert, enter any value and press Enter.\n\n");
	printf(">> ");
	my $result = <STDIN>;

	if ($result =~ /^\s*$/) {
	    printf("\n");
	    # Finalize the insert.
	    $err = $database->commit();
	
	    # Undo the commands when the commit fails.
	    if ($err ne "") {
		printf("%s  Rolling back previous commands.\n",$err);
		printf("%s",$database->rollback());
		printf("%s",$database->disconnect());
		exit(1);
	    } else {
		printf("\n\n%d files inserted successfully.\n\n",scalar(@files));
	    }
	} else {
	    printf("\nUser selected to cancel.  Rolling back previous commands\n");
	    printf("%s",$database->rollback());
	    printf("%s",$database->disconnect());
	    exit(1);
	}

	# Close the connection.
	printf("%s",$database->disconnect());
    } else {
	printf("\n\nNo files were found!\n\nNo files were inserted!!\n\n\n");
    }
}

##--------------------------------------------------------------------------
# @signature MySqlInserter new()
# <p>Create a new instance.</p>
#
# @output $self The new MySqlInserter.
##--------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

    return $self;
}

##--------------------------------------------------------------------------
# @signature void read_config_file(String file)
# <p>Parse the relevant information from the configuration file that will
# be used to insert the files into the database.</p>
#
# @input $file The name of the config file.
##--------------------------------------------------------------------------
sub read_config_file {
    my ($self,$file) = @_;
    open(my $CFG,$file) or die("Cannot open $file\n");
    my @data = grep(/^(\{|\})|^(\s*[a-z]+:)/,<$CFG>);
    close($CFG);

#    printf("Config Data:\n");
#    printf("%s\n",join("",@data));

    # Load the config data into the instance.
    my $ind = 0;
    for(my $count = 0; $count < scalar(@data); $count++) {
	my $line = $data[$count];

	# Loop through the bracketed sections
	if ($line =~ /^\{/) {
	    $line = $data[++$count];
	    while ($line !~ /^\}/) {
		$line =~ /^\s*([a-z]+):\s*([\S]+)/;
		$self->{"cfg"}->{$ind}->{$1} = $2;
		$line = $data[++$count];
	    }
	    $ind++;
	}
    }

    # Convert the file name into a string that can be used for pattern matching
    for (my $i = 0; $i < $ind; $i++) {
	my $index = 0;
	my $in_date = 0;
	my $date_bracket = "";
	my $pattern = "";

	# Loop through the length of the file name.
	while ($index < length($self->{"cfg"}->{$i}->{"filename"})) {
	    my $char = substr($self->{"cfg"}->{$i}->{"filename"},$index,1);

	    if (!$in_date) {
		# Look for date groupings
		if ($char eq "[" && $date_bracket eq "") {
		    $in_date = 1;
		    $date_bracket = "]";
		} elsif ($char eq "{" && $date_bracket eq "") {
		    $in_date = 1;
		    $date_bracket = "}";
		} elsif ($char eq "(" && $date_bracket eq "") {
		    $in_date = 1;
		    $date_bracket = ")";

		    # Look for nested date groupings
		} elsif ($char eq "{" || $char eq "[" || $char eq "(") {
		    printf("Bad file pattern: %s.  Brackets don't match.\n",
			   $self->{"cfg"}->{$i}->{"filename"});
		    exit(1);
		} else {
		    $pattern .= $char;
		}
	    } else {
		# Look for the closing of a date group
		if ($char eq $date_bracket) {
		    $in_date = 0;
		    $date_bracket = "";
		} elsif ($char =~ /^[YMDJh]$/) {
		    # Convert the date code to a digit
		    $pattern .= "\\d";
		} else {
		    # Put the pattern char that is not a date/time character.
		    $pattern .= $char;
		}
	    }
	    
	    $index++;
	}

	# Add the pattern to the config set
	$self->{"cfg"}->{$i}->{"pattern"} = $pattern;

        #printf("Pattern: %s\n",$pattern);

	# Add the dataset id to the config set.
	$file =~ /^cfg\-(.+)$/;
	$self->{"cfg"}->{$i}->{"dataset_id"} = $1;

    }
}

##--------------------------------------------------------------------------
# @signature void set_dates(MySqlFile mysql, String filename, int index)
# <p>Set the begin and end dates for the file.</p>
#
# @input $mysql The MySqlFile that will have the dates set.
# @input $filename The name of the file that contains the dates.
# @input $index The index of the file pattern from the config file.
##--------------------------------------------------------------------------
sub set_dates {
    my ($self,$mysql,$filename,$index) = @_;
    my $datepattern = $self->{"cfg"}->{$index}->{"filename"};
    my $century = $self->{"cfg"}->{$index}->{"century"};
    my $filelength = $self->{"cfg"}->{$index}->{"filelength"};

#    printf("%s\n%s\n",$filename,$datepattern);

    # Initialize Date/Time variables
    my ($begin_date,$begin_time,$end_date,$end_time) = ("","","","");
    my ($begin_date_format,$begin_time_format,$end_date_format,$end_time_format) =
	("","","","");

    # Search for dates and times that are for both the start and end times
    if ($datepattern =~ /\([^YMDJ]*([^ABCE-IKLN-XZa-z\d\[\]\{\}\(\)]+)[^YMDJ]*\)/) {
	$begin_date_format = $1;
	$end_date_format = $1;
    } 
    if ($datepattern =~ /\([^h]*([^a-gi-zA-Z\d\[\]\{\}\(\)]+)[^h]*\)/) {
	$begin_time_format = $1;
	$end_time_format = $1;
    }

    # Search for begin dates and times
    if ($datepattern =~ /\[[^YMDJ]*([^ABCE-IKLN-XZa-z\d\[\]\{\}\(\)]+)[^YMDJ]*\]/) {
	$begin_date_format = $1;
    }
    if ($datepattern =~ /\[[^h]*([^a-gi-zA-Z\d\[\]\{\}\(\)]+)[^h]*\]/) {
	$begin_time_format = $1;
    }

    # Search for end dates and times.
    if ($datepattern =~ /\{[^YMDJ]*([^ABCE-IKLN-XZa-z\d\[\]\{\}\(\)]+)[^YMDJ]*\}/) {
	$end_date_format = $1;
    } 
    if ($datepattern =~ /\{[^h]*([^a-gi-zA-Z\d\[\]\{\}\(\)]+)[^h]*\}/) {
	$end_time_format = $1;
    }

    # Pull out the dates and times from the file name.
    my $file_index = 0;
    my $patt_index = 0;
    while ($file_index < length($filename)) {
	my $patt_char = substr($datepattern,$patt_index,1);

	if ($patt_char eq "(") {
	    $patt_index++;
	    while (($patt_char = substr($datepattern,$patt_index,1)) ne ")") {
		if ($patt_char eq uc($patt_char)) {
		    $begin_date = substr($filename,$file_index,length($begin_date_format));
		    $end_date = $begin_date;
		    $file_index += length($begin_date_format);
		    $patt_index += length($begin_date_format);
		} else {
		    $begin_time = substr($filename,$file_index,length($begin_time_format));
		    $end_time = $begin_time;
		    $file_index += length($begin_time_format);
		    $patt_index += length($begin_time_format);
		}
	    }
	    $patt_index++;
	} elsif ($patt_char eq "[") {
	    $patt_index++;
	    while (($patt_char = substr($datepattern,$patt_index,1)) ne "]") {
		if ($patt_char eq uc($patt_char)) {
		    $begin_date = substr($filename,$file_index,length($begin_date_format));
		    $file_index += length($begin_date_format);
		    $patt_index += length($begin_date_format);
		} else {
		    $begin_time = substr($filename,$file_index,length($begin_time_format));
		    $file_index += length($begin_time_format);
		    $patt_index += length($begin_time_format);
		}
	    }
	    $patt_index++;
	} elsif ($patt_char eq "{") {
	    $patt_index++;
	    while (($patt_char = substr($datepattern,$patt_index,1)) ne "}") {
		if ($patt_char eq uc($patt_char)) {
		    $end_date = substr($filename,$file_index,length($end_date_format));
		    $file_index += length($end_date_format);
		    $patt_index += length($end_date_format);
		} else {
		    $end_time = substr($filename,$file_index,length($end_time_format));
		    $file_index += length($end_time_format);
		    $patt_index += length($end_time_format);
		}
	    }
	    $patt_index++;
	} elsif ($patt_char eq "+") {
	    while(length(substr($datepattern,$patt_index)) < 
		  length(substr($filename,$file_index))) { $file_index++; }
	    $file_index++;
	    $patt_index++;
	} elsif ($patt_char eq "\\") {
	    $patt_index += 2;
	    $file_index++;
	} else {
	    $patt_index++;
	    $file_index++;
	}
    }

    # Add the minutes and seconds to the times.
    if ($begin_time ne "") {
	$begin_time .= "0000";
	$begin_time_format .= "MMSS";
    }
    if ($end_time ne "") {
	$end_time .= "5959";
	$end_time_format .= "MMSS";
    }

    # Set the dates and times to the values defined in the config file.
    if ($begin_date eq "" && $end_date eq "") {
	$begin_date = $self->{"cfg"}->{$index}->{"startdate"};
	$begin_time = "00:00:00";
	$end_date   = $self->{"cfg"}->{$index}->{"enddate"};
	$end_time   = "23:59:59";
    } else {
	
	# Set any dates or times that have not yet been defined.
	if ($begin_time eq "") {
	    $begin_time = "000000",$begin_time_format = "hhmmss";
	}
	if ($end_time eq "" && $end_date ne "") {
	    $end_time = "235959";$end_time_format = "hhmmss";
	} elsif ($end_time eq "" && $end_date eq "") {
	    if (defined($filelength)) {
		($end_date,$end_time) = adjustDateTime($begin_date,$begin_date_format,
						       $begin_time,$begin_time_format,
						       split(/,/,$filelength),0,-1);
	    } else {
		($end_date,$end_time) = ($begin_date,$begin_time);
	    }
	    $end_date_format = $begin_date_format;
	    $end_time_format = $begin_time_format;
	}
	
	# Convert the dates and times to an expected format.
	$begin_date = formatDate($begin_date,$begin_date_format,"YYYY-MM-DD");
	$begin_time = formatTime($begin_time,$begin_time_format,"HH:MM:SS");
	$end_date = formatDate($end_date,$end_date_format,"YYYY-MM-DD");
	$end_time = formatTime($end_time,$end_time_format,"HH:MM:SS");
    }

    # Split the dates and times into their individual parts list.
    my @begin = (split('-',$begin_date),split(/:/,$begin_time));
    my @end = (split('-',$end_date),split(/:/,$end_time));

    # Handle the case where there is not a day
    if ($begin[2] == 0 || $end[2] == 0) {
	$begin[2] = 1;
	$end[2] = ((31,daysInFeb($end[0]),31,30,31,30,31,31,30,31,30,31))[$end[1] - 1];
    }

    # Handle 2 digit years.
    if ($begin[0] < 100) {
	if (!defined($century)) {
	    printf("century variable not defined in config file for 2 digit year!\n");
	    exit(1);
	}
	$begin[0] += $century;
    }
    if ($end[0] < 100) {
	if (!defined($century)) {
	    printf("century variable not defined in config file for 2 digit year!\n");
	    exit(1);
	}
	$end[0] += $century;
    }

    # Set the dates/times into the MySqlFile.
    $mysql->setBeginDate(@begin);
    $mysql->setEndDate(@end);
}

##--------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove leading and trailing white space from a string.</p>
#
# @input $line The string to be trimmed.
# @output $line The trimmed string.
##--------------------------------------------------------------------------
sub trim {
    my ($line) = @_;
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}

1;
