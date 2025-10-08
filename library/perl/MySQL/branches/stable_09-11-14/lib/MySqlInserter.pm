#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDatabase.pm>Link to MySqlDatabase.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDataset.pm>Link to MySqlDataset.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlFile.pm>Link to MySqlFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSFile.pm>Link to MySqlMSSFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlInserter.pm>Link to MySqlInserter.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSInserter.pm>Link to MySqlMSSInserter.pm</a><br />
#
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
# to specifiy where the date is located in the directory or file name and with
# the use of grouping, defines which dates/times the date is for.  It can capture
# years, months (number or abbreviation), days, hours, and minutes.  Seconds
# cannot be captured.</li>
#   <li>All or Nothing Insert:  The module is designed to do an all or nothing
# insert of the files.  This means that if any occurs with the inserting of
# the files into the database, none of the files will be inserted.  (It only
# commits (saves) the data once all of the files have been inserted without
# errors.  A rollback (undo) is done if any occurs before the commit is
# finalized.)</li>
#  
# </ul></p>
#
# @author Amanda Orin
# @version 2.6.1: <p>Updated MySqlInserter::read_config_file to check database server to see if the database id is really a BSIERP id (245.B##-###).</p>
#
#
# @author Sean Stroble
# @version 2.6: Added "rolloverbefore" option
# Example:
# "rolloverbefore: 12:00:00" will force a day increment on files where the begin and end times are before 12:00:00
# Useful for split flights which rollover midnight like:
# RF01A.20110526.182536_012510.nc
# RF01B.20110526.012511_035232.nc
# With rolloverbefore: 12:00:00 the second file will be inserted with a date of 2011-05-27 rather than 2011-05-26
#
#
# @author Sean Stroble
# @version 2.5.1 <p>Updated MySqlFile::setFile to accept an extra argument, the size of the file. MySqlInserter now grabs the file size from the directory listing and sends it
# to setFile avoiding calling hsi again for each file just to get the file size (speeds things up a bit).</p>
#
# @author Sean Stroble
# @version 2.5 <p>Updated to support HPSS via hsi commands. Also a selectAll function was added to this release. selectAll returns a hash with all rows, column types and
# columns names. </p>
#
# @author Sean Stroble
# @version 2.4.1 <p>Added complete support for seconds (\S meta character as well as filelength in seconds).
# Note: \S will <b>ONLY</b> be treated as a seconds digit <b>WITHIN A () or {} or []</b> otherwise it will be normally (\S is a non-white space character in standard REGEX). </p>
#
# @author Sean Stroble
# @version 2.4 <p>MySql File now checks to see if the end date is before the begin date and both are on the same day,
#	if it is then the end date is incremented by one day. (Corrects for midnight roll overs)</p>
#
# @author Sean Stroble
# @version 2.3 <p>Database server may now be specified in the configuration filename.
#	For example cfg-datasetID-merlot would insert into the test database on merlot.eol.ucar.edu .
#	Added the ability to specify variable length event strings. Any values captured
#	by a  capture group containing \E{ \E* or \E+ will be appended to the event string.</p>
#
# @author Pierce Martin
# @version 2.2 <p>Added the ability to extract event strings from file names. 
#    The capturing of events from a filename is very similar to capturing dates.
#    Instead of using the date meta characters (such as \Y \M \y etc..) the character \E is used.
#    The \E character should still be placed inside of brackets, parenthesis, or square brackets
#    so that the program knows to look for a meta character. 
#
# @author Joel Clawson
# @version 2.1 <p>Corrected the creation of the begin date/time to allow it to
#    use times after 23:59:59 to roll to the next day.</p>
#
# @author Joel Clawson
# @version 2.0 <p>This is a major upgrade to boost the date/time parsing abilities
#    of the script.  It also adds a few other minor upgrades as well.</p>
#    <ul>
#        <li><p>The date/time parsing has been reworked to allow the user to use
#            Perl pattern matching instead of the reduced modified Perl matching
#            the script was initial developed with.  This prevents backward
#            compatability with config files used with older versions of the
#            script.</p>
#            <p>Because of the Perl pattern matching ability, the date/time
#            characters were changed to be escaped characters not already defined
#            in Perl.  They are \Y - Year, \M - Month, \y - Day, \H - Hour,
#            \m - Minute, \J - Julian Day, and \V - Abbreviated Month.  This also
#            forces all date/time capture groups to only consist of date/time
#            characters, but allows for the use of multiple capture groups.</p>
#            <p>The change in the pattern matching allowed for some changed to the
#            startdate, enddate, and filelength config file keys.  The startdate
#            must exist in every configuration block unless every date/time value
#            (excluding seconds) can be determined from the directory/filename
#            pattern matching.  The enddate or filelength (never both) must be
#            defined unless every date/time value (excluding seconds) can be
#            determined from the directory/filename pattern matching.  The filelength
#            key can only be defined if no end date information exists from another
#            source.  All of these restrictions prevent conflicts between the dates
#            in the script.</p>
#            <p>The startdate and enddate keys have been changed to allow the use
#            of the date/time characters.  This allows the user to specify all of
#            the remaining date/time parts that are not defined in the date/time
#            (i.e. 2006-MM-yy 00:00:00 will expect to read only the month and day
#            from the date/time).  The date/time characters in the date key must
#            be the same character used in the date parsing (without the escaping).</p>
#            <p>The filelength key has been expanded to allow more usablity to the
#            user.  The user now provides a number and frequency (i.e. 1 month,
#            7 days, 12 hours, etc).  The frequency is flexible and will allow plural
#            case-insensitive form of year, month, day, hour and minute.  This should
#            allow any time frame a file could cover over a minute in length.</p>
#            <p>The script prevents itself from getting confused by ensuring that
#            only a single date part is defined for each date (i.e. only one start
#            year).  The script will throw a message to the user and quit if any
#            date/time part is defined more than once.  (This includes making sure
#            that a month is not defined as both a number and an abbrevation and
#            making sure that a month and do not exist when there is a julian day
#            defined.)</p>
#            <p>The pattern matching has been expanded to include the directory.
#            This allows both date/time and general patterns to be in the
#            directory config file key.  The script will search any directories
#            that match the pattern, but cannot match an indefinite amount of
#            directories between two other directories.  (i.e. You cannot have
#            /export/archive/data/name/.+/some_directory and find all of the
#            some_directory instances under /export/archive/data/name.)</p>
#            <p>The date/time parsing now allows \m for minutes and \V for month
#            abbreviations.  The minutes have been implemented to allow for
#            better metadata in the database.  The month abbreviations will
#            allow the user to use capilization insensitive patterns (i.e.
#            jan, feb, etc.) for the month instead of requiring a number.  There
#            are a few 4 character abbreviations (june, july, and sept) that
#            will match with the \V where all other months only will match the
#            3 character abbreviation.</p>
#            <p>The date/time also knows how to adjust the date and time when the
#            time is over 23:59:59.  A date with a time having at least an hour
#            of 24 will advance to the correct time on the next day.  This is
#            specifically designed with aircraft data in mind when a flight ends
#            in the early hours of the next day, but is defined in terms of the
#            start time.</p>
#            </li>
#        <li><p>New configuration keys have been defined while others are now ignored.
#            The new keys of host, purpose, and recursedirectories have been
#            added while the century key is now ignored.</p>
#            <p>The host key now defines where the files are stored (localhost
#            or mass_store).  This allows the inserter module and scripts to be
#            merged into a single module and script allowing a user to only be aware
#            of the single script and make future develop and maintanence a bit
#            easier.</p>
#            <p>The purpose key was added to allow the script to be able to insert
#            multiple documents for a dataset or to allow a single config file to
#            contain all of the file insert information for the dataset.</p>
#            <p>The recursedirectories key is a flag to allow the user to turn off
#            directory recursion below the location defined in the directory key.
#            This is not required and defaults to true.</p>
#            <p>The century key is now ignored.  The functionality has been included
#            in the startdate and enddate keys and is no longer necessary.</p>
#            </li>
#    </ul>
#
# @author Janine Goldstein
# @version 1.3 Changed default root dir from /export to /net to coincide with zedi
#	upgrade to zedi8.
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

use lib "/net/work/lib/perl/Utilities";
use lib "/net/work/lib/perl/hpss";

use HPSS;
use DpgDate qw(:DEFAULT);
use MySqlDatabase;
use MySqlFile;
use MySqlMSSFile;

my $MONTH_ABRV_PATTERN = "[Jj][Aa][Nn]|[Ff][Ee][Bb]|[Mm][Aa][Rr]|[Aa][Pp][Rr]|[Mm][Aa][Yy]|[Jj][Uu][Nn][Ee]?|[Jj][Uu][Ll][Yy]?|[Aa][Uu][Gg]|[Ss][Ee][Pp][Tt]?|[Oo][Cc][Tt]|[Nn][Oo][Vv]|[Dd][Ee][Cc]";

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

    $self->{'debug'} = 0;

	return $self;
}

##--------------------------------------------------------------------------
# @signature String[] create_directory_list(String block_number, String base_directory, String[] path_segments)
# <p>Determine the list of directories that match the directory pattern.</p>
#
# @input $block_number The index of the config file block being processed.
# @input $base_directory The directory currently being processed.
# @input path_segments[] The list of remaining parts of the directory path
# that still need to be matched.
# @output directories The list of directories that match the directory pattern.
##--------------------------------------------------------------------------
sub create_directory_list {
	my ($self,$block_number,$base_directory,@path_segments) = @_;
	my @directories = ();
	my $next_segment = shift(@path_segments);
	my @found_directories = ();

    # Read the directory and search for the next segment for the path.
    if ($self->is_mass_store_block($block_number)) {
	my @results = grep(/^d.+\s+$next_segment$/,HPSS::ls($base_directory,"-l"));

	foreach my $result (@results) {
        print "create_directory_list: HPSS result = ${result}\n" if ($self->{'debug'});
	    push(@found_directories,(split(' ',$result))[8]);
	}
    } else {
	opendir(my $DIR,$base_directory) or die("Can't find directory: $base_directory\n");
	@found_directories = grep(/^$next_segment$/,readdir($DIR));
    print "create_directory_list: local results:\n", join("\n\t",@found_directories), "\n" if ($self->{'debug'});
	closedir($DIR);
    }

    # Loop through all of the matching segments.
    foreach my $directory (sort(@found_directories)) {
    print "create_directory_list: looking at directory ${directory}\n" if ($self->{'debug'});
	
	# Skip the current and parent directories if they match the segment pattern.
	next if ($directory =~ /^\.+$/);

	my $next_directory = sprintf("%s/%s",$base_directory,$directory);

	# Only care if the next directory is truly a directory
	if ((!$self->is_mass_store_block($block_number) && -d $next_directory) ||
	    ($self->is_mass_store_block($block_number))) {
	    # Recursively dive through the path while there are more segments
	    if (@path_segments) {
          print "create_directory_list: going to recurse on ${next_directory}\n" if ($self->{'debug'});
		push(@directories,$self->create_directory_list($block_number,
							       $next_directory,
							       @path_segments));
	    }
	    # This directory is the end of the path (no more segments) so add it to the list
	    else {
          print "create_directory_list: adding directory to list: ${next_directory}\n" if ($self->{'debug'});
		push(@directories,$next_directory);
	    }
	}

    }

    return @directories;
}

##--------------------------------------------------------------------------
# @signature void create_files(int block_number, String directory, MySqlFile[]* files)
# <p>Create the MySQLFile objects that contain the information for the files
# to be inserted into the database.</p>
#
# @input $block_number The index of the config file block of the files to be
# created.
# @input $directory The directory to be processed.
# @input $files A reference to an array where the files are to be stored.
##--------------------------------------------------------------------------
sub create_files {
    my ($self,$block_number,$directory,$files,$database) = @_;

    printf("Create Files for Directory: %s\n",$directory) if ($self->{'debug'});

    # Determine which directories match the pattern specified by the user.
    my @path_segments = split(/\//,$self->{"cfg"}->{$block_number}->{"dir_pattern"});
    shift(@path_segments); # Remove the empty piece from the directory pattern starting with a /.
    print "create_files: path_segments:\n", join("\n\t",@path_segments), "\n" if ($self->{'debug'} >= 9);
    my @directories = $self->create_directory_list($block_number,
						   sprintf("/%s",shift(@path_segments)),
						   @path_segments);

    my @block_files = (); 
    # Loop through the matching directories for files to process.
    foreach my $found_directory (sort(@directories)) {
        print("create_files: found_directory = ${found_directory}\n") if ($self->{'debug'});
	    $self->process_directory($block_number,$directory,$found_directory,
                                 $self->{"cfg"}->{$block_number}->{"directory"},\@block_files,$database);
    }

    # No files were found for this block, so quit.
    if (@block_files == 0 && $self->{"cfg"}->{$block_number}->{"mode"} ne "add") {
        printf("\n\nNo files were found for:\n\tdirectory: %s\n\tfilename: %s\n",
               $self->{"cfg"}->{$block_number}->{"directory"},
               $self->{"cfg"}->{$block_number}->{"filename"});
        printf("The generated patterns were:\n\tdirectory: %s\n\tfilename: %s\n\n",
               $self->{"cfg"}->{$block_number}->{"dir_pattern"},
               $self->{"cfg"}->{$block_number}->{"pattern"});
        printf("Aborting further processing!\n\n");
        exit(1);
    }
    # Files were found, so add them to the file list.
    else {
        print "create_files: found:\n", join("\n\t",@{$files}), "\n" if ($self->{'debug'} >= 9);
        push(@{$files},@block_files);
    }
}

##--------------------------------------------------------------------------
# @signature String create_pattern(String base_pattern)
# <p>Convert the base pattern containing date/time characters to a Perl
# pattern usable for pattern matching.</p>
#
# @input $base_pattern The config file pattern containing the date/time characters.
# @output $pattern The base pattern converted into a Perl pattern matching pattern.
##--------------------------------------------------------------------------
sub create_pattern {
    my ($self,$base_pattern) = @_;

    printf("create_pattern: Base Pattern = %s\n",$base_pattern) if ($self->{'debug'});

    my $pattern = ""; # Holds the pattern being built from the base pattern
    my $pattern_index = 0; # Holder for the current character index for the pattern

    my $in_group = 0; # Holds flag while parsing though a group
    my $date_found = 0; # Holds flag while parsing through a date group.
    my $start_date_bracket_char = ""; # Holds the starting bracket character for the group
    my $close_date_bracket_char = ""; # Holds the closing bracket character for the group
    my $group_index = 0; # Holds the index where the group started so the brackets can be inserted into the correct part of the pattern.

    # Loop through the length of the base pattern
    while ($pattern_index < length($base_pattern)) {
	my $char = substr($base_pattern,$pattern_index,1);

	# Handle the search for a closing date bracket since we are in the
	# middle of a potential date grouping.
	if ($in_group) {
	    # Look for the expected closing date bracket character
	    if ($char eq $close_date_bracket_char) {
		# If we have found a date/time group, replace the grouping characters
		# with parens so they will be captured later
		if ($date_found) {
		    $pattern = sprintf("%s(%s)",
				       substr($pattern,0,$group_index),
				       substr($pattern,$group_index));
		}

		# We are not in a date group, so surround the group with the grouping
		# characters defined with the close date bracket character.
		else {
		    $pattern = sprintf("%s%s%s%s",
				       substr($pattern,0,$group_index),
				       $start_date_bracket_char,
				       substr($pattern,$group_index),
				       $close_date_bracket_char);
		}
		
		# We are finished with the grouping, so reset the variables to
		# allow the next group to be found.
		$in_group = 0;
		$date_found = 0;
		$start_date_bracket_char = "";
		$close_date_bracket_char = "";
		$group_index = 0;
	    }

	    # Look for special date/time meta-characters
	    elsif ($char =~ /^\\$/) {
		# Determine the meta character that follows the back slash
		my $meta_char = substr($base_pattern,++$pattern_index,1);
		
		# Look for the known date/time meta characters
		if ($meta_char =~ /^[YMyJHmVES]$/) {
		    $date_found = 1;

		    #If an event character is found replace it with a non-whitespace regex
		    if($meta_char eq "E"){
			    $pattern.="\\S";
		    }
		    else{
			    $pattern .= $meta_char eq "V" ? $MONTH_ABRV_PATTERN : "\\d";
		    }
		}

		# Not a date/time meta character, so just add it to the pattern
		else {
		    $pattern .= sprintf("%s%s",$char,$meta_char);
		}
	    }

	    # Not a character that matters to date/time parsing, so add it to the pattern
	    else {
			$pattern .= $char;
	    }
	}

	# Handle the remaining cases when we are searching for potential 
	# date groupings.
	else {
	    # Look for a potential start date bracket
	    if ($char eq "(" || $char eq "[" || $char eq "{") {
		# Prevent the nesting of groups.
		if ($close_date_bracket_char ne "") {
		    printf("Bad pattern: %s.  You cannot nest groups.\n",$base_pattern);
		    exit(1);
		}

		# Set up the loop to start looking for the end of the grouping
		else {
		    $in_group = 1;
		    $group_index = length($pattern);
		    $start_date_bracket_char = $char;

		    $close_date_bracket_char = $char eq "(" ? ")" :
			($char eq "[" ? "]" : "}");
		}
	    }

	    # Not a date bracket, so just add it to the pattern
	    else {
			$pattern .= $char;
	    }
	}

	$pattern_index++;
    }

    printf("Generated Pattern: %s\n",$pattern) if ($self->{'debug'});

    return $pattern;
}

##--------------------------------------------------------------------------
# @signature int days_in_month(int year, int month)
# <p>Determine the number of days in the specified month.</p>
#
# @input $year The year the month is in to determine a leap year.
# @input $month The month to find the days for.
# @output $days The number of days in the specified month.
##--------------------------------------------------------------------------
sub days_in_month {
    my ($year,$month) = @_;
    return (31,daysInFeb($year),31,30,31,30,31,31,30,31,30,31)[$month - 1]
}

##--------------------------------------------------------------------------
# @signature (Hash*, Hash*) determine_dates(int block_number, String directory, String file, String directory_pattern, String file_pattern, String directory_date_pattern, String file_date_pattern)
# <p>Determine the start and dates for a file.  Also captures any event characters</p>
#
# @input $block_number The index of the config file block the file was found in.
# @input $directory The directory where the file is located.
# @input $file The name of the file.
# @input $directory_pattern The Perl pattern that the directory matches.
# @input $file_pattern The Perl pattern that the file matches.
# @input $directory_date_pattern The config file pattern with the date
# characters that the directory matches.
# @input $file_date_pattern The config file pattern with the date
# characters that the file matches.
# @output $begin_date,$end_date,$event
##--------------------------------------------------------------------------
sub determine_dates {
    my ($self,$block_number,$directory,$file,$directory_pattern,$file_pattern,
        $directory_date_pattern,$file_date_pattern) = @_;
    my $full_path = sprintf("%s/%s",$directory,$file);
    my $full_pattern = sprintf("%s/%s",$directory_pattern,$file_pattern);
    my $date_pattern = sprintf("%s/%s",$directory_date_pattern,$file_date_pattern);

    # Define the hashes to hold the date information.
    my $begin = {};
    my $end = {};
    # Used to hold event string
    my $event;

    printf("\nSearching for dates for: %s\n",$full_path) if ($self->{'debug'});
    printf("Full Pattern: %s\n",$full_pattern) if ($self->{'debug'});
    printf("Date Pattern: %s\n\n",$date_pattern) if ($self->{'debug'});

    # This is required to be provide valid captures since we already know that the file
    # matches the patterns or it wouldn't have made it this far.
    $full_path =~ /^$full_pattern$/;

    # Create the list of values that were captured from the groups defined in the pattern
    # match.  This includes every group capture not just the date/time groups that we
    # want to extract.
    my @captures = ();
    for (my $i = 1; $i < @-; $i++) {
        # Make sure to include pattern matches that match an empty match
        if (defined($-[$i]) && defined($+[$i])) {
            push(@captures,substr($full_path,$-[$i],$+[$i] - $-[$i]));
        } else {
            push(@captures,"");
        }
    }

    printf("Captured Data:\n\t%s\n",join("\n\t",@captures)) if ($self->{'debug'});

    
    # Determine the date pattern groups that line up with the data that were captured
    # during the pattern match.
    #
    # The setting of the date/time pattern groups must include the grouping elements
    # (parens, brackets, or braces) to allow the script to figure out if the date/times
    # captured are begin dates, end dates, or both.
    #
    # The pattern the uses the month abbreviation pattern must be handled differently since
    # it has a single escaped date character for multiple matched characters and a very
    # long pattern match to allow for all of the months to be matched with various 
    # capitalizations.  To be able to parse this correctly, the script keeps track of the
    # offset for the month abbrevation pattern.  This is used to adjust the start and end
    # indicies following the pattern.
    # Note:  The offset does not affect the start index for the group that contains the
    # month abbreviation pattern since the start index is still before the pattern.
    my @date_patterns = ();
    my $paren_index = -1; # Hold the current place in the pattern being searched
    my $offset = 0; # Hold the number of characters to offset when the month abrv pattern is found
    my $found_month_abrv = 0; # Hold the flag that the month abrv pattern was found
    while (($paren_index = index($full_pattern,"(",$paren_index)) > -1) {
        my $start_index = $paren_index;
        my $end_index = index($full_pattern,")",$start_index);

        # Check to see if we have found the month abrv pattern
        if (substr($full_pattern,$start_index,$end_index - $start_index + 1) eq sprintf("(%s)",$MONTH_ABRV_PATTERN)) {
            $offset = length($MONTH_ABRV_PATTERN) - 2;
        }

		# Look to see if we have an offset that needs to adjust the indicies.
        if ($offset) {
            $start_index = $start_index - ($found_month_abrv ? $offset : 0);
            push(@date_patterns,substr($date_pattern,$start_index,$end_index - $start_index - $offset + 1));
        } else {
            push(@date_patterns,substr($date_pattern,$start_index,$end_index - $start_index + 1));
        }

        # Move the paren index to be after the current group.
        $paren_index = $end_index;

        # Let the following groups know that the month abrv pattern has been found.
        $found_month_abrv = $offset != 0;
    }

    printf("Date Groups:\n\t%s\n",join("\n\t",@date_patterns)) if ($self->{'debug'});

    # Look through the date pattern group list and parse out the date/times.
    for (my $i = 0; $i < @date_patterns; $i++) {

        printf("Processing Date Pattern: %s\n",$date_patterns[$i]) if ($self->{'debug'});
  
        if ($date_patterns[$i] =~ s/\\([YMyJHmVES])/$1/g) {
            printf("Matched Group: %s -> %s\n",$date_patterns[$i],$captures[$i]) if ($self->{'debug'});

            # Parse off the date/time grouping character
            my $date_type_char = substr($date_patterns[$i],0,1);

            # Determine which date(s) are in this group.
            my $is_begin_date = $date_type_char =~ /[\(\[]/;
            my $is_end_date = $date_type_char =~ /[\(\{]/;

            my $char_index = 1;
            while ($char_index < length($date_patterns[$i]) - 1) {
                my $char = substr($date_patterns[$i],$char_index,1);
                my $captured_char = substr($captures[$i],$char_index - 1,1);
		
		#Handle Event capture groups
		if ($date_patterns[$i] =~ /E[\*|\+|\{]/) {
			#Append the entire capture to the event string
			$event .= $captures[$i];
			#skip this capture since we are done with it
			last;
		}
                # Handle an Second time character.
                elsif ($char eq "S") {
                    $begin->{"second"} .= $captured_char if ($is_begin_date);
                    $end->{"second"} .= $captured_char if ($is_end_date);
                }
                # Handle an hour time character.
                elsif ($char eq "H") {
                    $begin->{"hour"} .= $captured_char if ($is_begin_date);
                    $end->{"hour"} .= $captured_char if ($is_end_date);
                }
                # Handle a julian date character.
                elsif ($char eq "J") {
                    $begin->{"julian"} .= $captured_char if ($is_begin_date);
                    $end->{"julian"} .= $captured_char if ($is_end_date);
                }
                # Handle a month date character.
                elsif ($char eq "M") {
                    $begin->{"month"} .= $captured_char if ($is_begin_date);
                    $end->{"month"} .= $captured_char if ($is_end_date);
                }
                # Handle a minute time character.
                elsif ($char eq "m") {
                    $begin->{"minute"} .= $captured_char if ($is_begin_date);
                    $end->{"minute"} .= $captured_char if ($is_end_date);
                }
		# Handle an event character
		elsif ($char eq "E") {
			$event .= $captured_char;
			
		}
                # Handle a month abbrevation character.
                elsif ($char eq "V") {
                    my $month;
                    if    ($captures[$i] =~ /jan/i  ) { $month = "01"; }
                    elsif ($captures[$i] =~ /feb/i  ) { $month = "02"; }
                    elsif ($captures[$i] =~ /mar/i  ) { $month = "03"; }
                    elsif ($captures[$i] =~ /apr/i  ) { $month = "04"; }
                    elsif ($captures[$i] =~ /may/i  ) { $month = "05"; }
                    elsif ($captures[$i] =~ /june?/i) { $month = "06"; }
                    elsif ($captures[$i] =~ /july?/i) { $month = "07"; }
                    elsif ($captures[$i] =~ /aug/i  ) { $month = "08"; }
                    elsif ($captures[$i] =~ /sept?/i) { $month = "09"; }
                    elsif ($captures[$i] =~ /oct/i  ) { $month = "10"; }
                    elsif ($captures[$i] =~ /nov/i  ) { $month = "11"; }
                    elsif ($captures[$i] =~ /dec/i  ) { $month = "12"; }

                    # Make sure we are not replacing an exisiting abbreviation in the start date.
		    if ($is_begin_date) {
			if (defined($begin->{"month_abrv"})) {
			    printf("There are multiple definitions of the MONTH ABRV for the START DATE.  Please update the config file to only have one \"\\V\" associated with a START DATE.\n");
			} else {
			    $begin->{"month_abrv"} = $month;
			}
		    }
                    # Do the same with the end date.
		    if ($is_end_date) {
			if (defined($end->{"month_abrv"})) {
			    printf("There are multiple definitions of the MONTH ABRV for the END DATE.  Please update the config file to only have one \"\\V\" associated with an END DATE.\n");
			    exit(1);
			} else {
			    $end->{"month_abrv"} = $month;
			}
		    }

                    # Since the month abbreviation is longer than a single character,
                    # the char_index needs to be advanced to one less than the length
                    # of the abbreviation to handle the entire abbreviation at once.
		    #!! this increments the pattern as well as the capture potentially missing 2-3 pattern characters
                    $char_index += length($captures[$i]) - 1;
                }
                # Handle a year date character.
                elsif ($char eq "Y") {
                    $begin->{"year"} .= $captured_char if ($is_begin_date);
                    $end->{"year"} .= $captured_char if ($is_end_date);
                }
                # Handle a day date character.
                elsif ($char eq "y") {
                    $begin->{"day"} .= $captured_char if ($is_begin_date);
                    $end->{"day"} .= $captured_char if ($is_end_date);
                }
                # Only handle date/time characters within a group.
                else {
                    printf("Unknown character %s in date/time grouping %d.  All characters within a date/time group must be a combination of [YMyJHmVES] date/time characters.  If there are characters between parts of the date/time, use multiple groups.\n",$char,$i+1);
                    exit(1);
                }

                $char_index++;
            }
        }
    }

    # All the parsing is now finished at this point.  Now we need to make sure
    # we have all of the date values defined and try to define them if they are
    # not.

    # Merge the "startdate" config file key with the values from the date patterns.
    $self->merge_dates("start",$begin,$self->{"cfg"}->{$block_number}->{"startdate"});

    # Run a sanity check to make sure that a real value exists for all of the start date values.
    if (!defined($begin->{"year"})) {
        printf("A YEAR has not been found for the START DATE.  Please update the config file to include a YEAR for the START DATE.\n");
        exit(1);
    }
    if (length($begin->{"year"}) < 4) {
        printf("A YEAR has been found with less than 4 digits in the START DATE.  Please update the config file to handle the YEAR in the START DATE.\n");
	exit(1);
    }
    if (!defined($begin->{"month"})) {
        printf("A MONTH has not been found for the START DATE.  Please update the config file to include a MONTH for the START DATE.\n");
        exit(1);
    }
    if (!defined($begin->{"day"})) {
        printf("A DAY has not been found for the START DATE.  Please update the config file to include a DAY for the START DATE.\n");
        exit(1);
    }
    if (!defined($begin->{"hour"})) {
        printf("An HOUR has not been found for the START DATE.  Please update the config file to include an HOUR for the START DATE.\n");
        exit(1);
    }
    if (!defined($begin->{"minute"})) {
        printf("A MINUTE has not been found for the START DATE.  Please update the config file to include a MINUTE for the START DATE.\n");
        exit(1);
    }
    if (!defined($begin->{"second"})) {
        # Allow the user to ignore seconds by using a default of 0 if it is not defined instead of
        # throwing an error.
        $begin->{"second"} = 0;
    }

    # Properly format the begin date and time.
    my $begin_date = sprintf("%04d-%02d-%02d",$begin->{"year"},$begin->{"month"},$begin->{"day"});
    my $begin_time = sprintf("%02d:%02d:%02d",$begin->{"hour"},$begin->{"minute"},$begin->{"second"});

    # Adjust any end dates that have rolled over to the next day.
    # To be valid dates in the next day.
    ($begin_date,$begin_time) = adjustDateTime($begin_date,"YYYY-MM-DD",$begin_time,"HH:MM:SS",0,0,0,0);

#    printf("%04d-%02d-%02d %02d:%02d:%02d\n",$begin->{"year"},$begin->{"month"},$begin->{"day"},$begin->{"hour"},$begin->{"minute"},$begin->{"second"});


    # With the begin date fully defined, start determining the end date.  The begin date must be fully
    # defined first because the end date may need to be calculated from it.

    # First check to see if there is going to be a conflict in trying to determine the end date.
    if (defined($self->{"cfg"}->{$block_number}->{"enddate"}) &&
        defined($self->{"cfg"}->{$block_number}->{"filelength"})) {
        printf("There is a conflict with the END DATE.  Both the ENDDATE and FILELENGTH keys are defined.  Please remove one of the keys from the config file.\n");
        exit(1);
    }

    # Merge the "enddate" config file key with the values from the date patterns.
    elsif (defined($self->{"cfg"}->{$block_number}->{"enddate"})) {
        $self->merge_dates("end",$end,$self->{"cfg"}->{$block_number}->{"enddate"});
    }

    # Calculate the end date from the begin date and the "filelength" config file key.
    elsif (defined($self->{"cfg"}->{$block_number}->{"filelength"})) {

        # Make sure that there isn't part or all of the end date already defined.
        foreach my $key (keys(%{ $end})) {
            printf("There is a conflict with the END DATE.  The %s is already defined as the script is trying to use the FILELENGTH key.  Please change the config file to only use the FILELENGTH or the DATE PATTERNS.\n",uc($key));
            exit(1);
        }

        $end = $self->determine_end_date($begin,$self->{"cfg"}->{$block_number}->{"filelength"});
    }

    # Neither the "enddate" or the "filelength" key are defined, so it better all have been set
    # using the date patterns.
    else {
        # Use the merge function with undef() to merge any month abbreviations and julian dates.
        $self->merge_dates("end",$end,undef());
    }


    # Run a sanity check to make sure that a real value exists for all of the end date values.
    if (!defined($end->{"year"})) {
        printf("A YEAR has not been found for the END DATE.  Please update the config file to include a YEAR for the END DATE.\n");
        exit(1);
    }
    if (length($end->{"year"}) < 4) {
        printf("A YEAR has been found with less than 4 digits in the END DATE.  Please update the config file to handle the YEAR in the END DATE.\n");
	exit(1);
    }
    if (!defined($end->{"month"})) {
        printf("A MONTH has not been found for the END DATE.  Please update the config file to include a MONTH for the END DATE.\n");
        exit(1);
    }
    if (!defined($end->{"day"})) {
        printf("A DAY has not been found for the END DATE.  Please update the config file to include a DAY for the END DATE.\n");
        exit(1);
    }
    if (!defined($end->{"hour"})) {
        printf("An HOUR has not been found for the END DATE.  Please update the config file to include an HOUR for the END DATE.\n");
    exit(1);
    }
    if (!defined($end->{"minute"})) {
        printf("A MINUTE has not been found for the END DATE.  Please update the config file to include a MINUTE for the END DATE.\n");
        exit(1);
    }
    if (!defined($end->{"second"})) {
        # Allow the user to ignore seconds by using a default of 59 if it is not defined instead of
        # throwing an error.
        $end->{"second"} = 59;
    }

    # Since all the dates are fully defined, they can be formatted.
    #my $begin_date = sprintf("%04d-%02d-%02d",$begin->{"year"},$begin->{"month"},$begin->{"day"});
    #my $begin_time = sprintf("%02d:%02d:%02d",$begin->{"hour"},$begin->{"minute"},$begin->{"second"});
    my $end_date = sprintf("%04d-%02d-%02d",$end->{"year"},$end->{"month"},$end->{"day"});
    my $end_time = sprintf("%02d:%02d:%02d",$end->{"hour"},$end->{"minute"},$end->{"second"});
 
    # Adjust any end dates that have rolled over to the next day.
    # To be valid dates in the next day.
    ($end_date,$end_time) = adjustDateTime($end_date,"YYYY-MM-DD",$end_time,"HH:MM:SS",0,0,0,0);

    return (sprintf("%s %s",$begin_date,$begin_time),
	    sprintf( "%s %s", $end_date, $end_time ), $event );
}

##--------------------------------------------------------------------------
# @signature Hash* determine_end_date(Hash* begin, String filelength)
# <p>Determine the end date from the filelength key off of the begin date.</p>
#
# @input $begin The begin date to use as the base for calculating the end date.
# @input $filelength The config file key to use to determine the end date.
# @warning This function will die if the filelength key is malformed.
##--------------------------------------------------------------------------
sub determine_end_date {
    my ($self,$begin,$filelength) = @_;
    my $end;

    if ($filelength =~ /^(\d+)\s+(years?|months?|days?|hours?|minutes?|seconds?)$/i) {
        my ($value,$frequency) = ($1,$2);

        # Format the begin date/time into a form that can be used for the calculations
        my $begin_date = sprintf("%04d-%02d-%02d",$begin->{"year"},$begin->{"month"},
				 $begin->{"day"});
        my $begin_time = sprintf("%02d:%02d:%02d",$begin->{"hour"},$begin->{"minute"},
				 $begin->{"second"});
        my ($end_date,$end_time);
 
        # Handle the frequency of years.
        if ($frequency =~ /years?/i) {
            # Since every year is 12 months, but there could be 365/366 days depending on the year,
            # Change to months and then convert.
            $value *= 12;
            $frequency = "months";
        }
        # Handle the frequency of months.
        if ($frequency =~ /months?/i) {
            ($end_date,$end_time) = ($begin_date,$begin_time);

            # Loop through the number of months to advance.
            for (my $i = 0; $i < $value; $i++) {
                ($end_date,$end_time) = adjustDateTime($end_date,"YYYY-MM-DD",$end_time,"HH:MM:SS",
                                                       days_in_month(substr($end_date,0,4),
                                                                     substr($end_date,5,2)),0,0,-1);

            }
        }
        # Handle the frequency of days
        elsif ($frequency =~ /days?/i) {
            ($end_date,$end_time) = adjustDateTime($begin_date,"YYYY-MM-DD",$begin_time,"HH:MM:SS",$value,0,0,-1);
        }
        # Handle the frequency of hours.
        elsif ($frequency =~ /hours?/i) {
            ($end_date,$end_time) = adjustDateTime($begin_date,"YYYY-MM-DD",$begin_time,"HH:MM:SS",0,$value,0,-1);
        }
        # Handle the frequency of minutes.
        elsif ($frequency =~ /minutes?/i) {
            ($end_date,$end_time) = adjustDateTime($begin_date,"YYYY-MM-DD",$begin_time,"HH:MM:SS",0,0,$value,-1);
        }
        elsif ($frequency =~ /seconds?/i) {
            ($end_date,$end_time) = adjustDateTime($begin_date,"YYYY-MM-DD",$begin_time,"HH:MM:SS",0,0,0,$value-1);
        }
        # Some major problem occurred if it gets this far, so warn the user to talk to someone
        # who can look into it.
        else {
            printf("A rogue frequency made it through the pattern matching and should not have occurred.  Please contact someone on what has happened.\n");
            exit(1);
        }

        # Parse out the different parts of the end date/time into the correct keys
        $end->{"year"} = substr($end_date,0,4);
        $end->{"month"} = substr($end_date,5,2);
        $end->{"day"} = substr($end_date,8,2);
        $end->{"hour"} = substr($end_time,0,2);
        $end->{"minute"} = substr($end_time,3,2);
        $end->{"second"} = substr($end_time,6,2);
    }

    # The filelength key value is not valid.
    else {
        printf("The FILELENGTH key in the config file does not have a vaild value.  It must be a number followed by one of the following: years, months, days, hours, or minutes.\n");
        exit(1);
    }

    return $end;
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

    #create database object
    my $database = MySqlDatabase->new("zithupdate","change-999");
    my $err = $database->connect();
    
    # Quit if the connection could not be created.
    die($err) if ($err ne "");

    # Create the files to be inserted into the database
    my @files;
    foreach my $index (keys(%{ $self->{"cfg"}})) {
        print "insert: create_files(${index}, ", $self->{"cfg"}->{$index}->{"dir_pattern"}, ",...)\n" if ($self->{'debug'});
        $self->create_files($index,$self->{"cfg"}->{$index}->{"dir_pattern"},\@files,$database);
	if (defined($self->{"cfg"}->{$index}->{"mysqlserver"})) {
		$database->setHost($self->{"cfg"}->{$index}->{"mysqlserver"});
	}
    }

    # Insert the files if they were created.
    if (scalar(@files) > 0) {
	print "Inserting files into database at " . $database->getHost() . "\n\n";

        # Holder for storing multiple errors
        my $err_report = "";

        # Insert all of the files.
	# Print all of the files as long as (size of the insert)<200
	# if the size is bigger than 200 just print 200
	my $numberOfPrints = 0;
        foreach my $file (@files) {

            # Handle cross-mounted file system.
            # and Reset the directory to make sure it uses /net
            my $directory = $file->getDirectory();
	        $directory =~ s/^\/export/\/net/;
	        $directory =~ s%^/archive/%/net/archive/%;
            $file->setDirectory($directory);

            $err = $file->insert($database);

	    # Check that printf hasn't executed more than 200 times
	    if($numberOfPrints < 200){
            	# Display the file information to the user.

                my $dsidstr = $file->getDatasetArchiveIdent();
                $dsidstr = '' if (!defined($dsidstr));
                $dsidstr .= ' (' . $file->getDatasetId() . ')' if (defined $file->getDatasetId());
                
                my $evstr = $file->getEvent();
                $evstr = '' if (!defined($evstr));

            	printf("%s %s %s %7s %02d %05d %10s %s/%s %s\n",
                  $dsidstr,$file->getBeginDate(),$file->getEndDate(),
                  $file->getPurpose(),$file->getFormatId(),
                  $file->getSize(),$file->getHost(),
                  $file->getDirectory(),$file->getFilename(),
                  $evstr);

	    	$numberOfPrints = $numberOfPrints + 1;
	    }

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

        # Provide the user the ability to cancel the commit.
	    printf("\n\n%d files are ready to be inserted.\n\n",scalar(@files));
        printf("To insert the files, press Enter.\n");
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
    } else {
	printf("\n\nNo new files were found!\n\nNo files were inserted!!\n\n\n");
    }
    # Close the connection.
    printf("%s",$database->disconnect());
}

##--------------------------------------------------------------------------
# @signature int is_mass_store_block(int block_number)
# <p>Determine if the host of the specified block number is on the mass store.</p>
#
# @input $block_number The number of the config file block being checked.
# @return <code>true</code> if the block is found on the mass store,
# <code>false</code> if it is on the localhost.
##--------------------------------------------------------------------------
sub is_mass_store_block {
    my ($self,$block_number) = @_;
    return ($self->{"cfg"}->{$block_number}->{"host"} eq "hpss");
}

##--------------------------------------------------------------------------
# @signature void merge_dates(String type, Hash* date, String datekey)
# <p>Merge the values in the date hash with the values defined in the datekey
# variable from the configuration file.  This will convert any month 
# abbreviations into the month part of the date and julian dates into the
# month and day parts of the date before merging with the datekey.</p>
#
# @input $type The "type" of the date being checked (i.e start or end)
# @input $date The reference to the date being merged.
# @input $datekey The value for the type of date from the config file.
# @warning This function performs a number of checks on the dates and will
# die if any of the checks fail.
##--------------------------------------------------------------------------
sub merge_dates {
    my ($self,$type,$date,$datekey) = @_;

    # Make sure that there is only one type of month defined and make sure it is in the "month" key.
    if (defined($date->{"month"}) && defined($date->{"month_abrv"})) {
        printf("Multiple months have been defined for the %s date.  Please update the config file to include only the MONTH or only the MONTH ABBR in the DATE PATTERNS.\n",$type);
        exit(1);
    } elsif (defined($date->{"month_abrv"})) {
        $date->{"month"} = $date->{"month_abrv"};
        # Remove the abrv value to prevent it from accidentally being used later.
        delete($date->{"month_abrv"});
    }

    # Convert any Julian dates into the standard format.
    if (defined($date->{"julian"})) {
        # Make sure there is a defined year to convert out of julian day.
        if (!defined($date->{"year"})) {
            printf("The %s date does not have a YEAR defined and cannot convert out of JULIAN day.  Please update the config file to include a %s year.\n",$type,$type);
            exit(1);
        }
        # Make sure there is not a conflict with a month or day already existing.
        if (defined($date->{"month"}) || defined($date->{"day"})) {
            printf("The %s date has a JULIAN day defined with either a MONTH or DAY defined as well.  Please update the config file to only include either JULIAN or MONTH and DAY for the %s date.\n",$type,$type);
            exit(1);
        } else {
            ($date->{"month"},$date->{"day"}) = convertJulian($date->{"year"},$date->{"julian"});
        }
        
        # Remove the julian date to prevent any possible confusion later.
        delete($date->{"julian"});
    }          

    if (defined($datekey)) {
        my @date_parts = split(/[\-\s:]/,$datekey);

        # Merge the years.
        if ($date_parts[0] =~ /^\d{4}$/) {
            # There is a conflict with 2 defined years.
            if (defined($date->{"year"})) {
                printf("The %s date has multiple YEAR definitions.  Please update the config file to only include the %s year in either the %sDATE key or in the DATE PATTERNS.\n",$type,$type,uc($type));
                exit(1);
            } else {
                $date->{"year"} = $date_parts[0];
            }
        } elsif ($date_parts[0] =~ /^Y{4}$/) {
            # This just needs to ensure that the year is defined fully.
            if (!defined($date->{"year"}) || $date->{"year"} !~ /^\d{4}$/) {
                printf("The %s YEAR has not been properly defined.  Please update the config file to include a 4-digit %s YEAR in either the %sDATE key or in the DATE PATTERNS.\n",uc($type),uc($type),uc($type));
            }
        } elsif ($date_parts[0] =~ /^\d+(Y+)$/ && length($date_parts[0]) == 4) {
            # Merge the two parts of the year and make sure that it is formatted properly.
            if (length($date->{"year"}) != length($1)) {
                printf("Unable to merge the pattern %s in the %sDATE key with the value %s from the DATE PATTERNS since they are not the same length.  Please update the config file so these values will match.\n",$1,uc($type),$date->{"year"});
                exit(1);
            } else {
                $date_parts[0] =~ s/Y+/$date->{"year"}/;
                $date->{"year"} = $date_parts[0];
            }
        } else {
            printf("The year pattern in the %sDATE cannot be parsed.  It must be a 4-digit year, the YYYY pattern, or a set of digits followed by a sequence of Y characters that is only 4 characters long.\n",uc($type));
            exit(1);
        }

        # Merge the months.
        if ($date_parts[1] =~ /^\d{1,2}$/) {
            # There is a conflict with 2 defined months.
            if (defined($date->{"month"})) {
                printf("The %s date has multiple MONTH definitions.  Please update the config file to only include the %s month in either the %sDATE key or in the DATE PATTERNS.\n",$type,$type,uc($type));
                exit(1);
            } elsif ($date_parts[1] < 1 || $date_parts[1] > 12) {
                printf("The %s DATE in the %sDATE key does not contain a valid month value.\n",uc($type),uc($type));
            } else {
                $date->{"month"} = $date_parts[1];
            }
        } elsif ($date_parts[1] eq "MM") {
            # This just needs to ensure that the month is defined fully.
            if (!defined($date->{"month"}) || $date->{"month"} !~ /^\d{1,2}$/) {
                printf("The %s MONTH has not been properlty defined.  Please update the config file to include a %s MONTH in either the %sDATE key or in the DATE PATTERNS.\n",uc($type),uc($type),uc($type));
                exit(1);
            }
            # Make sure the month is valid (1-12)
            elsif ($date->{"month"} < 1 || $date->{"month"} > 12) {
                printf("The %s MONTH has an invalid value %s.  It must be between 1 and 12.\n",uc($type),$date->{"month"});
                exit(1);
            }
        } else {
            printf("The month pattern in the %sDATE cannot be parsed.  It must be a month value between 1 and 12 or a MM pattern.\n",uc($type));
            exit(1);
        }

        # Merge the days.
        if ($date_parts[2] =~ /^\d{1,2}$/) {
           # There is a conflict with 2 defined days.
           if (defined($date->{"day"})) {
               printf("The %s date has multipel DAY definitions.  Please update the config file to only include the %s day in either the %sDATE key or in the DATE PATTERNS.\n",$type,$type,uc($type));
               exit(1);
           } elsif ($date_parts[2] < 1 || $date_parts[2] > days_in_month($date->{"year"},$date->{"month"})) {
               printf("The %s DATE in the %sDATE key does not contain a valid DAY value for %04d-%02d.\n",uc($type),uc($type),$date->{"year"},$date->{"month"});
               exit(1);
           } else {
               $date->{"day"} = $date_parts[2];
           }
        } elsif ($date_parts[2] eq "yy") {
           # This needs to ensure that the day is correctly defined.
           if (!defined($date->{"day"}) || $date->{"day"} !~ /^\d{1,2}$/) {
               printf("The %s DAY has not been properly defined.  Please update the config file to include a %s DAY in either the %sDATE key or in the DATE PATTERNS.\n",uc($type),uc($type),uc($type));
               exit(1);
           }
           # Make sure the day is valid.
           elsif ($date->{"day"} < 1 || $date->{"day"} > days_in_month($date->{"year"},$date->{"month"})) {
               printf("The %s DAY has an invalid value %s.  It must be between 1 and %d for %04d-%02d or a yy pattern.\n",uc($type),$date->{"day"},days_in_month($date->{"year"},$date->{"month"}),$date->{"year"},$date->{"month"});
               #exit(1);
           }
        } else {
           printf("The day pattern in the %sDATE key cannot be parsed.  It must be a day value between 1 and max value for month/year or a yy pattern.\n",uc($type));
           exit(1);
        }


        # Merge the hours.
        if ($date_parts[3] =~ /^\d{1,2}$/) {
            # There is a conflict with 2 defined hours.
            if (defined($date->{"hour"})) {
                printf("The %s date has multiple HOUR definitions.  Please update the config file to only include the %s day in either the %sDATE key or in the DATE PATTERNS.\n",$type,$type,uc($type));
                exit(1);
            } elsif ($date_parts[3] < 0 || $date_parts[3] > 24) {
                printf("The HOUR in the %sDATE key does not contain a valid HOUR value.  It must be between 0 and 23.\n",uc($type));
                exit(1);
            } else {
                $date->{"hour"} = $date_parts[3];
            }
        } elsif ($date_parts[3] eq "HH") {
            # This needs to ensure that the day is correctly defined.
            if (!defined($date->{"hour"}) || $date->{"hour"} !~ /^\d{1,2}$/) {
                printf("The %s HOUR has not been properly defined.  Please update the config file to include a %s HOUR in either the %sDATE key or in the DATE PATTERNS.\n",uc($type),uc($type),uc($type));
                exit(1);
            }
            # Make sure the hour is valid.  Unlike the startdate key, we will allow the read in hour
            # to be over 23 to allow a rollover to the next day.
            elsif ($date->{"hour"} < 0) {
                printf("The %s HOUR has an invalid value of %s.  It must be at least 0 or a HH pattern.\n",uc($type),$date->{"hour"});
                exit(1);
            }
       } else {
            printf("The HOUR pattern in the %sDATE key cannot be parsed.  It must be an HOUR value between 0 and 23 or a HH pattern.\n",uc($type));
            exit(1);    
       }

        # Merge the minutes.
        if ($date_parts[4] =~ /^\d{1,2}$/) {
            # There is a conflict with 2 defined minutes.
            if (defined($date->{"minute"})) {
                printf("The %s date has multiple MINUTE definitions.  Please update the config file to only inclue the %s minute in either %sDATE key or in the DATE PATTERNS.\n",$type,$type,uc($type));
                exit(1);
            } elsif ($date_parts[4] < 0 || $date_parts[4] > 59) {
                printf("The MINUTE in the %sDATE key does not contain a valid MINUTE value.  It must be between 0 and 59.\n",uc($type));
                exit(1);
            } else {
                $date->{"minute"} = $date_parts[4];
            }
        } elsif ($date_parts[4] eq "mm") {
            # This needs to ensure that the minutes is correctly defined.
            if (!defined($date->{"minute"}) || $date->{"minute"} !~ /^\d{1,2}$/) {
                printf("The %s MINUTE has not been properly defined.  Please update the ocnfig file to include a %S MINUTE in either the %sDATE key or in the DATE PATTERNS.\n",uc($type),uc($type),uc($type));
                exit(1);
            }
            # Make sure the minute is valid.
            elsif ($date->{"minute"} < 0 || $date->{"minute"} > 59) {
                printf("The %s MINUTE has an invalid value %s.  It must be between 0 and 59 or a \"mm\" pattern.\n",uc($type),$date->{"minute"});
                exit(1);
            }
        } else {
            printf("The MINUTE pattern in the %sDATE key cannot be parsed.  It must be a MINUTE value between 0 and 59 or the \"mm\" pattern.\n",uc($type));
            exit(1);
        }

 
        # Merge the seconds.
        if ($date_parts[5] =~ /^\d{1,2}$/) {
            # There is a conflict with 2 defined seconds.
            if (defined($date->{"second"})) {
                printf("The %s date has multiple SECOND definitions.  Please update the config file to only include the %s second in either %sDATE key or in the DATE PATTERNS.\n",$type,$type,uc($type));
                exit(1);
            } elsif ($date_parts[5] < 0 || $date_parts[5] > 59) {
                printf("The SECOND in the %sDATE key does not contain a valid SECOND value.  It must be between 0 and 59.\n",uc($type));
                exit(1);
            } else {
                $date->{"second"} = $date_parts[5];
            }
        } elsif ($date_parts[5] eq "SS") {
            # This needs to ensure that the seconds are correctly defined.
            if (!defined($date->{"second"}) || $date->{"second"} !~ /^\d{1,2}$/) {
                printf("The %s SECOND has not been properly defined.  Please update the config file to include a %s SECOND in either the %sDATE key or in the DATE PATTERNS.\n",uc($type),uc($type),uc($type));
                exit(1);
            }
            # Make sure the second is valid.
            elsif ($date->{"second"} < 0 || $date->{"second"} > 59) {
                printf("The %s SECOND has an invalid value %s.  It must be between 0 and 59.\n",uc($type),$date->{"second"});
                exit(1);
            }
        } else {
            printf("The SECOND pattern in the %sDATE key cannot be parsed.  It must be a SECOND value between 0 and 59 or the \"SS\" pattern.\n",uc($type));
            exit(1);
        }
    }
}

##--------------------------------------------------------------------------
# @signature void process_directory(int block_number, String directory_pattern, String directory, String directory_date_pattern,$files)
# <p>Process a directory looking for files to be inserted.  This will recurse
# the subdirectories as well if recursion is allowed.</p>
#
# @input $block_number The index of the block number from the config file
# that is being processed.
# @input $directory_pattern The Perl pattern matching pattern for the
# directory being processed.
# @input $directory The actual directory being processed.
# @input $directory_date_pattern The config file pattern containing the
# date characters used for parsing the date from the file pattern.
# @input $files A pointer to the array containing the MySqlFile objects
# to be inserted into the database.
##--------------------------------------------------------------------------
sub process_directory {
    my ($self,$block_number,$directory_pattern,$directory,$directory_date_pattern,$files,$database) = @_;

    printf("Processing Directory: %s\n",$directory);

    my $file_pattern = $self->{"cfg"}->{$block_number}->{"pattern"};
    
    my @file_list = ();
    my @mss_type_list = ();
    my @size_list = ();
    
    # Read in the list of files and directories for this directory.
    if ($self->is_mass_store_block($block_number)) {
	my @results = grep(/^(\-|\d)/,HPSS::ls($directory, "-l"));

	foreach my $result (@results) {
	    push(@mss_type_list,(split(' ',$result))[0]);
	    push(@file_list,(split(' ',$result))[8]);
	    push(@size_list,(split(' ',$result))[4]);
	}
    } else {
	opendir(my $DIR,$directory) or die("Cannot open directory: $directory\n");
	@file_list = readdir($DIR);
	closedir($DIR);
	foreach (@file_list) { push @size_list, (-s "$directory/$_"); }
    }

    # Loop through the files in the directory.
    for (my $i = 0; $i <= $#file_list; $i++){
	my $file = $file_list[$i];
	my $size = $size_list[$i];

	# Recursively process the directory if recursion is allowed
	if (((!$self->is_mass_store_block($block_number) && (-d sprintf("%s/%s",$directory,$file))) || 
	     (($self->is_mass_store_block($block_number) && shift(@mss_type_list) =~ /^d/))) && 
	    $file !~ /^\.+$/ && $self->use_recursion($block_number)) {

	    $self->process_directory($block_number,
				     sprintf("%s/%s",$directory_pattern,$file),
				     sprintf("%s/%s",$directory,$file),
                                     sprintf("%s/%s",$directory_date_pattern,$file),
				     $files);
	}

	# Process files that match the file pattern
	elsif ($file =~ /^$file_pattern$/) {

	    printf("\n\nFound File: %s\n",$file) if ($self->{'debug'});

	    my ($begin_date,$end_date,$event) = $self->determine_dates($block_number,
								$directory,
								$file,
								$directory_pattern,
								$file_pattern,
                                $directory_date_pattern,
                                $self->{"cfg"}->{$block_number}->{"filename"});

            # Create the MySQL entry
            my $mysql =  MySqlFile->new();
            
	    # if mode = add and file exists
	    next if (
           $self->{"cfg"}->{$block_number}->{"mode"} eq "add"
           &&
           $mysql->load($self->{"cfg"}->{$block_number}->{"dataset_id"},
                           $directory, $file, $database)
           );

	    if (
            $self->{"cfg"}->{$block_number}->{"mode"} eq "update"
            &&
            $mysql->load($self->{"cfg"}->{$block_number}->{"dataset_id"},
                         $directory, $file, $database)
           ) { 
		    print STDERR "Updating " . $mysql->getFilename() . "\n";
		    die "NOT YET IMPLEMENTED!\n";
		    # TO BE CONTINUED!
	        }

	    $mysql->setDatasetArchiveIdent($self->{"cfg"}->{$block_number}->{"dataset_id"});

	    $mysql->setHost($self->{"cfg"}->{$block_number}->{"host"});
        $mysql->setBeginDate(split(/[\-\s:]/,$begin_date));
        $mysql->setEndDate(split(/[\-\s:]/,$end_date));

	    if (defined $size && $size > 0) {
		    $mysql->setFile($directory,$file, ($size/1024)+1);
	    } else {
		    $mysql->setFile($directory, $file);
	    }

        $mysql->setFormatId($self->{"cfg"}->{$block_number}->{"format"});
        $mysql->setPurpose($self->{"cfg"}->{$block_number}->{"purpose"});

		# set event to default/config value if it is not defined from the filename
		if (!defined($event) || ($event !~ m/\S/)) {
            $event = $self->{'cfg'}->{$block_number}->{'event'};
            }
        # now set the file.event if we have a decent event string
	    $mysql->setEvent($event) if ($event =~ m/\S/);

            # Determine if the file must fall within a time range.
            if (defined($self->{"cfg"}->{$block_number}->{"rolloverbefore"})) {
                my @time = split(/:/,$self->{"cfg"}->{$block_number}->{"rolloverbefore"});
                my @file = (split(/:/,(split(' ',$mysql->getBeginDate()))[1]),
                            split(/:/,(split(' ',  $mysql->getEndDate()))[1]));

		my $timeSec = $time[0]*3600+$time[1]*60+$time[2];

		my $fileBegin = $file[0]*3600+$file[1]*60+$file[2];
		my $fileEnd = $file[3]*3600+$file[4]*60+$file[5];

		if ($timeSec >= $fileBegin && $timeSec >= $fileEnd) {
		    #It is important to expand the end time first other wise the end time will get double incremented
		    #Technically could just increment the Begin time and let MySql file handle the end time but it more confusing that way
		    $mysql->setEndDate($mysql->increment_day($mysql->getEndDate()));
		    $mysql->setBeginDate($mysql->increment_day($mysql->getBeginDate()));
                }
            } 

            # Determine if the file must fall within a date range.
            if (defined($self->{"cfg"}->{$block_number}->{"insertrange"})) {
                my @range = split(/:/,$self->{"cfg"}->{$block_number}->{"insertrange"});
                my @date = ((split(' ',$mysql->getBeginDate()))[0],
                            (split(' ',  $mysql->getEndDate()))[0]);

                if (compareDates($range[0],"YYYY-MM-DD",$date[0],"YYYY-MM-DD") >= 0 &&
                    compareDates($range[1],"YYYY-MM-DD",$date[1],"YYYY-MM-DD") <= 0) {
                    push(@{$files},$mysql);
                }
            } 
            # No insert range, so it must be inserted.
            else {
                push(@{$files},$mysql);
            }
	}
    }
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

    # Read in the configuration information for all of the blocks
    open(my $CFG,$file) or die("Cannot open config file $file\n");
    my @data = grep(/^(\{|\})|^(\s*[a-z]+:)/,<$CFG>);
    close($CFG);

    # Load the config data into the instance.
    my $blockCount = 0;
    for(my $count = 0; $count < scalar(@data); $count++) {
	my $line = $data[$count];

    # set the global debug var
    if ($line =~ m/^\s+debug\s*:\s*(\d+)\s*$/) {
      $self->{'debug'} = $1;
      next;
      }

	# Loop through the bracketed sections
	if ($line =~ /^\{/) {
	    $line = $data[++$count];
	    while ($line !~ /^\}/) {
		$line =~ /^\s*([a-z]+):\s*(\S.*)\s*$/;
		$self->{"cfg"}->{$blockCount}->{trim($1)} = trim($2);
		$line = $data[++$count];
	    }



	    # Add the dataset id to the config set.
	    $file =~ /^cfg\-([^\-]+)\-?(.+)?$/;
	    $self->{"cfg"}->{$blockCount}->{"dataset_id"} = $1;
	    if (defined($2)) {
	        # If this dataset id is a BSIERP dataset id, then do not specify the server.
		if ( ($2 =~ m/^\d+$/) && ($self->{"cfg"}->{$blockCount}->{"dataset_id"} =~ m/^\d+\.B\d+/) ) {
		    my $tmp_dataset_id = $file;
		    $tmp_dataset_id =~ s/^cfg\-//;
		    $self->{"cfg"}->{$blockCount}->{"dataset_id"} = $tmp_dataset_id; 
		}
		else { $self->{"cfg"}->{$blockCount}->{"mysqlserver"} = $2 . ".eol.ucar.edu"; }
	    }

	    # Create the patterns that will be used for finding directories
	    # and files to be loaded.
	    $self->{"cfg"}->{$blockCount}->{"pattern"} =
		$self->create_pattern($self->{"cfg"}->{$blockCount}->{"filename"});
	    $self->{"cfg"}->{$blockCount}->{"dir_pattern"} =
		$self->create_pattern($self->{"cfg"}->{$blockCount}->{"directory"});
	   
	    # Set the default purpose to data if it hasn't been defined by the user
            if (!defined($self->{"cfg"}->{$blockCount}->{"purpose"})) {
                $self->{"cfg"}->{$blockCount}->{"purpose"} = "data";
            }

	    # Set the default directory recursion to 1
            if (!defined($self->{"cfg"}->{$blockCount}->{"recursedirectories"})) {
                $self->{"cfg"}->{$blockCount}->{"recursedirectories"} = 1;
            } else {
		# Convert any true or false value to the appropriate number
                $self->{"cfg"}->{$blockCount}->{"recursedirectories"} =~ s/true/1/i;
                $self->{"cfg"}->{$blockCount}->{"recursedirectories"} =~ s/false/0/i;
            }

	    # Set the default host to localhost
	    if (!defined($self->{"cfg"}->{$blockCount}->{"host"})) {
		    $self->{"cfg"}->{$blockCount}->{"host"} = "localhost";
	    }

	    if (!defined($self->{"cfg"}->{$blockCount}->{"mode"})) {
		    $self->{"cfg"}->{$blockCount}->{"mode"} = "";
	    }

	    if (!defined($self->{"cfg"}->{$blockCount}->{"event"})) {
		    $self->{"cfg"}->{$blockCount}->{"event"} = "";
	    }

	    if (!defined($self->{"cfg"}->{$blockCount}->{"format"})) {
		    printf("The format is not defined for block \#%d\n",$blockCount);
		    exit(1);
	    }

	    $blockCount++;
	}
    }
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

##--------------------------------------------------------------------------
# @signature int use_recursion(int block_number)
# <p>Determine if the specified block allows the directory to recurse its
# subdirectories.</p>
#
# @input $block_number The number of the block to be checked.
# @output $flag <code>true</code> if the directory can be recursed,
# <code>false</code> if it cannot.
##--------------------------------------------------------------------------
sub use_recursion {
    my ($self,$block_number) = @_;
    return $self->{"cfg"}->{$block_number}->{"recursedirectories"};
}

1;
