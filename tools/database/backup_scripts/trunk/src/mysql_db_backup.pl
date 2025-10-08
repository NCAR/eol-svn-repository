#! /usr/bin/perl -w

##Module--------------------------------------------------------------------------------------
# <p>The mysql_db_backup.pl script is a cron script to backup the DMG's databases into text
# files using <code>mysqldump</code>.  The script creates two new dump files for each database
# it monitors per day (run at 00:20 and 12:20).  It will also remove all old backup files on the system that are older
# than the specified <code>DAYS_TO_KEEP</code> variable currently set to 30 days.  If any
# errors occur during the backup, an email will be sent to the monitors of the script notifying
# them of the problems discovered.</p>
#
# @author Joel Clawson
# @version 1.0  The creation of the script.
#
# @author Janet Scannell
# @version 2.0 Change the backups to run twice/day, add host and hour created to filename, remove files
#              after 30 days, change monitors, fix to backup only two databases that currently exist, change
#              directory where the backups are stored, only email report when there are errors, gzip the dump files.
#
# @author Janet Scannell
# @version 2.1 Update host to emdac
# Add option for no table spaces to dump command.  12/20
#
# @author Janet Scannell
# @version 2.2 Move backups to run on typhoon and move where backups are stored to 
#              /net/work/db_backups                12/2022
##Module--------------------------------------------------------------------------------------
use strict;

use lib "/net/work/lib/perl/Utilities";
use DpgDate;

# Constants used for executing the mysqldump program.
my $MYSQLDUMP = "/usr/bin/mysqldump";
my $OPTIONS = "--no-defaults -c -K -Q --single-transaction --no-tablespaces";
my $HOST = "emdac";
my $USER = "dts-full";
my $PASSWORD = "l\@micbc";
my $USER2 = "mlview";
my $PASSWORD2 = "st00p1d";

# The number of days to keep backup files.
my $DAYS_TO_KEEP = 30;

# The list of databases to be backed up on the system.
my @DATABASES = ("dmg_merged_ml", "dmg_dts");

# The location where the backup files are to be stored.
my $STORAGE_DIRECTORY = "/net/work/db_backups";

# The list of monitors who should receive emails when there is a failure.
my @MONITORS = ("cully\@ucar.edu", "jja\@ucar.edu", "anstett\@ucar.edu");

&main();

##--------------------------------------------------------------------------------------------
# @signature void main()
# <p>Run the script to execute the <code>mysqldump</code> program on the DMG databases and
# store the dump files as backups on the system.</p>
##--------------------------------------------------------------------------------------------
sub main {
    # Used to hold error messages that need to be reported for the monitor.
    my $report = "";

    my ($today, $hour) = build_date();

    # Make sure the storage directory exists on the system.
    mkdir($STORAGE_DIRECTORY) unless (-e $STORAGE_DIRECTORY);

    # Loop through the databases to be backed up.
    foreach my $database (@DATABASES) {
	# Execute the mysqldump program for the database.
	# Note the 2>&1 1>$STORAGE_DIRECTORY/$HOST.$database.$today$hour.sqldump part of the command.  This
	# captures the standard output to the $STORAGE_DIRECTORY/$HOST.$database.$today$hour.sqldump file while
	# allowing the standard error content to be caught and added to the report.

        if ($database eq "dmg_merged_ml") {
#           $report .= "backing up $database\n";
	   $report .= `$MYSQLDUMP $OPTIONS -h $HOST -u $USER2 --password=$PASSWORD2 --databases $database 2>&1 1>$STORAGE_DIRECTORY/$HOST.$database.$today$hour.sqldump`;
           $report .= `gzip $STORAGE_DIRECTORY/$HOST.$database.$today$hour.sqldump`;
        } else {
#           $report .= "backing up $database\n";
	   $report .= `$MYSQLDUMP $OPTIONS -h $HOST -u $USER --password=$PASSWORD --databases $database 2>&1 1>$STORAGE_DIRECTORY/$HOST.$database.$today$hour.sqldump`;
           $report .= `gzip $STORAGE_DIRECTORY/$HOST.$database.$today$hour.sqldump`;
        }

	# Remove any files for the database over a month old.
	$report .= remove_old_files($database, $today);
    }

    # Send an error report to the monitors, but only if there was a problem.
    if ($report ne "") { send_mail($report); }
}

##--------------------------------------------------------------------------------------------
# @signature String build_date()
# <p>Get the current date from the file system in YYYYMMDD format.</p>
# @output $date The current date.
##--------------------------------------------------------------------------------------------
sub build_date {
    my @date = localtime();
    return sprintf("%04d%02d%02d", $date[5] + 1900, $date[4] + 1, $date[3]), sprintf("%02d", $date[2]);
}

##--------------------------------------------------------------------------------------------
# @signature String remove_old_files(String database, String date)
# <p>Remove the old files for the specified database from the system.  Old files are defined
# to be dated (from the filename) more than DAYS_TO_KEEP from the current date.</p>
# @input $database The name of the database to have the old files removed.
# @input $date The current date to use as the base date for old files.
# @output $report An accumulated list of error messages generated while trying to remove the
# files.
##--------------------------------------------------------------------------------------------
sub remove_old_files {
    my ($database, $date) = @_;

    # Accumulates error messages during the file deletion.
    my $report = "";

    # Define the first date that backup files can be removed.
    my $first_to_drop = (adjustDateTime($date, "YYYYMMDD", "000000", "HHMMSS", -1 * $DAYS_TO_KEEP, 0, 0, 0))[0]; 

    # Read the storage directory and find the list of backup database files.
    opendir(my $DIR, $STORAGE_DIRECTORY) or die("Unable to open $STORAGE_DIRECTORY\n");
    my @files = grep(/$database\.\d{10}\.sqldump\.gz/, readdir($DIR));
    closedir($DIR);

    # Loop through the files found on the system.
    foreach my $file (@files) {
	# Find the date from the file name that the file was created.
	$file =~ /\.(\d{8})(\d{2})\.sqldump\.gz$/;
	# Check to see if the current file is at least as old as the first to drop date.
	if (compareDates($1, "YYYYMMDD", $first_to_drop, "YYYYMMDD") >= 0) {
	    # Remove the file from the disk and generate an error message if it fails.
	    unlink(sprintf("%s/%s", $STORAGE_DIRECTORY, $file)) or 
		$report .= sprintf("Unable to delete file %s/%s: %s\n", $STORAGE_DIRECTORY, $file, $!);
	}
    }

    # Return the error report to the caller.
    return $report;
}

##--------------------------------------------------------------------------------------------
# @signature void send_mail(String message)
# <p>Send an email message to the monitors of the script.</p>
# @input message The contents of the email to send to the monitors.
##--------------------------------------------------------------------------------------------
sub send_mail {
    my ($message) = @_;
    
    open(my $SENDMAIL,"|/usr/lib/sendmail -t") || die("Unable to open sendmail\n");
    printf($SENDMAIL "Subject: MySQL Database Backup Error\n");
    printf($SENDMAIL "To: %s\n", join(",", @MONITORS));
    printf($SENDMAIL "Content-type: text/plain\n\n");
    printf($SENDMAIL "There was an error backing up the MySQL databases.  Details follow:\n\n");
    printf($SENDMAIL "$message");
    close($SENDMAIL);    
}
