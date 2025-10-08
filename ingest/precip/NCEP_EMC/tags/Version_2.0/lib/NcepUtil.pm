#! /usr/bin/perl -w

##Module------------------------------------------------------------------
# The NcepUtil module is a collection of utility programs to assist the
# other Ncep modules in downloading and archiving the NCEP data from
# Sid Katz.
#
# @author Joel Clawson
##Module------------------------------------------------------------------
package NcepUtil;
use Email::MIME;
use Email::MIME::Creator;
use IO::All;
use strict;

##---------------------------------------------------------------------------
# @signature (String, String) adjustDateTime(String date, String time, int day_offset, int hour_offset, int min_offset)
# <p>Shift the specified date and time by the amount specified in either 
# positive or negative values.  A combination of negative and positive value
# will subtract and add the values given and does not imply all one or the
# other.
# 
# @input $date The date in YYYY/MM/DD format.
# @input $time The time in HH:MM format.
# @input $day_offset The number of days to increase/decrease the date by.
# @input $hour_offset The number of hours to increase/decrease the hour by.
# @input $min_offset The number of hours to increase/decrease the minutes by.
##---------------------------------------------------------------------------
sub adjustDateTime {
    my $date = shift;
    my $time = shift;
    my $day_offset = shift;
    my $hour_offset = shift;
    my $min_offset = shift;
   
    my ($year, $month, $day) = split('/', $date);
    $day = convertJulian($year, $day, $month);
    my ($hour, $min) = split(':', $time);
    
    $min += $min_offset; 
    while ($min > 59) { $hour_offset++; $min -= 60; }
    while ($min < 00) { $hour_offset--; $min += 60; }
    
    $hour += $hour_offset;
    while ($hour > 23) { $day_offset++; $hour -= 24; }
    while ($hour < 00) { $day_offset--; $hour += 24; }
    
    $day += $day_offset;
    my $days_in_year = (days_in_feb($year) == 28) ? 365 : 366;
    while ($day > $days_in_year) { 
	$year++;
	$day -= $days_in_year;
	$days_in_year = (days_in_feb($year) == 28) ? 365 : 366;
    }
    while ($day < 1) {
	$year--;
	$day += (days_in_feb($year) == 28) ? 365 : 366;
    }

    return (sprintf("%04d/%02d/%02d", $year, convertJulian($year, $day)),
	    sprintf("%02d:%02d", $hour, $min));
}

##------------------------------------------------------------------------
# @signature String addresses()
# <p>Get the list of email addresses who should get all of the emails sent
# by the scripts.</p>
#
# @output $list The comma delimited list of email addresses.
##------------------------------------------------------------------------
sub addresses {
    return "jclawson\@ucar.edu,janine\@ucar.edu";
}

##--------------------------------------------------------------------------
# @signature (int, int) convertJulian(int year, int julian)
# <p>Convert a julian date into a month and day.</p>
# @input $year The year of the julian date.
# @input $julian The julian date.
# @output $month The month of the julian date.
# @output $day The day in the month of the julian date.
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
# @signature int convertJulian(int year, int days, int month)
# <p>Convert a month and day into a julian date.</p>
# @input $year The year of the date.
# @input $days The day of the month of the date.
# @input $month The month of the date.
# @output $julian The julian date from the month and day for the year.
##--------------------------------------------------------------------------
sub convertJulian {
  my $days = $_[1] + 0;
  my $month = $_[2];

  my @daysInMonth = (31,days_in_feb($_[0]),31,30,31,30,31,31,30,31,30,31);

  if (defined($month)) {
    while ($month > 1) {
      $days += $daysInMonth[$month-2] if defined($daysInMonth[$month-2]);
      $month--;
    }
    return $days;
  } else {
    $month = 1;
    while ($days > $daysInMonth[$month - 1]) {
      $days = $days - $daysInMonth[$month - 1];
      $month++;
    }
    return ($month, $days);
  }
}

##------------------------------------------------------------------------
# @signature int createDirectory(String path)
# <p>Create the directory structure specified in the path.</p>
#
# @input $path The path to be created.
# @output $success A boolean value if the directory was able to be created.
##------------------------------------------------------------------------
sub create_directory {
    my $path = shift;
    my @dirs = split(/\//,$path);
    my $count = 1;
    my $accum_dir = $dirs[0];
    while ($count < scalar(@dirs)) {
        $accum_dir = sprintf("%s/%s",$accum_dir,$dirs[$count]);
        if (!(-e $accum_dir)) {
            mkdir($accum_dir) || return 0;
	    chmod(0775,$accum_dir);
        }
        $count++;
    }
    return 1;
}

##------------------------------------------------------------------------
# @signature int days_in_feb(int year)
# <p>Determine the number of days in February for the specified year.</p>
#
# @input $year The year to get the day count.
# @output $days The number of days in Feb. for the year.
##------------------------------------------------------------------------
sub days_in_feb {
    my $year = shift;
    return ((($year % 4 == 0) && ($year % 100 != 0)) || ($year % 400 == 0))
	? 29 : 28;
}

##------------------------------------------------------------------------
# @signature String dssMSS()
# <p>Get the mass store directory where the Ncep tar ball will be placed.
# @output $dir The mass store directory including the mss:
##------------------------------------------------------------------------
sub dssMSS { return "mss:/DSS/DS507.5/updates"; }

##------------------------------------------------------------------------
# @signature int file_size(String file)
# <p>Get the size of the specified file.</p>
#
# @input $file The file to get the size of.
# @output $size The size of the specified file.
##------------------------------------------------------------------------
sub file_size { return (stat($_[0]))[7]; }

##------------------------------------------------------------------------
# @signature String final_email_addresses()
# <p>Get the list of email addresses that will receive the final email 
# after a successful run.
#
# @output $addresses The list of final email addresses.
##------------------------------------------------------------------------
sub final_email_addresses {
    return "Sid.Katz\@noaa.gov,".addresses();
}

##------------------------------------------------------------------------
# @signature int get_days_in_month(int month, int year)
# <p>Get the number of days in the specified month and year.</p>
#
# @input $month The month to use to get the number of days.
# @input $year The year of the month.
# @output $days The number of days for the specified month and year.
##------------------------------------------------------------------------
sub get_days_in_month {
    my $month = shift;
    my $year = shift;
    my @days = (31,days_in_feb($year),31,30,31,30,31,31,30,31,30,31);

    return $days[$month - 1];
}

##------------------------------------------------------------------------
# @signature String jossMSS(int year)
# <p>Get the directory where the ceopavn tar ball is stored on the mass store.</p>
#
# @input $year The year of the data in the tar ball to be archived.
# @output $path The mass store path where the files are stored for the specified year.
##------------------------------------------------------------------------
sub jossMSS { return sprintf("mss:/JOSS/DATA/RAW/BY_PROJECT/CEOP/%04d",$_[0]); }

##------------------------------------------------------------------------
# @signature String ncep_archive()
# <p>Get the directory where the NCEP data is archived for CODIAC.</p>
# @return $path The directory path wher the NCEP data is archived.
##------------------------------------------------------------------------
sub ncep_archive { return "/archive/data/eop/katz_data"; }

##------------------------------------------------------------------------
# @signature String ncep_home()
# <p>Get the directory which is the working home directory for the NCEP
# processing.</p>
# @output $path The directory path where NCEP processing takes place.
##------------------------------------------------------------------------
#sub ncep_home { return "/home/jclawson/NCEP_EMC"; }
sub ncep_home { return "/work/operational/surface/NCEP_EMC"; }

##------------------------------------------------------------------------
# @signature String ncep_image_archive()
# <p>Get the location where image files are to be archived.</p>
# @output $path The path of where image files are archived.
##------------------------------------------------------------------------
sub ncep_image_archive { return "/archive/data/eop/katz_data"; }

##------------------------------------------------------------------------
# @signature String mass_store(String local_file, String mss_file, String options)
# <p>Place the local file onto the mass store.</p>
#
# @input $local_file The file to be placed on the mass store.
# @input $mss_file The full path and file name of the file on the mass store.
# @input $options A list of options to use to place the file on the mass store.
# @output $err An error message from the system command.
##------------------------------------------------------------------------
sub mass_store {
    return system(sprintf("/opt/dcs/bin/msrcp -pe 32767 %s %s %s",$_[2],$_[0],$_[1]));
}

##------------------------------------------------------------------------
# @signature String monthAbbrev(int month)
# <p>Get the 3 character abbreviation for the specified month.</p>
#
# @input $month The month to get the abbreviation for.
##------------------------------------------------------------------------
sub monthAbbrev {
    return substr("janfebmaraprmayjunjulaugsepoctnovdec",($_[0]-1)*3,3);
}

##------------------------------------------------------------------------
# @signature String send_mail(String body, String subject, String to, String cc, <i>String attach_file</i>)
# <p>Send an email message to the specified recipients.</p>
#
# @input $body The message body.
# @input $subject The subject of the message.
# @input $to The addresses that will be placed in the "To:" field.
# @input $cc The addresses that will be placed in the "Cc:" field.
# @input $attach_file <b>Optional</b> A file to attach to the message.
# @output $err An error message that prevented an email from being sent.
##------------------------------------------------------------------------
sub send_mail {
    my $body = shift;
    my $subject = shift;
    my $to = shift;
    my $cc = shift;
    my $attach_file = shift;

    my $reply = "jclawson\@ucar.edu";

    # Create the body of the message
    my @parts = (Email::MIME->create(attributes => {
	content_type => "text/plain" }, body => $body));

    # Only add the attachment if there is an attachment to send.
    if (defined($attach_file)) {
	my @dirs = split(/\//, $attach_file);
	my $attach = Email::MIME->create(attributes => {
	    filename => $dirs[scalar(@dirs) - 1],
	    content_type => "text/plain",
	    disposition => "attachment",
	    charset => "US-ASCII"},
					 body => io($attach_file)->all());
	push(@parts, $attach);
    }
    
    # Create the final email message
    my $email = Email::MIME->create(parts => [ @parts ]);

    # Generate the header
    $email->header_set("From" => $reply);
    $email->header_set("Reply_to" => $reply);
    $email->header_set("To" => $to);
    $email->header_set("Cc" => $cc);
    $email->header_set("Subject" => $subject);

    # Send the message.
    my $SENDMAIL;
    open($SENDMAIL, "|/usr/lib/sendmail -t") || return ("Unable to open sendmail.");
    printf($SENDMAIL $email->as_string());
    close($SENDMAIL);

    return "";
}

##------------------------------------------------------------------------
# @signature String tar(String tar_file, String files, <i>String output</i>)
# <p>Put the files into the specified tar ball and optionally print the 
# output from the command to the output file.</p>
#
# @input $tar_file The name of the tar ball to be created.
# @input $files The files or pattern of files to put into the tar ball.
# @input $output <b>Optional</b> The output file to store the information
# generated from creating the tar ball.
# @output $msg An error message from the system.
##------------------------------------------------------------------------
sub tar {
    my $tar_file = shift;
    my $files = shift;
    my $output = shift;

    if (defined($output)) {
	return system(sprintf("/bin/tar -cvf %s %s > %s",$tar_file,$files,$output));
    } else {
	return system(sprintf("/bin/tar -cf %s %s",$tar_file,$files));
    }
}

##------------------------------------------------------------------------
# @signature String uncompress(String file)
# <p>Uncompress the specified file into the current working directory.</p>
#
# @input $file The file to be uncompressed.
# @output $msg An error message from the system.
##------------------------------------------------------------------------
sub uncompress { return system(sprintf("/bin/uncompress %s",$_[0])); }

##------------------------------------------------------------------------
# @signature String untar(String file)
# <p>Untar the specified file into the current working directory.</p>
#
# @input $file The name of the file to be untarred.
# @output $msg An error message from the system.
##------------------------------------------------------------------------
sub untar { return system(sprintf("/bin/tar -xf %s",$_[0])); }

1;




