#! /usr/bin/perl -w
##Module---------------------------------------------------------------------------
# <p>The PrecipAccumulator is a data structure to assist in the accumulation of
# precipitation values over a period of time.  It assumes all frequencies are
# specified in minutes.  This means that hourly and daily need to be converted to
# minutes.</p>
# <p><font color="red"><b>This does <font size="+1">NOT</font> handle accumulating 
# precipitation.  It only will sum known values.  It cannot calculated differences
# between times to determine the precipitation at a given time.</b></font></p>
# <p>The accumulator currently knows about the following frequencies:</p><ul>
#   <li>   1 -  1 minute</li>
#   <li>   2 -  2 minute</li>
#   <li>   5 -  5 minute</li>
#   <li>  10 - 10 minute</li>
#   <li>  15 - 15 minute</li>
#   <li>  20 - 20 minute</li>
#   <li>  30 - 30 minute</li>
#   <li>  60 - hourly</li>
#   <li>1440 - daily</li>
# </ul>
# <p>This module is defined in a manner that will allow the user to hold as much
# as little data as the user wishes for a single station.  (For multiple stations,
# a new accumulator will need to be generated for each station.)  This means that
# the accumulator can hold all of the data or can incrememntally hold a specified
# amount of data (such as an hour or a day) to save memory.  If the accumulator is
# used to only hold the specified amount, it would be wise to use the <code>clear
# </code> function to remove the inserted data once it has been used to free up
# memory and increase the speed of the software using the data structure.  If the
# space is not freed, it will be remain in the accumulator until the entire data
# structure is no longer needed and is garbage collected.</p>
# <p><font color="red">IMPORTANT NOTES:</font></p><ol>
#   <li>The constructor (<code>new</code>) must have the an open file handle for
# displaying error/warning messages.  If the messages are not wanted in a file, use
# <code>*STDOUT</code> or <code>*STDERR</code> to have them printed to the screen.</li>
#   <li><p>The <code>addPrecip</code> function will not overwrite a value already in
# the accumulator.  The <code>overwritePrecip</code> function will do this instead.
# This also works in the other direction as well.  The <code>overwritePrecip</code>
# function will not set a value in the accumulator unless it has already been set by
# a defined value that is not missing.</p>
#       <p>These functions were designed this way to prevent a user from accidently
# overwritting a value already in the accumulator and to let the user know that there
# is multiple values for a certain date/time.  It also prevents the user from 
# recklessly using the overwrite function to set everything and potentially overwriting
# something that should not have been overwritten.  This will make the use of the
# accumulator a little more difficult, but should provide a more responsible and 
# useful data structure.</p></li>
#   <li>All of the date/times (whether input or output) are expected to exactly match
# the corresponding input or output frequency.  This means that a 10 minute frequency
# must have its times on the 10 minutes (:00, :10, :20, :30, :40, :50), hourly on the
# hour (:00), daily on the 0 hour (00:00), and so on.  Any other time for the frequency 
# will result in the function that was called to die.</li>
#   <li>Negative values will not be accepted.  The functions <code>addPrecip</code>
# and <code>overwritePrecip</code> check to see if the specified value is negative.
# They will print a warning to the open file handle and set the value to missing.  If
# another action should be taken with negative values, it <b>must</b> be handled
# before it gets added to the accumulator.</li>
# </ol>
#
# @since Version 4
#
# @author Joel Clawson
# @version 4.0 This is the original creation of the module.
# @author Joel Clawson
# @version 4.1 Added the 2 minute frequency to list of acceptable frequencies.
##Module---------------------------------------------------------------------------
package PrecipAccumulator;
use strict;
use lib "/net/work/lib/perl/Utilities";
use DpgConstants qw(:DEFAULT);
use DpgConversions qw(:DEFAULT);
use DpgDate qw(:DEFAULT);
$| = 1;

##---------------------------------------------------------------------------------
# @signature int addPrecip(String date, String date_fmt, String time, String time_fmt, float value)
# <p>Add a precipitation value into the accumulator for a specific date and time.</p>
#
# @input $date The date of the precip being added.
# @input $date_fmt The format of the date using YMDJ characters.
# @input $time The time of the precip being added.
# @input $time_fmt The format of the time using HMS characters.
# @input $value The value of the precipitation at the specified date and time.
# @output $success If the value was added to the accumulator.
# @warning This function will die if the date/time does not match the input frequency.
##---------------------------------------------------------------------------------
sub addPrecip {
    if (@_ != 6) { die("Invalid parameters to PrecipAccumulator::addPrecip\n"); }

    my ($self,$date,$date_fmt,$time,$time_fmt,$value) = @_;
    my $LOG = $self->{"logger"};

    # Change the date and time to a common format.
    $date = formatDate($date,$date_fmt,"YYYY/MM/DD");
    $time = formatTime($time,$time_fmt,"HH:MM");

    # Check the time with the input frequency.
    if (!$self->isValidTime($time,$self->{"frequency"}->{"in"})) {
	die(sprintf("Cannot add precip for %s %s.  It does not match the frequency of %s minute.\n",$date,$time,$self->{"frequency"}->{"in"}));
    }

    # Make sure the value is defined.
    $value = $self->{"missing"} unless(defined($value));

    # Make sure the precip value is not negative
    if ($value < 0 && $value != $self->{"missing"}) {
	printf($LOG "The value for time %s %s is negative (%s).  It is being set to missing.\n",
	       $date,$time,$value);
	$value = $self->{"missing"};
    }

    # Handle cases where the precip has already been set.
    if (defined($self->{"data"}->{$date}->{$time}->{"value"})) {
	printf($LOG "Precip at %s %s is already defined.",$date,$time);

	if ($self->{"data"}->{$date}->{$time}->{"value"} == $value) {
	    printf($LOG "  The values are equal.  Ignoring second value.\n");
	} else {
	    printf($LOG "  Keeping the first value: %s,  dropping the second value: %s.\n",
		   $self->{"data"}->{$date}->{$time}->{"value"},$value);
	    return $FALSE;
	}
    }

    # Set the precip value
    $self->{"data"}->{$date}->{$time}->{"value"} = $value unless($value == $self->{"missing"});

    return $TRUE;
}

#---------------------------------------------------------------------------------
# @signature void assignFrequencies(int in_freq, int out_freq)
# <p>An auxillary function for the constructor.  It is used to make sure that the
# input and output frequencies are valid and the input frequecy can be accumulated
# into the output frequency.</p>
#
# @input $in_freq The input frequency in minutes.
# @input $out_freq The output frequency in minutes.
# @warning This function will die if any of the following cases are true.  1. The
# input frequency is not recognized by the accumulator.  2. The output frequency is
# not recognized by the accumulator.  3. The input frequency cannot be accumulated
# into the output frequency.  (This includes trying to accumulate to more frequent
# data or into the same frequency.  i.e 10 -> 5 or 10 -> 10)
#---------------------------------------------------------------------------------
sub assignFrequencies {
    my ($self,$in_freq,$out_freq) = @_;

    my @valids = @{ $self->{"frequency"}->{"list"}};

    # Make sure the input and output frequencies are known by the accumulator.
    my ($in,$out) = ($FALSE,$FALSE);
    foreach my $valid (@valids) {
	$in = $TRUE if ($in || $in_freq == $valid);
	$out = $TRUE if ($out || $out_freq == $valid);
    }
    die("In frequency $in_freq is not a recognized frequency!\n") if (!$in);
    die("Out frequency $out_freq is not a recognized frequency!\n") if (!$out);

    # Make sure the input freq can be accumulated to the output freq.
    if ($in_freq >= $out_freq || (int($out_freq/$in_freq)) != $out_freq/$in_freq) {
	die("Cannot accumulate $in_freq minute data to $out_freq minute!\n");
    }

    # Set the values into the object.
    $self->{"frequency"}->{"in"} = $in_freq;
    $self->{"frequency"}->{"out"} = $out_freq;
}

##---------------------------------------------------------------------------------
# @signature void clear()
# <p>Remove all of the data in the accumulator.</p>
##---------------------------------------------------------------------------------
sub clear {
    my $self = shift;
    delete($self->{"data"});
}

##---------------------------------------------------------------------------------
# @signature float getAccumulation(String date, String date_fmt, String time, String time_fmt)
# <p>Get the accumulated precipitation amount for the specified date and time.</p>
#
# @input $date The date to be accumulated.
# @input $date_fmt The format of the inputted date.
# @input $time The time to be accumulated.
# @input $time_fmt The format of the inputted time.
# @output $accum The accumulated preciptation for the time.  Itwill return the preset 
# missing value if any of the times needed for the accumulation have not been set or
# were set to missing.
# @warning This function will die if the date/time to be accumulated does not match
# the output frequency.
##---------------------------------------------------------------------------------
sub getAccumulation {
    if (@_ != 5) { die("Invalid parameters to PrecipAccumulator::getAccumulation\n"); }
    my ($self,$date,$date_fmt,$time,$time_fmt) = @_;

    # Change the date and time into an expected format.
    $date = formatDate($date,$date_fmt,"YYYY/MM/DD");
    $time = formatTime($time,$time_fmt,"HH:MM");

    # Make sure the date/time matches the output freq.
    if (!$self->isValidTime($time,$self->{"frequency"}->{"out"})) {
	die(sprintf("Cannot accumulate precip for %s %s.  It does not match the frequency of %s minute.\n",$date,$time,$self->{"frequency"}->{"out"}));
    }

    # Accumulate the precip for the date/time.
    my $accum = 0;
    for (my $i = 0; $i < $self->{"frequency"}->{"out"} / $self->{"frequency"}->{"in"}; $i++) {
	my ($date,$time) = adjustDateTime($date,"YYYY/MM/DD",$time,"HH:MM",0,0,
					  -1*$i*$self->{"frequency"}->{"in"},0);

	if (defined($self->{"data"}->{$date}->{$time}->{"value"})) {
	    $accum += $self->{"data"}->{$date}->{$time}->{"value"};
	} else {
	    # Not defined, no sense in continuing the accumulation.
	    return $self->{"missing"};
	}
    }
    return $accum;
}

##---------------------------------------------------------------------------------
# @signature float getPrecip(String date, String date_fmt, String time, String time_fmt)
# <p>Get a precipitation value that was previously set for a specified date/time.</p>
#
# @input $date The date to be retreived.
# @input $date_fmt The format of the input date.
# @input $time The time to be retreived.
# @input $time_fmt The format of the input time.
# @output $value The non-accumulated value for the date/time or undef if the value
# was never set or was set to missing.
# @warning This function will die if the date/time is not on the input frequency.
##---------------------------------------------------------------------------------
sub getPrecip {
    if (@_ != 5) { die("Invalid parameters to PrecipAccumulator::getPrecip\n"); }
    my ($self,$date,$date_fmt,$time,$time_fmt) = @_;

    # Convert the date and time into an expected format.
    $date = formatDate($date,$date_fmt,"YYYY/MM/DD");
    $time = formatTime($time,$time_fmt,"HH:MM");

    # Make sure the date/time matches the input frequency.
    if (!$self->isValidTime($time,$self->{"frequency"}->{"in"})) {
	die(sprintf("Cannot get precip for %s %s.  It does not match the frequency of %s minute.\n",$date,$time,$self->{"frequency"}->{"in"}));
    }

    return $self->{"data"}->{$date}->{$time}->{"value"};
}

#---------------------------------------------------------------------------------
# @signature int isValidTime(String time, int freq)
# <p>Check to see if the time aligns with the frequency.</p>
#
# @input $time The time to be checked in HH:MM format.
# @input $freq The frequency to check the time against.
# @return true if the time aligns with the frequency, false otherwise.
#---------------------------------------------------------------------------------
sub isValidTime {
    my ($self,$time,$freq) = @_;
    my ($hour,$min) = split(/:/,$time);

    $min += convertTime($hour,"hr","min");
    return ($min % $freq) == 0;
}

##---------------------------------------------------------------------------------
# @signature PrecipAccumulator new(FileHandle ACCUM, int in_freq, int out_freq, float missing)
# <p>Create a new PrecipAccumulator.</p>
#
# @input $ACCUM The FileHandle where processing logs/errors are written.
# @input $in_freq The input frequency of the data.
# @input $out_freq The ouput frequency to be generated.
# @input $missing The missing value of input precipitation values.
##---------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

    if (@_ != 4) { die("Invalid parameters to PrecipAccumulator::new\n"); }
    my ($ACCUM,$in_freq,$out_freq,$missing) = @_;
    
    $self->{"logger"} = $ACCUM;
    @{ $self->{"frequency"}->{"list"}} = (1,2,5,10,15,20,30,60,1440);
    $self->assignFrequencies($in_freq,$out_freq);
    $self->{"missing"} = defined($missing) ? $missing : -999.99;

    return $self;
}

##---------------------------------------------------------------------------------
# @signature int overwritePrecip(String date, String date_fmt, String time, String time_fmt, float value)
# <p>Overwrite the current precipitation value at the specified date/time.</p>
#
# @input $date The date to be overwritten.
# @input $date_fmt The format of the input date.
# @input $time The time to be overwritten.
# @input $time_fmt The format of the input time.
# @input $value The new precipitation value for the date/time.
# @output $success If the old value was successfully overwritten with the new value.
# @warning This function will die if the date/time is not aligned with the input 
# frequency or if the value has not been previously set.
##---------------------------------------------------------------------------------
sub overwritePrecip {
    if (@_ != 6) { die("Invalid parameters to PrecipAccumulator::overwritePrecip\n"); }
    my ($self,$date,$date_fmt,$time,$time_fmt,$value) = @_;
    my $LOG = $self->{"logger"};

    # Convert the date and time to a common format.
    $date = formatDate($date,$date_fmt,"YYYY/MM/DD");
    $time = formatTime($time,$time_fmt,"HH:MM");

    # Make sure the date/time aligns with the input frequency.
    if (!$self->isValidTime($time,$self->{"frequency"}->{"in"})) {
	die(sprintf("Cannot overwrite precip for %s %s.  It does not match the frequency of %s minute.\n",$date,$time,$self->{"frequency"}->{"in"}));
    }

    # Make sure the input value is defined.
    $value = $self->{"missing"} unless(defined($value));

    # Make sure the precip value is not negative
    if ($value < 0 && $value != $self->{"missing"}) {
	printf($LOG "The value for time %s %s is negative (%s).  It is being set to missing.\n",
	       $date,$time,$value);
	$value = $self->{"missing"};
    }

    if (!defined($self->{"data"}->{$date}->{$time}->{"value"})) {
	die(sprintf("Precip at %s %s is not defined yet. Cannot overwrite value.\n",
		    $date,$time));
    } else {
	printf($LOG "Overwrite precip at %s %s.",$date,$time);
	if ($self->{"data"}->{$date}->{$time}->{"value"} == $value) {
	    printf($LOG "  The values are equal.  Ignoring second value.\n");
	    return $FALSE;
	} else {
	    printf($LOG "  Dropping the first value: %s, keeping the second value: %s.\n",
		   $self->{"data"}->{$date}->{$time}->{"value"},$value);
	}
    }

    # Convert a missing value to undefined.
    $value = undef() if ($value == $self->{"missing"});

    # Overwrite the value.
    $self->{"data"}->{$date}->{$time}->{"value"} = $value;

    return $TRUE;
}

























