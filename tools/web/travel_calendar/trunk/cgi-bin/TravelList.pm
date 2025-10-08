#! /usr/bin/perl -T -w

##Module---------------------------------------------------------------------
# <p>The <code>TravelList</code> module reads and writes travel data about 
# employees to the travel file.  It is a line by list of travel instances in 
# the format of:</p>
# <p><code>Travel ID, Start Date, End Date, Reason, Where</code></p>
# <p>The data is separated by the ^V separator, which can be changed if
# it is discovered to cause a problem, but will force the data file to be
# changed so it can be read in properly.</p>
# <p>The <code>Travel ID</code> is the unique id for the employees in the 
# employee file.</p>
# <p>The <code>Start Date</code> and <code>End Date</code> are the inclusive
# dates when the employee is going to be out of the office.</p>
# <p>The <code>Reason</code> field is the cause for the employee to be gone.
# </p>
# <p>The <code>Where</code> field is where the employee will be when they are
# out of the office.</p>
# <p>The <code>Travel Id</code> and the <code>Start Date</code> uniquely 
# define the travel information.</p>
#
# @author Joel Clawson
##Module---------------------------------------------------------------------
package TravelList;
use strict;
use lib ".";

# Constants used by the TravelList
my $DATA_FILE = "travel.dat";
my $SEPARATOR = "\026";

##---------------------------------------------------------------------------
# @signature void change_id(String orig_id, String new_id)
# <p>Change the <code>Travel Id</code> for all of the travel information for
# an employee.</p>
# @input orig_id The original travel id of the employee.
# @input new_id The new travel id of the employee.
##---------------------------------------------------------------------------
sub change_id {
  my $self = shift;
  if ($_[0] ne $_[1]) { # Only change the ids if they are different.
     $self->{"data"}->{$_[1]} = $self->{"data"}->{$_[0]};
     $self->deleteAllTravel($_[0]);
  }
}

##---------------------------------------------------------------------------
# @signature void deleteAllTravel(String id)
# <p>Remove all of the travel information for the specified employee.</p>
# @input id The travel id of the employee that is having their data removed.
##---------------------------------------------------------------------------
sub deleteAllTravel {
  my $self = shift;
  delete $self->{"data"}->{$_[0]};
  $self->save_file();
}

##---------------------------------------------------------------------------
# @signature void deleteTravel(String id, String date)
# <p>Remove a single travel instance for an employee.</p>
# @input id The travel id of the employee.
# @input date The start date of the travel time to be removed.
##---------------------------------------------------------------------------
sub delete_travel {
  my $self = shift;
  delete $self->{"data"}->{$_[0]}->{$_[1]};
  $self->save_file();
}

##---------------------------------------------------------------------------
# @signature int doesTravel(String id, String start, String end)
# <p>Check to see if the specified employee is traveling between the start
# and end dates, inclusive.</p>
# @input id The travel id of the employee.
# @input start The start date in YYYY/MM/DD format.
# @input end The end date in YYYY/MM/DD format.
# @output travel A true value if the specified employee travels between the
# specified dates, false otherwise.
##---------------------------------------------------------------------------
sub doesTravel {
    my $self = shift;
    foreach my $trav (keys %{ $self->{"data"}->{$_[0]}}) {
	my $start = $self->{"data"}->{$_[0]}->{$trav}->{"start"};
	my $end = $self->{"data"}->{$_[0]}->{$trav}->{"end"};
	$start =~ s/\///g;
	$end =~ s/\///g;
	if (($_[1] <= $start && $start <= $_[2]) || 
	    ($_[1] <= $end && $end <= $_[2]) ||
	    ($start <= $_[1] && $_[2] <= $end)) {
	    return 1;
	}
    }
    return 0;
}

##---------------------------------------------------------------------------
# @signature int doesTravelToday(String id, String date)
# <p>Check to see if an employee is gone on the specified date.</p>
# @input id The travel id of the employee.
# @input date The date to check with the travel info in YYYY/MM/DD format.
# @output travel A true value if the specified employee is gone on the 
# specified date, false otherwise.
##---------------------------------------------------------------------------
sub doesTravelToday {
    my $self = shift;
    foreach my $trav (keys %{ $self->{"data"}->{$_[0]}}) {
	my $start = $self->{"data"}->{$_[0]}->{$trav}->{"start"};
	my $end = $self->{"data"}->{$_[0]}->{$trav}->{"end"};
	$start =~ s/\///g;
	$end =~ s/\///g;
	if ($start <= $_[1] && $_[1] <= $end) {
	    return 1;
	}
    }
    return 0;
}

##---------------------------------------------------------------------------
# @signature String[] getAllTravelInfo(String id)
# <p>Get all of the travel information for the specified employee sorted by
# the most recent start date.</p>
# @input id The travel id of the employee.
# @output dates The start dates of the travel information in order of the 
# most recent start dates.
##---------------------------------------------------------------------------
sub getAllTravelInfo {
  my $self = shift;
  return (reverse sort keys %{ $self->{"data"}->{$_[0]}});
}

##---------------------------------------------------------------------------
# @signature String getBeginDate(String id, String date)
# <p>Get the start date of the travel information for the employee.</p>
# @input id The travel id for the employee.
# @input date The start date of the travel time.
# @return start The start date of the travel time.
##---------------------------------------------------------------------------
sub getBeginDate {
  my $self = shift;
  return $self->{"data"}->{$_[0]}->{$_[1]}->{"start"};
}

##---------------------------------------------------------------------------
# @signature String getEndDate(String id, String date)
# <p>Get the end date of the travel information for the employee.</p>
# @input id The travel id for the employee.
# @input date The start date of the travel time.
# @return end The end date of the travel time.
##---------------------------------------------------------------------------
sub getEndDate {
  my $self = shift;
  return $self->{"data"}->{$_[0]}->{$_[1]}->{"end"};
}

##---------------------------------------------------------------------------
# @signature String getReason(String id, String date)
# <p>Get the reason why the employee is going to out of the office.</p>
# @input id The travel id for the employee.
# @input date The start date of the travel time.
# @return reason The reason the employee will be out of the office.
##---------------------------------------------------------------------------
sub getReason {
  my $self = shift;
  return $self->{"data"}->{$_[0]}->{$_[1]}->{"reason"};
}

##---------------------------------------------------------------------------
# @signature String getStartOfTravel(String id, String date)
# <p>Find the start date of travel information given a date for an 
# employee.</p>
# @input id The travel id for the employee.
# @input date The date to use to find the start date in YYYY/MM/DD format.
# @output start The start date of the travel information.
##---------------------------------------------------------------------------
sub getStartOfTravel {
    my $self = shift;
    foreach my $trav (keys %{ $self->{"data"}->{$_[0]}}) {
	my $start = $self->{"data"}->{$_[0]}->{$trav}->{"start"};
	my $end = $self->{"data"}->{$_[0]}->{$trav}->{"end"};
	$start =~ s/\///g;
	$end =~ s/\///g;
	if ($start <= $_[1] && $_[1] <= $end) {
	    return $self->{"data"}->{$_[0]}->{$trav}->{"start"};
	}
    }    
}

##---------------------------------------------------------------------------
# @signature String getWhere(String id, String date)
# <p>Get where the employee will be when they are out of the office.</p>
# @input id The travel id for the employee.
# @input date The start date of the travel time.
# @return where Where the employee will be when they are out of the office.
##---------------------------------------------------------------------------
sub getWhere {
  my $self = shift;
  return $self->{"data"}->{$_[0]}->{$_[1]}->{"where"};
}

##---------------------------------------------------------------------------
# @signature void initialize()
# <p>Read the data from the DATA_FILE and put it into the list.</p>
# @warning This function will die if it cannot open the data file.
##---------------------------------------------------------------------------
sub initialize {
  my $self = shift;
  open(DAT, $DATA_FILE) || die("Can't open data file\n");
  while (<DAT>) {
    chomp($_);
    $self->insert_travel(split("$SEPARATOR", $_), 1);
  }
  close(DAT);
}

##---------------------------------------------------------------------------
# @signature void insert_travel(String id, String start, String end, String reason, String where, <i>int flag</i>)
# <p>Add new travel information to the list.</p>
# @input id The travel id of the employee.
# @input start The start date of the travel information in YYYY/MM/DD format.
# @input end The end date of the travel information in YYYY/MM/DD format.
# @input reason The reason the employee is going to travel.
# @input where Where the employee will be when they are out of the office.
# @input flag <b>Optional</b> This flag is used to to tell the function not to
# save the inserted data to the data file.  This is a special case used when
# loading the list from the file and should be ignored in other instances.
##---------------------------------------------------------------------------
sub insert_travel {
  my $self = shift;
  $self->{"data"}->{$_[0]}->{$_[1]}->{"start"} = $_[1];
  $self->{"data"}->{$_[0]}->{$_[1]}->{"end"}  = $_[2];
  $self->{"data"}->{$_[0]}->{$_[1]}->{"reason"} = $_[3];
  $self->{"data"}->{$_[0]}->{$_[1]}->{"where"} = $_[4];
  if ((defined($_[5]) && !$_[5]) || !defined($_[5])) { $self->save_file(); }
}

##---------------------------------------------------------------------------
# @signature TravelList new()
# <p>Create a new instance of the list from the DATA_FILE.</p>
# @output list The new TravelList instance.
##---------------------------------------------------------------------------
sub new {
  my $invocant = shift;
  my $self = {};
  my $class = ref($invocant) || $invocant;
  bless($self, $class);
  $self->initialize();
  return $self;
}

##---------------------------------------------------------------------------
# @signature void save_file()
# <p>Save the travel data in the list to the DATA_FILE.</p>
# @warning This function will die if it cannot open the DATA_FILE.
##---------------------------------------------------------------------------
sub save_file {
  my $self = shift;
  open(DAT, ">".$DATA_FILE) || die("Can't open data file\n");
  foreach my $id (keys %{ $self->{"data"}}) {
    foreach my $s_date (keys %{ $self->{"data"}->{$id}}) {
       printf(DAT "%s$SEPARATOR%s$SEPARATOR%s$SEPARATOR%s$SEPARATOR%s\n", $id, 
              $self->{"data"}->{$id}->{$s_date}->{"start"},
              $self->{"data"}->{$id}->{$s_date}->{"end"}, 
              $self->{"data"}->{$id}->{$s_date}->{"reason"},
	      $self->{"data"}->{$id}->{$s_date}->{"where"});
    }
  }
  close(DAT);
}

##---------------------------------------------------------------------------
# @signature void update_travel(String orig_id, String orig_start, String new_id, String new_start, String end, String reason, String where)
# <p>Update the travel data in the list with the new information.</p>
# @input orig_id The original travel id of the employee.
# @input orig_start The original start date of the travel time.
# @input new_id The new travel id of the employee.
# @input new_start The new start date of the travel time.
# @input end The new end date of the travel time.
# @input reason The new reason for the travel.
# @input where The new value for where the employee is going to be.
##---------------------------------------------------------------------------
sub update_travel {
  my $self = shift;
  if (defined($_[0])) {
     delete $self->{"data"}->{$_[0]}->{$_[1]};
  }
  $self->insert_travel($_[2], $_[3], $_[4], $_[5], $_[6], 0);
}

1;
