#! /usr/bin/perl -T -w

##Module-------------------------------------------------------------------
# <p>The <code>EmployeeList</code> module reads and writes data about 
# employees to the employee file.  It is a line by list of employees in 
# the format of:</p>
# <p><code>Travel ID, First Name, Last Name, Color</code></p>
# <p>The data is separated by the ^V separator, which can be changed if
# it is discovered to cause a problem, but will force the data file to be
# changed so it can be read in properly.</p>
# <p>The <code>Travel ID</code> is a unique id for the employees in the 
# file.  It is what is displayed in the calendar to prevent it from trying
# to display duplicate names or long full names.</p>
# <p>The <code>First Name</code> and <code>Last Name</code> should be self
# explanatory.</p>
# <p>The <code>Color</code> is an HTML color name that is also unique to the
# individual employee.  It is used with the <code>Travel ID</code> to provide
# more differentiation between employees in the calendar.</p>
#
# @author Joel Clawson
##Module-------------------------------------------------------------------
package EmployeeList;
use strict;
use lib ".";
use TravelList;

# Constants for the Employee List
my $DATA_FILE = "employee.dat";
my $SEPARATOR = "\026"; # Control V (^V)


sub archive_list {
  my $self = shift;
  my @list = ();
  foreach my $id (sort keys %{ $self->{"data"}}) {
      if ($id =~ /^arch\d{3}/) { push(@list, $id); }
  }
  return @list;
}


##-------------------------------------------------------------------------
# @signature void delete_employee(String id)
# <p>Remove an employee from the list.</p>
# @input id The travel id of the employee being removed.
##-------------------------------------------------------------------------
sub delete_employee {
  my $self = shift;
  TravelList->new()->deleteAllTravel($_[0]);
  delete $self->{"data"}->{$_[0]};
  $self->save_file();
}

##-------------------------------------------------------------------------
# @signature String[] employee_list()
# <p>Get all of the employees' travel ids in alphabetical order.</p>
# @output list The alphabetical list of employee ids.
##-------------------------------------------------------------------------
sub employee_list {
  my $self = shift;
  my @list = ();
  foreach my $id (sort keys %{ $self->{"data"}}) {
      if ($id !~ /^arch\d{3}/) { push(@list, $id); }
  }
  return @list;
}


sub getArchiveCount {
    my $self = shift;
    my $count = 0;
    foreach my $id (keys %{ $self->{"data"}}) {
	if ($id =~ /^arch\d{3}/) { $count++; }
    }
    return $count;
}


##-------------------------------------------------------------------------
# @signature String getColor(String id)
# <p>Get the HTML color for an employee.</p>
# @input id The travel id of the employee.
# @output Return the HTML color for the specified employee.
##-------------------------------------------------------------------------
sub getColor {
  my $self = shift;
  return $self->{"data"}->{$_[0]}->{"color"};
}

##-------------------------------------------------------------------------
# @signature String getFirstName(String id)
# <p>Get the first name of an employee.</p>
# @input id The travel id of the employee.
# @output first The first name of the specified employee.
##-------------------------------------------------------------------------
sub getFirstName {
  my $self = shift;
  return $self->{"data"}->{$_[0]}->{"first"};
}

##-------------------------------------------------------------------------
# @signature String getLastName(String id)
# <p>Get the last name of an employee.</p>
# @input id The travel id of the employee.
# @output last The last name of the specified employee.
##-------------------------------------------------------------------------
sub getLastName {
  my $self = shift;
  return $self->{"data"}->{$_[0]}->{"last"};
}

##-------------------------------------------------------------------------
# @signature void initialize()
# <p>Create the <code>EmployeeList</code> with the data in the DATA_FILE.</p>
# @warning This function will die if it is unable to open the file.
##-------------------------------------------------------------------------
sub initialize {
  my $self = shift;
  open(DAT, $DATA_FILE) || 
      die("Can't open data file ".$self->{"data_file"}."\n");
  while (<DAT>) {
    chomp($_);
    $self->insert_employee(split("$SEPARATOR", $_), 1);
  }
  close(DAT);
}

##-------------------------------------------------------------------------
# @signature void insert_employee(String id, String first, String last, String color, <i>int flag</i>)
# <p>Insert an employee into the list.</p>
# @input id The travel id of the employee.
# @input first The first name of the employee.
# @input last The last name of the employee.
# @input color The HTML color for the employee.
# @input flag <b>Optional</b> If set, this will prevent the file from being
# saved.  This should be ignored or set to zero in most instances.
##-------------------------------------------------------------------------
sub insert_employee {
  my $self = shift;
  $self->{"data"}->{$_[0]}->{"first"} = $_[1];
  $self->{"data"}->{$_[0]}->{"last"}  = $_[2];
  $self->{"data"}->{$_[0]}->{"color"} = $_[3];
  if ((defined($_[4]) && !$_[4]) || !defined($_[4])) { $self->save_file(); }
}

##-------------------------------------------------------------------------
# @signature EmployeeList new()
# <p>Create a new <code>EmployeeList</code> and load the data from the 
# DATA_FILE.</p>
##-------------------------------------------------------------------------
sub new {
  my $invocant = shift;
  my $self = {};
  my $class = ref($invocant) || $invocant;
  bless($self, $class);
  $self->initialize();
  return $self;
}

##-------------------------------------------------------------------------
# @signature void save_file()
# <p>Save the data in the list to the DATA_FILE.</p>
# @warning This function will die if the DATA_FILE is not able to be opened.
##-------------------------------------------------------------------------
sub save_file {
  my $self = shift;
  open(DAT, ">".$DATA_FILE) || 
      die("Can't open data file ".$self->{"data_file"}."\n");
  foreach my $id (keys %{ $self->{"data"}}) {
    printf(DAT "%s$SEPARATOR%s$SEPARATOR%s$SEPARATOR%s\n", $id, 
	   $self->{"data"}->{$id}->{"first"}, $self->{"data"}->{$id}->{"last"},
	   $self->{"data"}->{$id}->{"color"});
  }
  close(DAT);
}

##-------------------------------------------------------------------------
# @signature void update_employee(String orig_id, String new_id, String first, String last, String color)
# <p>Change the specified employee information to the new information.</p>
# @input orig_id The original travel id of the employee.
# @input new_id The new travel id of the employee.
# @input first The new first name of the employee.
# @input last The new last name of the employee.
# @input color The new HTML color for the employee.
##-------------------------------------------------------------------------
sub update_employee {
   my $self = shift;
   delete $self->{"data"}->{$_[0]};
   $self->insert_employee($_[1], $_[2], $_[3], $_[4]);
   TravelList->new()->change_id($_[0], $_[1]);
}

1;


