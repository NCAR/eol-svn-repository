package Dataset;

# --------------------------------------------------------------
# Dataset.pm:
#  Contains the Dataset class.
#
# Author - Dan Sullivan
# Date - July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - Returns a new instane of the Dataset class
#
# sub set( $arr_ref ) - Sets the instance variables in this Dataset with the
#  given values.
#
# sub checkDefined() - Sets default values for undefined values 
#  returned by the database
#
# sub dbPrepare() - Prepares the Dataset to be entered into
#  the database.  In general sets blank text fields to undef();
# --------------------------------------------------------------

use lib ".";
use Status;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = 0;
	$self->{storm_id} = undef();
	$self->{title} = "";
	$self->{date} = "";
	$self->{remote_url} = "";

	$self->{status} = Status->new();

	return $self;
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->{id} = shift(@$row);
	$self->{storm_id} = shift(@$row);
	$self->{title} = shift(@$row);
	$self->{date} = shift(@$row);
	$self->{remote_url} = shift(@$row);

	$self->{status} = Status->new();
	$self->{status}->set( $row );

	$self->checkDefined();
}

sub checkDefined
{
	my $self = shift;

	$self->{id} = ( defined( $self->{id} ) )? $self->{id} : -1;
	$self->{storm_id} = ( defined( $self->{storm_id} ) )? $self->{storm_id} : "";
	$self->{title} = ( defined( $self->{title} ) )? $self->{title} : "";
	$self->{date} = ( defined( $self->{date} ) )? $self->{date} : "0000-00-00";
	$self->{remote_url} = ( defined( $self->{remote_url} ) )? $self->{remote_url} : "";
}


sub dbPrepare
{
	my $self = shift;

	$self->{id} = ( defined($self->{id}) )? $self->{id} : 0;
	$self->{storm_id} = ( defined($self->{storm_id}) && $self->{storm_id} ne ""  )? $self->{storm_id} : undef();
	$self->{title} = ( defined( $self->{title} ) && $self->{title} ne "" )? $self->{title} : undef();
	$self->{date} = ( defined( $self->{date} ) && $self->{date} ne "" )? $self->{date} : undef();
	$self->{remote_url} = ( defined( $self->{remote_url} ) && $self->{remote_url} ne ""  )? $self->{remote_url} : undef(); 

	$self->{status}->dbPrepare();
}
1;
