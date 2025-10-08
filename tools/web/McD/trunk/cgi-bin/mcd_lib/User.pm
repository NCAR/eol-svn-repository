package User;


# --------------------------------------------------------------
# User.pm:
#  Contains the User class.
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() 
#
# sub set( $arr_ref )
#
# sub checkDefined()
#
# sub dbPrepare()
#
# --------------------------------------------------------------

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = "";
	$self->{display_name} = "";
	$self->{email} = "";
	$self->{active} = 0;

	return $self;
}

sub set
{
	my $self = shift;
	my $arr = shift;

	$self->{id} = shift( @$arr );
	$self->{display_name} = shift( @$arr );
	$self->{email} = shift( @$arr );
	$self->{active} = shift( @$arr );
}

sub checkDefined
{
	my $self = shift;

	$self->{id} = defined( $self->{id} )? $self->{id} : "";
	$self->{display_name} = defined( $self->{display_name} )? $self->{display_name} : "";
	$self->{email} = defined( $self->{email} )? $self->{email} : "";
	$self->{active} = defined( $self->{active} )? $self->{active} : 0;
}

sub dbPrepare
{
	my $self = shift;

	$self->{id} = defined( $self->{id} )? $self->{id} : undef();
	$self->{display_name} = (defined( $self->{display_name} ) && $self->{display_name} ne "")? $self->{display_name} : "";
	$self->{email} = (defined( $self->{email} ) && $self->{email} ne "")? $self->{email} : "";
	$self->{active} = (defined( $self->{active} ))? $self->{active} : 0;
}
1;
