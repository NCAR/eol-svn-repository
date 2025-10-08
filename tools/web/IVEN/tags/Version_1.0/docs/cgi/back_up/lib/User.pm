package User;

use strict;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();

	return $self;
}

sub checkDefined
{
	my $self = shift;

	$self->{uid} = ( defined( $self->{uid} ) )? $self->{uid} : -1;
	$self->{fname} = ( defined( $self->{fname} ) )? $self->{fname} : "";
	$self->{lname} = ( defined( $self->{lname} ) )? $self->{lname} : "";
	$self->{email} = ( defined( $self->{email} ) )? $self->{email} : "";
	$self->{active} = ( defined( $self->{active} ) )? $self->{active} : 0;
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->{uid} = shift( @$row );
	$self->{fname} = shift( @$row );
	$self->{lname} = shift( @$row );
	$self->{email} = shift( @$row );
	$self->{active} = shift( @$row );

	$self->checkDefined();
}

sub dbPrepare
{
	my $self = shift;

	$self->{fname} = ( $self->{fname} && $self->{fname} ne "" )? $self->{fname} : undef();
	$self->{lname} = ( $self->{lname} && $self->{lname} ne "" )? $self->{lname} : undef();
	$self->{email} = ( $self->{email} && $self->{email} ne "" )? $self->{email} : undef();
	$self->{active} = ( defined( $self->{active} ) )? $self->{active} : 1;
}

1;
