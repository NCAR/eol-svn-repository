package Product;

use strict;

sub new
{
	my $class = shift;	
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();
	
	$self->{datasets} = undef();
	$self->{extractions} = undef();

	return $self;
}

sub checkDefined
{
	my $self = shift;

	$self->{pdname} = defined( $self->{pdname} )? $self->{pdname} : "";
	$self->{pjname} = defined( $self->{pjname} )? $self->{pjname} : "";
	$self->{status} = defined( $self->{status} )? $self->{status} : "";
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->{pdname} = shift( @$row );
	$self->{pjname} = shift( @$row );
	$self->{status} = shift( @$row );

	$self->checkDefined();
}

sub dbPrepare
{
	my $self = shift;
	$self->{status} = defined( $self->{status} )? $self->{status} : undef();
}

1;
