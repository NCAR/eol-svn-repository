package Extraction;

use strict;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();

	$self->{dataset_object} = undef();

	return $self;
}

sub checkDefined
{
	my $self = shift;

	foreach my $p ("dsname", "pdname", "pjname", "pdsource")
	{
		$self->{$p} = defined( $self->{$p} )? $self->{$p} : "";
	}
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->{dsname} = shift( @$row );
	$self->{pdname} = shift( @$row );
	$self->{pjname} = shift( @$row );
	$self->{pdsource} = shift( @$row );

	$self->checkDefined();
}

sub dbPrepare
{
	my $self = shift;
}

1;
