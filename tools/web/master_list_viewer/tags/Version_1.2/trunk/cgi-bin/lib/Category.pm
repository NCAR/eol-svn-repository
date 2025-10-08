#!/bin/perl

package Category;

# new( id, name, abreviation )
sub new
{
	my $class = shift;
	my $id = shift;
	my $name = shift;
	my $abrv = shift;

	my $self = {};	
	bless( $self, $class );

	$self->{id} = $id;
	$self->{name} = $name;
	$self->{abrv} = $abrv;
	return $self;
}

1;
