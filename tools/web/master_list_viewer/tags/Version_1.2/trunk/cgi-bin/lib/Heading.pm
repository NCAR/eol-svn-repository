#!/bin/perl

package Heading;

# new( name, abrv, is_category, category1, category2... )
sub new
{
	my $class = $_[0];
	my $name = $_[1];
	my $abrv = $_[2];
	my $is_cat = $_[3];

	my $self = {};

	$self->{name} = $name;
	$self->{abrv} = $abrv;
	$self->{is_category} = $is_cat; 
	$self->{count} = 0;
	$self->{categories} = [];

	for( my $x = 4; $x < @_; $x++ )
	{
		$self->{categories}[$self->{count}] = $_[$x];
		$self->{count}++;
	}
	return $self;
}

1;
