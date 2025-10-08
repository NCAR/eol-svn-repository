#!/bin/perl -w

package Table;

use strict;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{array} = [];
	$self->{count} = 0;
	$self->{sortfield} = undef;
	$self->{tabletype} = undef;
	$self->{displaycount} = undef;
	$self->{sortdir} = undef;	
	$self->{secsort} = undef;
	$self->{proj_id} = undef;
	return $self;
}

sub addRow
{
	my $self = shift;
	my $row = shift;
	$self->{array}[$self->{count}] = $row;
	$self->{count}++;
}

sub getRowById
{
	my $self = shift;
	my $id = shift;
	my $ret = undef;

	foreach my $row ( @{$self->{array}} )
	{
		if( $row->{id} == $id )
		{
			$ret = $row;
			last;
		}
	} 

	return $ret;
}
1;
