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
	$self->{proj_id} = undef;
	$self->{proj_name} = undef;
	$self->{sort_field} = undef;
	$self->{sort_dir} = undef;
	$self->{dis_count} = undef;
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
