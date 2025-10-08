#!/bin/perl -w

package Table;

#-----------------------------------------------------------------
# Table.pm: This module contains a table class that maintains an
#  array of Rows and various information for the data loading
#  notes page.
#
# Subroutines:
#  new - constructs a new Table
#  addRow - adds a new row to the Table
#  getRowById - returns the row with the given id number
#
# Properties:
#	  sortfield - the field by which this table is sorted
#	  tabletype - the type of table to display (long/wide)
#	  displaycount - the number of rows to display
#	  sortdir - the sort direction (asc or desc)
#	  secsort - the secondary sort field
#	  proj_id - the project id in the database
#	  show_notes - yes/no show the notes for each record
#	  show_form - yes/no show the data entry form
#-----------------------------------------------------------------

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
	$self->{show_notes} = 1;
	$self->{show_form} = 1;
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
