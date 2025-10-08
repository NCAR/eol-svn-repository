#!/usr/bin/perl -w

package Row;

#----------------------------------------------------
#	Row.pm: This module contains the Row class.
#		A Row contains the fields for a given record. 
#		An array and a hash table are maintained,
#		the key of the hash table is the name of the Field.
#
#	Subroutines:
#		new - constructor, initializes a new Row
#		addField - adds a field to the Row
#		getFieldByIndex - returns the field corresponding
#			to the given index
#		getFieldByName - returns the field corresponding
#			to the given name
#
#	Properties:
#		hash - hash table of fields
#		array - array table of fields
#		count - the number of fields added
#		id - the id of this row
#		next - the pointer to the next Row in the Table's
#			linked list	
#----------------------------------------------------


use strict;

sub new
{
	my $class = shift;
	my $id = shift;
	my $self = {};
	bless( $self, $class );

	$self->{hash} = {};
	$self->{array} = [];
	$self->{count} = 0;
	$self->{id} = $id;
	$self->{next} = undef;
	return $self;
}

sub addField
{
	my $self = shift;
	my $field = shift;

	$self->{hash}{$field->{name}} = \$field;
	$self->{array}[$self->{count}] = \$field;

	$self->{count}++;
}

sub getFieldByIndex
{
	my $self = shift;
	my $index = shift;

	return $self->{array}[$index];	
}

sub getFieldByName
{
	my $self = shift;
	my $name = shift;

	return $self->{hash}{$name};
}	
1;
