#!/bin/perl

package Person;

#-----------------------------------------------------------------
# Person.pm: This module contains a simple container class that
#  holds the information needed for the Loaders, Contacts, and
#  Checkers tables for the Data Loading Notes.
#
# Subroutines:
#   new - constructs a new Person object
#
# Properties:
#   id - the id number of this record in the database
#   name - the person's name
#   email - the person's e-mail
#   active - yes/no is this person still working at JOSS 
#-----------------------------------------------------------------

# Person->new( id, name, email, active )
sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = shift;
	$self->{name} = shift;
	$self->{email} = shift;
	$self->{active} = shift;

	return $self;
}

1;
