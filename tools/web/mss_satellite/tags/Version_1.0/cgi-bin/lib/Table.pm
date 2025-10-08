#!/bin/perl

package Table;

#----------------------------------------------------
#Table.pm: 
#	This module stores the records of either the DayTable
#	or FileTable of the suldan MySQL database as a doubly
#	linked list.  This was not used with the query* script
#	but was used to help populate the database originally.
#----------------------------------------------------


sub new
{
	my $class = shift;	
	my $self = {};
	bless( $self, $class );

	$self->{head} = undef;
	$self->{tail} = undef;
	$self->{count} = 0;

	return $self;
}

sub addRecord
{
	my $self = shift;
	my $rec = shift;

	if( !defined( $self->{tail} ) )
	{
		$self->{head} = $rec;
		$self->{tail} = $rec;
	}
	else
	{
		$rec->{right} = $self->{head};
		$self->{head}->{left} = $rec;
		$self->{head} = $rec;
	}

	$self->{count}++;
}

sub write
{
	my $self = shift;
	*OUT = shift;
	my $tail = $self->{tail};

	while( $tail )
	{
		$tail->write( *OUT );
		$tail = $tail->{left};
	}
}

sub showTable
{
	my $self = shift;
	*OUT = shift;
	my $col1 = shift;
	my $col2 = shift;
	if( !$col2 ) { $col2 = $col1; }
	my $col = $col2;

	my $tail = $self->{tail};

	print( OUT $tail->getTableHeader() );

	while( $tail )
	{
		my $link = "<a href=" . $my_url . "?Action=getfiles&day_id=$tail->{id} target=bottom>";

		if( $col eq $col1 )
		{ $col = $col2; } else { $col = $col1; }

		print( OUT $tail->getHtmlRow( $col, $link ) );

		$tail = $tail->{left};
	}

	print( OUT "</table>\n" );
}
1;
