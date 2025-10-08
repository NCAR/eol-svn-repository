#
# EQuery.pm
#  Author: Dan Sullivan
#  Date: 2003-06-23
#

# EQuery - Empress Query class.  Uses empcmd to query
#  Empress.

#-----------------------------------------------------------------------------#
# EQuery->new( $database ) 
#   -Creates an instance of EQuery with the given database, paramater is opt.
# query( $sql, $database ) or query( $sql )
#   -Executes the given SQL SELECT statement and populates the result array
# getRow()
#    - Returns a row of the result (returns undef() when
#       all rows have been returned)
# getAllData 
#    - Returns all of the data as an array of references to hashes 
# getResultMetadata()
#    - Returns the column names in an array
# getRowCount() 
#    - Returns the number of rows in the result 
# getColCount() 
#    - Returns the number of columns in the result 
# getSQL() 
#    - Returns the sql statement sent to the query() sub
# getDatabaes() 
#    - Returns the db to query
# setDatabaes( $database ) 
#    - Set the db to query
# quote( $str ) 
#    - escapes all ' characters and surrounds the string with quotes
#
# See below for more detailed descriptions of each subroutine.
#-----------------------------------------------------------------------------#

package EQuery;
use lib ".";

my $db_path = "/storm/codiac/codiac_db";
my $empcmd = "/usr/empress/bin/empcmd";
$ENV{EMPRESSPATH} = "/usr/empress";
$ENV{MSQLSELCOLSEP} = "";
$ENV{MSQLSELROWSEP} = "";
$ENV{MSQLSELROWCOLCROSS} = "";

#-----------------------------------------------------------------------------#
# EQuery->new( $database ) 
#
# Create an instance of EQuery 
#
# Arguments:
#   $database (optional) - name of database to query.
# Returns:
#   new instance of EQuery, itself 
#-----------------------------------------------------------------------------#
sub new
{
	my $class = shift;
	my $db = shift;

	my $self = {};
	bless( $self, $class );

	$self->{database} = $db;
	$self->{resultmeta} = undef();
	$self->{result} = undef();	
	$self->{row} = -1;
	$self->{sql} = "";
	$self->{row_count} = -1;
	$self->{col_count} = -1;

	return $self;
}

#-----------------------------------------------------------------------------#
# query( $sql, $database ) or query( $sql )
#
# Executes the given query and stores the result
#
# Arguments:
#   $sql - SELECT statemet to execute
#   $database (optional) - name of database to query
#     Name of database must already be set if not given
# Returns:
#   the number of rows in the result
#-----------------------------------------------------------------------------#
sub query
{
	my $self = shift;

	my $sql = shift;
	my $database = shift;

	if( defined( $database ) )
	{ $self->{database} = $database; }
	else
	{ $database = $self->{database}; }

	# Make sure the user passed an sql statement
	die( "Error: EQuery::query( \$sql, \$database ): No SQL given\n" ) if( !defined( $sql ) );

	# Make sure user specified the database to query
	die( "Error: EQuery::query( \$sql, \$database ): Must specify database\n" ) if( !defined( $database ) );

	$self->{sql} = $sql;

	# Execute the query
	my $result = `$empcmd $db_path/$database \"$sql\"`; 
	my @result = split( /+/, $result );

	$self->{row_count} = @result;

	# Make sure the query did not fail
	die( "Error: EQuery::query( \$sql, \$database ): SQL Error\n" ) if( $self->{row_count} == 0 );

	# Store the metadata (column names) get from the top of the first record
	my @tmp = split( "\n", $result[0] );
	my @meta = split( "", $tmp[0] );
	$self->{col_count} = @meta;

	for( my $x = 0; $x < $self->{col_count}; $x++ )
	{
		$meta[$x] = trim( $meta[$x] );
	}

	$self->{resultmeta} = \@meta;

	# Get rid of the meta data
	my @tmp2;
	for( my $x = 2; $x < @tmp; $x++ )
	{
		$tmp2[$x-2] = $tmp[$x];	
	}	

	if( scalar(@tmp) <= 2 )
	{
		$self->{row_count} = 0;
		@result = [];
	}
	else
	{
		$result[0] = join( "\n", @tmp2 );
	}
	
	# Store the result, creates the array of hash table references	
	my @new_arr;
	my $count = 0;
	foreach my $row (@result)
	{
		my @record = split( "\n", $row );
		my %hash;

		# Init the row
		for( my $y = 0; $y < $self->{col_count}; $y++ )
		{
			$hash{$meta[$y]} = "";
		}

		# Store each line of the record, some fields can have
		#  have 3-d character space, empcmd multiple lines for these
		#  fields, this takes care of that.  Most of the time this
		#  loop will only iterate once
		foreach my $rec (@record)
		{
			my @line = split( "", $rec );
			for( my $y = 0; $y < $self->{col_count}; $y++ )
			{
				$hash{$meta[$y]} = $hash{$meta[$y]} . trim( $line[$y] ) if( defined( $line[$y] ) );
			}
		}

		$new_arr[$count++] = \%hash;
	}

	$self->{result} = \@new_arr;
	$self->{row} = 0;
	
	return $self->{row_count};	
}

#-----------------------------------------------------------------------------#
# getSQL() - returns the sql statement sent to the query() sub
#-----------------------------------------------------------------------------#
sub getSQL
{
	my $self = shift;
	return $self->{sql};
}

#-----------------------------------------------------------------------------#
# setDatabaes( $database ) - set the db to query
#-----------------------------------------------------------------------------#
sub setDatabase
{
	my $self = shift;
	my $db = shift;
	$self->{database} = $db;
}

#-----------------------------------------------------------------------------#
# getDatabaes() - returns the db to query
#-----------------------------------------------------------------------------#
sub getDatabase
{
	my $self = shift;
	return $self->{database};
}

#-----------------------------------------------------------------------------#
# getResultMetadata()
#   Returns an array of column names from the most recent
#    call to query, returns undef if no query has been made.
#-----------------------------------------------------------------------------#
sub getResultMetadata
{
	my $self = shift;

	return @{ $self->{resultmeta} } if( defined( $self->{resultmeta} ) );

	return;
}

#-----------------------------------------------------------------------------#
# getRowCount() - returns the number of rows in the result of
#  the most recent call to query().
#-----------------------------------------------------------------------------#
sub getRowCount
{
	my $self = shift;
	return $self->{row_count};
}

#-----------------------------------------------------------------------------#
# getColCount() - returns the number of columns in the result of
#  the most recent call to query().
#-----------------------------------------------------------------------------#
sub getColCount
{
	my $self = shift;
	return $self->{col_count};
}

#-----------------------------------------------------------------------------#
# getRow()
#   Returns a row (a hash with the column names as the key)
#    from the most recent query.  A counter is incremented
#    each time this sub is called, when all rows have been
#    returned undef() is returned.
#  Could use this as such:
#    while( (my %hash = $q->getRow() ) )
#    {
#      ...stuff here....
#    }
#-----------------------------------------------------------------------------#
sub getRow
{
	my $self = shift;

	if( $self->{row} < $self->{row_count} )
	{
		return %{ $self->{result}->[$self->{row}++] } ;
	}
	else
	{
		return;
	}
}

#-----------------------------------------------------------------------------#
# getAllData - Returns all of the data as an array of 
#   hash references.
#  Example:
#   my @arr = $q->getAllData();
#   foreach my $r (@arr)
#   {
#     my %h = %$r;
#     ...stuff here...
#   }
#
#   OR
#
#   $arr[0]->{storm_id}
#   $arr[0]->{summary}  etc.
# 
#-----------------------------------------------------------------------------#
sub getAllData
{
	my $self = shift;

	return @{ $self->{result} } if( defined( $self->{result} ) ); 

	return;
}

#-----------------------------------------------------------------------------#
# quote( $str ) - escapes all ' characters and surrounds the
#   string in the needed quotes.  Godd for SQL statements
#
#  my $sql = "SELECT storm_id FROM dataset WHERE title=" . 
#            $q->quote( $title );
#
#-----------------------------------------------------------------------------#
sub quote
{
	my $self = shift;
	my $str = shift;
	my $qt = "'";

	$str =~ s/$qt/\\$qt/g;

	return "$qt$str$qt";
}

# Trim the spaces at the beginning and end of
#  the string
sub trim
{
	my $str = shift;
	$str =~ s/^\s+//;
	$str =~ s/\s+$//;
	return $str;
}
1;
