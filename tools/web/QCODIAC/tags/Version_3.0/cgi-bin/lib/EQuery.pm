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
use DBI;

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

#	$self->{database} = $db;
#	$self->{database} = MySqlDatabase->new();
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
#   $sql - SELECT statement to execute
#   $database (optional) - name of database to query
#     Name of database must already be set if not given
# Returns:
#   the number of rows in the result
#-----------------------------------------------------------------------------#
sub query
{
	my $self = shift;

	my ($sql,$id) = @_;

#        my $database = DBI->connect("DBI:mysql:database=zedi8;host=tsunami.eol.ucar.edu",
#                                    "zediview","look-123", {RaiseError => 1}) or
	my $database = DBI->connect("DBI:mysql:database=zith9;host=sferic.eol.ucar.edu",
				    "zithview","look-999", {RaiseError => 1}) or
		    die("Unable to establish a datatbase connection.");
		
	my $sth = $database->prepare($sql);
	$sth->execute() or die("Unable to execute query: $sql\n");

	my $result_ref = $sth->fetchall_arrayref({});

	$database->disconnect();

	$self->{row_count} = scalar(@{ $result_ref});

	my @new_arr;
	my $count = 0;
	foreach my $row (@{ $result_ref}) {
		$new_arr[$count++] = $row;
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
