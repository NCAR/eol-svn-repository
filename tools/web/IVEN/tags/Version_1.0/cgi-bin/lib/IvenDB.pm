##Module----------------------------------------------------------------------
# <p>IvenDB.pm establishes a connection to the iven database and returns 
#  a handle to the connection.
#
# @author Dan Sullivan
##Module----------------------------------------------------------------------

package IvenDB;

use DBI;

my $dbh = getConnection();

##----------------------------------------------------------------------------
# @signature db_handle getConnection()
# <p>Establishes a new connection to the database.  The IvenDB.pm module
#  calls by this function whenever the module is 'used' and stores the handle.  
#  Use the connect() function to obtain the handle.</p>
#
# @output $dbh handle to the database connection
##----------------------------------------------------------------------------
sub getConnection
{
  return DBI->connect( "DBI:mysql:database=iven;host=hurricane",
                        "iven", "data-proc", { RaiseError=>1} ) || die( "Unable to connect to database: iven" );
}

##----------------------------------------------------------------------------
# @signature db_handle connect()
# <p>Returns a handle to the database connection.
#
# @output $dbh handle to the database connection
##----------------------------------------------------------------------------
sub connect
{
	return $dbh;
}

1;

