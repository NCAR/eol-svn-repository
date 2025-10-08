package IvenDB;

use DBI;

my $dbh = getConnection();

sub getConnection
{
  return DBI->connect( "DBI:mysql:database=iven;host=hurricane",
                        "stormdba", "codiac", { RaiseError=>1} ) || die( "Unable to connect to database: iven" );
}

sub connect
{
	return $dbh;
}

1;

